%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_commit_gen.m.
% Main author: fjh.
%
% This module handles code generation for commits.
%
% There are several different ways of handling commits:
%   - using catch/throw
%   - using setjmp/longjmp
%   - using GCC's __builtin_setjmp/__builtin_longjmp
%   - exiting nested functions via gotos to their containing functions
%
% The MLDS data structure abstracts away these differences using the
% `try_commit' and `do_commit' instructions. The comments below show
% the MLDS try_commit/do_commit version first, but for clarity I've also
% included sample code using each of the three different techniques.
% This shows how the MLDS->target back-end can map mlds_commit_type,
% do_commit and try_commit into target language constructs.
%
% Note that if we are using GCC's __builtin_longjmp(), then it is important
% that the call to __builtin_longjmp() be put in its own function, to ensure
% that it is not in the same function as the __builtin_setjmp(). The code
% generation schema below does that automatically. We will need to be careful
% with MLDS optimizations to ensure that we preserve that invariant, though.
% (Alternatively, we could just call a function that calls __builtin_longjmp()
% rather than calling it directly. But that would be a little less efficient.)
%
% If those methods turn out to be too inefficient, another alternative would be
% to change the generated code so that after every function call, it would
% check a flag, and if that flag was set, it would return. Then MR_DO_COMMIT
% would just set the flag and return. The flag could be in a global
% (or thread-local) variable, or it could be an additional value returned
% from each function.
%
%   model_non in semi context: (using try_commit/do_commit)
%       <succeeded = Goal>
%   ===>
%       MR_COMMIT_TYPE ref;
%       void success() {
%           MR_DO_COMMIT(ref);
%       }
%       MR_TRY_COMMIT(ref, {
%           <Goal && success()>
%           succeeded = MR_FALSE;
%       }, {
%           succeeded = MR_TRUE;
%       })
%
%   model_non in semi context: (using catch/throw)
%       <succeeded = Goal>
%   ===>
%       void success() {
%           throw COMMIT();
%       }
%       try {
%           <Goal && success()>
%           succeeded = MR_FALSE;
%       } catch (COMMIT) {
%           succeeded = MR_TRUE;
%       }
%
% The above is using C++ syntax. Here COMMIT is an exception type, which
% can be defined trivially (e.g. "class COMMIT {};"). Note that when using
% catch/throw, we don't need the "ref" argument at all; the target language's
% exception handling implementation keeps track of all the information needed
% to unwind the stack.
%
%   model_non in semi context: (using setjmp/longjmp)
%       <succeeded = Goal>
%   ===>
%       jmp_buf ref;
%       void success() {
%           longjmp(ref, 1);
%       }
%       if (setjmp(ref)) {
%           succeeded = MR_TRUE;
%       } else {
%           <Goal && success()>
%           succeeded = MR_FALSE;
%       }
%
%   model_non in semi context: (using GNU C nested functions,
%               GNU C local labels, and exiting
%               the nested function by a goto
%               to a label in the containing function)
%       <succeeded = Goal>
%   ===>
%       __label__ commit;
%       void success() {
%           goto commit;
%       }
%       <Goal && success()>
%       succeeded = MR_FALSE;
%       goto commit_done;
%   commit:
%       succeeded = MR_TRUE;
%   commit_done:
%       ;
%
%   model_non in det context: (using try_commit/do_commit)
%       <do Goal>
%   ===>
%       MR_COMMIT_TYPE ref;
%       void success() {
%           MR_DO_COMMIT(ref);
%       }
%       MR_TRY_COMMIT(ref, {
%           <Goal && success()>
%       }, {})
%
%   model_non in det context (using GNU C nested functions,
%               GNU C local labels, and exiting
%               the nested function by a goto
%               to a label in the containing function)
%       <do Goal>
%   ===>
%       __label__ done;
%       void success() {
%           goto done;
%       }
%       <Goal && success()>
%   done:   ;
%
%   model_non in det context (using catch/throw):
%       <do Goal>
%   ===>
%       void success() {
%           throw COMMIT();
%       }
%       try {
%           <Goal && success()>
%       } catch (COMMIT) {}
%
%   model_non in det context (using setjmp/longjmp):
%       <do Goal>
%   ===>
%       jmp_buf ref;
%       void success() {
%           longjmp(ref, 1);
%       }
%       if (setjmp(ref) == 0) {
%           <Goal && success()>
%       }
%
% Note that for all of these versions, we must hoist any static declarations
% generated for <Goal> out to the top level; this is needed so that such
% declarations remain in scope for any following goals.

:- module ml_backend.ml_commit_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

    % Generate code for a commit.
    %
:- pred ml_gen_commit(hlds_goal::in, code_model::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_accurate_gc.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module string.
:- import_module set.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

ml_gen_commit(Goal, CodeModel, Context, Decls, Statements, !Info) :-
    Goal = hlds_goal(_, GoalInfo),
    GoalCodeModel = goal_info_get_code_model(GoalInfo),
    GoalContext = goal_info_get_context(GoalInfo),

    ( if
        GoalCodeModel = model_non,
        CodeModel = model_semi
    then

        %   model_non in semi context: (using try_commit/do_commit)
        %       <succeeded = Goal>
        %   ===>
        %       MR_bool succeeded;
        %   #ifdef NONDET_COPY_OUT
        %       <local var decls>
        %   #endif
        %   #ifdef PUT_COMMIT_IN_OWN_FUNC
        %       /*
        %       ** to avoid problems with setjmp() and non-volatile
        %       ** local variables, we need to put the call to
        %       ** setjmp() in its own nested function
        %       */
        %       void commit_func()
        %       {
        %   #endif
        %       MR_COMMIT_TYPE ref;
        %
        %       void success() {
        %           MR_DO_COMMIT(ref);
        %       }
        %
        %       MR_TRY_COMMIT(ref, {
        %           <Goal && success()>
        %           succeeded = MR_FALSE;
        %       }, {
        %   #ifdef NONDET_COPY_OUT
        %           <copy local vars to output args>
        %   #endif
        %           succeeded = MR_TRUE;
        %       })
        %   #ifdef PUT_COMMIT_IN_OWN_FUNC
        %
        %       commit_func();
        %   #endif

        ml_gen_maybe_make_locals_for_output_args(GoalInfo, LocalVarDecls,
            CopyLocalsToOutputArgs, OrigVarLvalMap, !Info),

        % Generate the `success()' function.
        ml_gen_new_func_label(no, SuccessFuncLabel, SuccessFuncLabelRval,
            !Info),
        % push nesting level
        MLDS_Context = mlds_make_context(Context),
        ml_gen_info_new_aux_var_name("commit", CommitRef, !Info),
        ml_gen_var_lval(!.Info, CommitRef, mlds_commit_type, CommitRefLval),
        CommitRefDecl = ml_gen_commit_var_decl(MLDS_Context, CommitRef),
        DoCommitStmt = ml_stmt_do_commit(ml_lval(CommitRefLval)),
        DoCommitStatement = statement(DoCommitStmt, MLDS_Context),
        % Pop nesting level.
        ml_gen_nondet_label_func(!.Info, SuccessFuncLabel, Context,
            DoCommitStatement, SuccessFunc),

        ml_get_env_ptr(!.Info, EnvPtrRval),
        SuccessCont = success_cont(SuccessFuncLabelRval, EnvPtrRval, [], []),
        ml_gen_info_push_success_cont(SuccessCont, !Info),
        ml_gen_goal(model_non, Goal, GoalDecls, GoalStatements, !Info),
        GoalStatement = ml_gen_block(GoalDecls, GoalStatements, GoalContext),
        ml_gen_info_pop_success_cont(!Info),
        ml_gen_set_success(!.Info, ml_const(mlconst_false), Context,
            SetSuccessFalse),
        ml_gen_set_success(!.Info, ml_const(mlconst_true), Context,
            SetSuccessTrue),
        TryCommitStmt = ml_stmt_try_commit(CommitRefLval,
            ml_gen_block([], [GoalStatement, SetSuccessFalse], Context),
            ml_gen_block([], CopyLocalsToOutputArgs ++ [SetSuccessTrue],
                Context)
        ),
        TryCommitStatement = statement(TryCommitStmt, MLDS_Context),
        CommitFuncLocalDecls = [CommitRefDecl, SuccessFunc],
        maybe_put_commit_in_own_func(CommitFuncLocalDecls,
            [TryCommitStatement], Context, CommitFuncDecls, Statements, !Info),
        Decls = LocalVarDecls ++ CommitFuncDecls,

        ml_gen_info_set_var_lvals(OrigVarLvalMap, !Info)
    else if
        GoalCodeModel = model_non,
        CodeModel = model_det
    then
        %   model_non in det context: (using try_commit/do_commit)
        %       <do Goal>
        %   ===>
        %   #ifdef NONDET_COPY_OUT
        %       <local var decls>
        %   #endif
        %   #ifdef PUT_COMMIT_IN_NESTED_FUNC
        %       /*
        %       ** to avoid problems with setjmp() and non-volatile
        %       ** local variables, we need to put the call to
        %       ** setjmp() in its own nested functions
        %       */
        %       void commit_func()
        %       {
        %       #endif
        %       MR_COMMIT_TYPE ref;
        %       void success() {
        %           MR_DO_COMMIT(ref);
        %       }
        %       MR_TRY_COMMIT(ref, {
        %           <Goal && success()>
        %       }, {
        %   #ifdef NONDET_COPY_OUT
        %           <copy local vars to output args>
        %   #endif
        %       })
        %   #ifdef PUT_COMMIT_IN_NESTED_FUNC
        %
        %       commit_func();
        %       #endif

        ml_gen_maybe_make_locals_for_output_args(GoalInfo, LocalVarDecls,
            CopyLocalsToOutputArgs, OrigVarLvalMap, !Info),

        % Generate the `success()' function.
        ml_gen_new_func_label(no, SuccessFuncLabel, SuccessFuncLabelRval,
            !Info),
        % push nesting level
        MLDS_Context = mlds_make_context(Context),
        ml_gen_info_new_aux_var_name("commit", CommitRef, !Info),
        ml_gen_var_lval(!.Info, CommitRef, mlds_commit_type, CommitRefLval),
        CommitRefDecl = ml_gen_commit_var_decl(MLDS_Context, CommitRef),
        DoCommitStmt = ml_stmt_do_commit(ml_lval(CommitRefLval)),
        DoCommitStatement = statement(DoCommitStmt, MLDS_Context),
        % pop nesting level
        ml_gen_nondet_label_func(!.Info, SuccessFuncLabel, Context,
            DoCommitStatement, SuccessFunc),

        ml_get_env_ptr(!.Info, EnvPtrRval),
        SuccessCont = success_cont(SuccessFuncLabelRval, EnvPtrRval, [], []),
        ml_gen_info_push_success_cont(SuccessCont, !Info),
        ml_gen_goal(model_non, Goal, GoalDecls, GoalStatements, !Info),
        % Hoist any static constant declarations for Goal out to the top level.
        GoalStatement = ml_gen_block(GoalDecls, GoalStatements, GoalContext),
        ml_gen_info_pop_success_cont(!Info),

        TryCommitStmt = ml_stmt_try_commit(CommitRefLval, GoalStatement,
            ml_gen_block([], CopyLocalsToOutputArgs, Context)),
        TryCommitStatement = statement(TryCommitStmt, MLDS_Context),
        CommitFuncLocalDecls = [CommitRefDecl, SuccessFunc],
        maybe_put_commit_in_own_func(CommitFuncLocalDecls,
            [TryCommitStatement], Context, CommitFuncDecls, Statements, !Info),
        Decls = LocalVarDecls ++ CommitFuncDecls,
        ml_gen_info_set_var_lvals(OrigVarLvalMap, !Info)
    else
        % No commit required.
        ml_gen_goal(CodeModel, Goal, Decls, Statements, !Info)
    ).

    % maybe_put_commit_in_own_func(Defns0, Stmts0, Defns, Stmts):
    %
    % If the --put-commit-in-own-func option is set, put the commit in its
    % own function. This is needed for the high-level C back-end, to handle
    % problems with setjmp()/longjmp() clobbering non-volatile local variables.
    %
    % Detailed explanation:
    %
    % For the high-level C back-end, we implement commits using
    % setjmp()/longjmp(). Unfortunately for us, ANSI/ISO C says that longjmp()
    % is allowed to clobber the values of any non-volatile local variables
    % in the function that called setjmp() which have been modified between
    % the setjmp() and the longjmp().
    %
    % To avoid this, whenever we generate a commit, we put it in its own
    % nested function, with the local variables (e.g. `succeeded', plus any
    % outputs from the goal that we are committing over) remaining in the
    % containing function. This ensures that none of the variables which
    % get modified between the setjmp() and the longjmp() and which get
    % referenced after the longjmp() are local variables in the function
    % containing the setjmp().
    %
    % [The obvious alternative of declaring the local variables in the function
    % containing setjmp() as `volatile' doesn't work, since the assignments
    % to those output variables may be deep in some function called indirectly
    % from the goal that we are committing across, and assigning to a
    % volatile-qualified variable via a non-volatile pointer is undefined
    % behaviour. The only way to make it work would be to be to declare
    % *every* output argument that we pass by reference as `volatile T *'.
    % But that would impose distributed fat and would make interoperability
    % difficult.]
    %
:- pred maybe_put_commit_in_own_func(list(mlds_defn)::in, list(statement)::in,
    prog_context::in, list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

maybe_put_commit_in_own_func(CommitFuncLocalDecls, TryCommitStatements,
        Context, Decls, Statements, !Info) :-
    ml_gen_info_put_commit_in_own_func(!.Info, PutCommitInOwnFunc),
    (
        PutCommitInOwnFunc = yes,

        % Generate the `void commit_func() { ... }' wrapper
        % around the main body that we generated above
        ml_gen_new_func_label(no, CommitFuncLabel, CommitFuncLabelRval, !Info),
        % push nesting level
        CommitFuncBody = ml_gen_block(CommitFuncLocalDecls,
            TryCommitStatements, Context),
        % pop nesting level
        ml_gen_nondet_label_func(!.Info, CommitFuncLabel, Context,
            CommitFuncBody, CommitFunc),

        % Generate the call to `commit_func();'
        ml_gen_info_use_gcc_nested_functions(!.Info, UseNestedFuncs),
        (
            UseNestedFuncs = yes,
            ArgRvals = [],
            ArgTypes = []
        ;
            UseNestedFuncs = no,
            ml_get_env_ptr(!.Info, EnvPtrRval),
            ArgRvals = [EnvPtrRval],
            ArgTypes = [mlds_generic_env_ptr_type]
        ),
        RetTypes = [],
        Signature = mlds_func_signature(ArgTypes, RetTypes),
        CallKind = ordinary_call,
        set.init(Markers),
        CallStmt = ml_stmt_call(Signature, CommitFuncLabelRval, no, ArgRvals,
            [], CallKind, Markers),
        CallStatement = statement(CallStmt, mlds_make_context(Context)),
        % Package it all up.
        Statements = [CallStatement],
        Decls = [CommitFunc]
    ;
        PutCommitInOwnFunc = no,
        Statements = TryCommitStatements,
        Decls = CommitFuncLocalDecls
    ).

    % In commits, you have model_non code called from a model_det or model_semi
    % context. With --nondet-copy-out, when generating code for commits,
    % if the context is a model_det or model_semi procedure with output
    % arguments passed by reference, then we need to introduce local variables
    % corresponding to those output arguments, and at the end of the commit
    % we will copy the local variables into the output arguments.
    %
:- pred ml_gen_maybe_make_locals_for_output_args(hlds_goal_info::in,
    list(mlds_defn)::out, list(statement)::out,
    map(prog_var, mlds_lval)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_maybe_make_locals_for_output_args(GoalInfo, LocalVarDecls,
        CopyLocalsToOutputArgs, OrigVarLvalMap, !Info) :-
    ml_gen_info_get_var_lvals(!.Info, OrigVarLvalMap),
    ml_gen_info_get_globals(!.Info, Globals),
    globals.lookup_bool_option(Globals, nondet_copy_out, NondetCopyOut),
    (
        NondetCopyOut = yes,
        Context = goal_info_get_context(GoalInfo),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        ml_gen_info_get_byref_output_vars(!.Info, ByRefOutputVars),
        VarsToCopy = set_of_var.intersect(
            set_of_var.list_to_set(ByRefOutputVars), NonLocals),
        ml_gen_make_locals_for_output_args(
            set_of_var.to_sorted_list(VarsToCopy),
            Context, LocalVarDecls, CopyLocalsToOutputArgs, !Info)
    ;
        NondetCopyOut = no,
        LocalVarDecls = [],
        CopyLocalsToOutputArgs = []
    ).

:- pred ml_gen_make_locals_for_output_args(list(prog_var)::in,
    prog_context::in, list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_make_locals_for_output_args([], _, [], [], !Info).
ml_gen_make_locals_for_output_args([Var | Vars], Context,
        LocalDefns, Assigns, !Info) :-
    ml_gen_make_locals_for_output_args(Vars, Context, LocalDefns0, Assigns0,
        !Info),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_variable_type(!.Info, Var, Type),
    IsDummy = check_dummy_type(ModuleInfo, Type),
    (
        IsDummy = is_dummy_type,
        LocalDefns = LocalDefns0,
        Assigns = Assigns0
    ;
        IsDummy = is_not_dummy_type,
        ml_gen_make_local_for_output_arg(Var, Type, Context,
            LocalDefn, Assign, !Info),
        LocalDefns = [LocalDefn | LocalDefns0],
        Assigns = [Assign | Assigns0]
    ).

:- pred ml_gen_make_local_for_output_arg(prog_var::in, mer_type::in,
    prog_context::in, mlds_defn::out, statement::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_make_local_for_output_arg(OutputVar, Type, Context,
        LocalVarDefn, Assign, !Info) :-
    % Look up the name of the output variable.
    ml_gen_info_get_varset(!.Info, VarSet),
    OutputVarName = ml_gen_var_name(VarSet, OutputVar),

    % Generate a declaration for a corresponding local variable.
    OutputVarName = mlds_var_name(OutputVarNameStr, MaybeNum),
    LocalVarName = mlds_var_name(
        string.append("local_", OutputVarNameStr), MaybeNum),
    ml_gen_type(!.Info, Type, MLDS_Type),
    ml_gen_gc_statement(LocalVarName, Type, Context, GCStatement,
        !Info),
    LocalVarDefn = ml_gen_mlds_var_decl(mlds_data_var(LocalVarName), MLDS_Type,
        GCStatement, mlds_make_context(Context)),

    % Generate code to assign from the local var to the output var.
    ml_gen_var(!.Info, OutputVar, OutputVarLval),
    ml_gen_var_lval(!.Info, LocalVarName, MLDS_Type, LocalVarLval),
    Assign = ml_gen_assign(OutputVarLval, ml_lval(LocalVarLval), Context),

    % Update the lval for this variable so that any references to it inside
    % the commit refer to the local variable rather than to the output
    % argument. (Note that we reset all the var lvals at the end of the
    % commit.)
    ml_gen_info_set_var_lval(OutputVar, LocalVarLval, !Info).

    % Generate the declaration for the `commit' variable.
    %
:- func ml_gen_commit_var_decl(mlds_context, mlds_var_name) = mlds_defn.

ml_gen_commit_var_decl(Context, VarName) =
    ml_gen_mlds_var_decl(mlds_data_var(VarName), mlds_commit_type, gc_no_stmt,
        Context).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_commit_gen.
%-----------------------------------------------------------------------------%
