%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: declarative_execution.m.
% Author: Mark Brown.
% 
% This module defines a Mercury representation of Mercury program execution,
% the annotated trace.  This structure is described in papers/decl_debug.  The
% declarative debugging infrastructure in the trace directory builds an
% annotated trace, using predicates exported from this module.  Once built,
% the structure is passed to the front end (in browser/declarative_debugger.m)
% where it is analysed to produce a bug diagnosis.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module mdb.declarative_execution.
:- interface.

:- import_module mdb.term_rep.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.rtti_access.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % This type represents a port in the annotated trace.
    % The type R is the type of references to other nodes in the store.
    %
    % If this type is modified, the procedures below which
    % do destructive update on values of this type may also
    % need to be modified.
    %
:- type trace_node(R)
    --->    node_call(
                call_preceding      :: R,
                                    % Preceding event.

                call_last_interface :: R,
                                    % Last EXIT, REDO, FAIL or
                                    % EXCP event.

                call_atom_args      :: list(trace_atom_arg),
                                    % Atom that was called.

                call_seq            :: sequence_number,
                                    % Call sequence number.

                call_event          :: event_number,
                                    % Trace event number.

                call_at_max_depth   :: maybe(implicit_tree_info),
                                    % Yes if the node is the root of an
                                    % implicitly represented tree. Some
                                    % information about the implicit tree
                                    % is also stored.

                call_return_label   :: maybe(label_layout),
                                    % The return label, if there is one.

                call_label          :: label_layout,

                call_io_seq_num     :: int,
                                    % The I/O action sequence number at the
                                    % time of the call.

                call_suspicion      :: suspicion_accumulator
                                    % The value of the suspicion
                                    % accumulator at the time of
                                    % the call.
            )

    ;       node_exit(
                exit_preceding      :: R,
                                    % Preceding event.

                exit_call           :: R,
                                    % CALL event.

                exit_prev_redo      :: R,
                                    % Previous REDO event, if any.

                exit_atom_args      :: list(trace_atom_arg),
                                    % Atom in its final state.

                exit_event          :: event_number,
                                    % Trace event number.

                exit_label          :: label_layout,

                exit_io_seq_num     :: int,
                                    % The I/O action sequence number at the
                                    % time of the exit.

                exit_suspicion      :: suspicion_accumulator
                                    % The value of the suspicion accumulator
                                    % at the time of the exit.
            )

    ;       node_redo(
                redo_preceding      :: R,
                                    % Preceding event.

                redo_exit           :: R,
                                    % EXIT event.

                redo_event          :: event_number,
                                    % REDO event number.

                redo_label          :: label_layout,
                redo_suspicion      :: suspicion_accumulator
                                    % The value of the suspicion accumulator
                                    % at the time of the redo.
            )

    ;       node_fail(
                fail_preceding      :: R,
                                    % Preceding event.

                fail_call           :: R,
                                    % CALL event.

                fail_redo           :: R,
                                    % Previous REDO event, if any.

                fail_event          :: event_number,
                                    % Trace event number.

                fail_label          :: label_layout,
                fail_suspicion      :: suspicion_accumulator
                                    % The value of the suspicion accumulator
                                    % at the time of the fail.
            )

    ;       node_excp(
                excp_preceding      :: R,
                                    % Preceding event.

                excp_call           :: R, % Call event.

                excp_redo           :: R,
                                    % Previous redo, if any.

                excp_value          :: term_rep,
                                    % Exception thrown.

                excp_event          :: event_number,
                                    % Trace event number.

                excp_label          :: label_layout,

                excp_suspicion      :: suspicion_accumulator
                                    % The value of the suspicion accumulator
                                    % at the time of the excp.
            )

    ;       node_switch(
                switch_preceding    :: R,
                                    % Preceding event.

                switch_label        :: label_layout
            )

    ;       node_first_disj(
                first_disj_preceding:: R,
                                    % Preceding event.

                first_disj_label    :: label_layout
            )

    ;       node_later_disj(
                later_disj_preceding:: R,
                                    % Preceding event.

                later_disj_label    :: label_layout,

                later_disj_first    :: R
                                    % Event of the first DISJ.
            )

    ;       node_cond(
                cond_preceding      :: R,
                                    % Preceding event.

                cond_label          :: label_layout,

                cond_status         :: goal_status
                                    % Whether we have reached a THEN or ELSE
                                    % event.
            )

    ;       node_then(
                then_preceding      :: R,
                                    % Preceding event.

                then_cond           :: R,
                                    % COND event.

                then_label          :: label_layout
            )

    ;       node_else(
                else_preceding      :: R,
                                    % Preceding event.

                else_cond           :: R,
                                    % COND event.

                else_label          :: label_layout
            )

    ;       node_neg(
                neg_preceding       :: R,
                                    % Preceding event.
                neg_label           :: label_layout,
                                    % Path for this event.
                neg_status          :: goal_status
                                    % Whether we have reached
                                    % a NEGS or NEGF event.
            )

    ;       node_neg_succ(
                neg_succ_preceding  :: R,
                                    % Preceding event.

                neg_succ_enter      :: R,
                                    % NEGE event.
                neg_succ_label      :: label_layout
            )

    ;       node_neg_fail(
                neg_fail_preceding  :: R,
                                    % Preceding event.

                neg_fail_enter      :: R,
                                    % NEGE event.

                neg_fail_label      :: label_layout
            ).

:- type trace_atom_arg
    --->    arg_info(
                prog_visible            :: bool,
                prog_vis_headvar_num    :: int,
                                        % N, if this is the Nth
                                        % programmer visible headvar
                                        % (as opposed to a variable
                                        % created by the compiler).
                arg_value               :: maybe(term_rep)
            ).

:- type trace_atom
    --->    atom(
                proc_layout         :: proc_layout,
                                    % Info about the
                                    % procedure like its name and module
                                    % and whether it is a function or a
                                    % predicate.

                atom_args           :: list(trace_atom_arg)
                                    % The arguments, including the
                                    % compiler-generated ones.
                                    % XXX This representation can't handle
                                    % partially instantiated data structures.
            ).

:- type implicit_tree_info
    --->    implicit_tree_info(
                % The maximum depth to which the implicit subtree
                % can be materialized, so that its weight will be less than
                % or equal to the desired subtree weight.
                ideal_depth :: int
            ).

:- func get_trace_exit_atom(trace_node(R)) = trace_atom.
:- mode get_trace_exit_atom(in(trace_node_exit)) = out is det.
:- mode get_trace_exit_atom(in) = out is semidet.

:- func get_trace_call_atom(trace_node(R)) = trace_atom.
:- mode get_trace_call_atom(in(trace_node_call)) = out is det.
:- mode get_trace_call_atom(in) = out is semidet.

    % get_pred_attributes(ProcLabel, Module, Name, Arity, PredOrFunc).
    % Return the predicate/function attributes common to both UCI and
    % regular predicates/functions.
    %
:- pred get_pred_attributes(proc_label::in, module_name::out, string::out,
    int::out, pred_or_func::out) is det.

:- pred call_node_maybe_proc_rep(trace_node(R)::in(trace_node_call),
    maybe(proc_rep)::out) is det.

%-----------------------------------------------------------------------------%

    % If the following type is modified, some of the macros in
    % trace/mercury_trace_declarative.h may need to be updated.
    %
:- type goal_status
    --->    succeeded
    ;       failed
    ;       undecided.

:- type sequence_number == int.
:- type event_number == int.

:- type suspicion_accumulator == int.

    % Members of this typeclass represent an entire annotated
    % trace.  The second parameter is the type of references
    % to trace nodes, and the first parameter is the type of
    % a "store": an abstract mapping from references to the
    % nodes they refer to.
    %
    % By convention, we use the names S and R for type variables
    % which are constrained by annotated_trace.  We also use
    % these names in type declarations where it is *intended* that
    % the type variables be constrained by annotated_trace.
    %
    % (Compare with the similar conventions for mercury_edt/2.)
    %
:- typeclass annotated_trace(S, R) where [

        % Dereference the identifier.  This fails if the
        % identifier does not refer to any trace_node (ie.
        % it is a NULL pointer).
        %
    pred trace_node_from_id(S::in, R::in, trace_node(R)::out) is semidet
].

    % Given any node in an annotated trace, find the most recent
    % node in the same contour (ie. the last node which has not been
    % backtracked over, skipping negations, failed conditions, the
    % bodies of calls, and alternative disjuncts).  Throw an exception
    % if there is no such node (ie. if we are at the start of a
    % negation, call, or failed condition).
    %
    % In some cases the contour may reach a dead end.  This can
    % happen if, for example, a DISJ node is not present because
    % it is beyond the depth bound or in a module that is not traced;
    % "stepping left" will arrive at a FAIL, REDO or NEGF node.  Since
    % it is not possible to follow the original contour in these
    % circumstances, we follow the previous contour instead.
    %
:- func step_left_in_contour(S, trace_node(R)) = R <= annotated_trace(S, R).

    % Given any node in an annotated trace, find the most recent
    % node in the same stratum (ie. the most recent node, skipping
    % negations, failed conditions, and the bodies of calls).
    % Throw an exception if there is no such node (ie. if we are at
    % the start of a negation, call, or failed negation).
    %
:- func step_in_stratum(S, trace_node(R)) = R <= annotated_trace(S, R).

    % The following procedures also dereference the identifiers,
    % but they give an error if the node is not of the expected type.
    %
:- pred det_trace_node_from_id(S::in, R::in, trace_node(R)::out) is det
    <= annotated_trace(S, R).

:- inst trace_node_call ---> node_call(ground, ground, ground, ground, ground,
    ground, ground, ground, ground, ground).

:- pred call_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_call))
    is det <= annotated_trace(S, R).

:- inst trace_node_redo ---> node_redo(ground, ground, ground, ground, ground).

    % maybe_redo_node_from_id/3 fails if the argument is a
    % NULL reference.
    %
:- pred maybe_redo_node_from_id(S::in, R::in,
    trace_node(R)::out(trace_node_redo)) is semidet
    <= annotated_trace(S, R).

:- inst trace_node_exit ---> node_exit(ground, ground, ground, ground,
    ground, ground, ground, ground).

:- pred exit_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_exit))
    is det <= annotated_trace(S, R).

:- inst trace_node_cond ---> node_cond(ground, ground, ground).

:- pred cond_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_cond))
    is det <= annotated_trace(S, R).

:- inst trace_node_neg ---> node_neg(ground, ground, ground).

:- pred neg_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_neg))
    is det <= annotated_trace(S, R).

:- inst trace_node_first_disj ---> node_first_disj(ground, ground).

:- pred first_disj_node_from_id(S::in, R::in,
    trace_node(R)::out(trace_node_first_disj)) is det
    <= annotated_trace(S, R).

:- inst trace_node_disj
    --->    node_first_disj(ground, ground)
    ;       node_later_disj(ground, ground, ground).

:- pred disj_node_from_id(S::in, R::in, trace_node(R)::out(trace_node_disj))
    is det <= annotated_trace(S, R).

    % Load an execution tree which was previously saved by
    % the back end.
    %
:- pred load_trace_node_map(io.input_stream::in, trace_node_map::out,
    trace_node_key::out, io::di, io::uo) is det.

    % Save an execution tree generated by the back end.  It is
    % first converted into a trace_node_map/trace_node_key pair.
    %
:- pred save_trace_node_store(io.output_stream::in, trace_node_store::in,
    trace_node_id::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % This instance is used when the declarative debugger is in
    % normal mode.  Values of this instance are produced by the
    % back end and passed directly to the front end.
    %
:- type trace_node_store.
:- type trace_node_id.
:- instance annotated_trace(trace_node_store, trace_node_id).

    % This instance is used when the declarative debugger is in
    % test mode.  Values of this instance are produced by copying
    % values of the previous instance.  Unlike the previous
    % instance, values of this one can be fed through a stream.
    %
:- type trace_node_map.
:- type trace_node_key.
:- instance annotated_trace(trace_node_map, trace_node_key).

%-----------------------------------------------------------------------------%

:- type which_headvars
    --->    all_headvars
    ;       only_user_headvars.

:- pred maybe_filter_headvars(which_headvars::in, list(trace_atom_arg)::in,
    list(trace_atom_arg)::out) is det.

:- func chosen_head_vars_presentation = which_headvars.

:- pred is_user_visible_arg(trace_atom_arg::in) is semidet.

:- pred select_arg_at_pos(arg_pos::in, list(trace_atom_arg)::in,
    trace_atom_arg::out) is det.

:- pred absolute_arg_num(arg_pos::in, trace_atom::in, int::out) is det.

:- pred user_arg_num(arg_pos::in, trace_atom::in, int::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.declarative_debugger.
:- import_module mdb.declarative_edt.

:- import_module exception.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module store.
:- import_module string.
:- import_module univ.

%-----------------------------------------------------------------------------%

get_pred_attributes(ProcId, Module, Name, Arity, PredOrFunc) :-
    (
        ProcId = ordinary_proc_label(Module, PredOrFunc, _, Name, Arity, _)
    ;
        ProcId = special_proc_label(Module, SpecialId, _, _, _, _),
        PredOrFunc = predicate,
        Arity = get_special_pred_id_arity(SpecialId),
        Name = get_special_pred_id_target_name(SpecialId)
    ).

%-----------------------------------------------------------------------------%

:- pragma promise_pure(call_node_maybe_proc_rep/2).

call_node_maybe_proc_rep(CallNode, MaybeProcRep) :-
    Label = CallNode ^ call_label,
    ( call_node_bytecode_layout(Label, ProcLayout) ->
        ( semipure have_cached_proc_rep(ProcLayout, ProcRep) ->
            MaybeProcRep = yes(ProcRep)
        ;
            lookup_proc_bytecode(ProcLayout, ByteCode),
            read_proc_rep(ByteCode, Label, ProcRep),
            impure cache_proc_rep(ProcLayout, ProcRep),
            MaybeProcRep = yes(ProcRep)
        )
    ;
        MaybeProcRep = no
    ).

:- pred call_node_bytecode_layout(label_layout::in, proc_layout::out)
    is semidet.

    % Default version for non-C backends.
call_node_bytecode_layout(_, _) :-
    semidet_fail.

:- pragma foreign_proc("C",
    call_node_bytecode_layout(CallLabelLayout::in, ProcLayout::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ProcLayout = CallLabelLayout->MR_sll_entry;
    if (ProcLayout->MR_sle_body_bytes != NULL) {
#ifdef MR_DEBUG_PROC_REP
        printf(""call_node_bytecode_layout: %p success\\n"", CallLabelLayout);
#endif
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
#ifdef MR_DEBUG_PROC_REP
        printf(""call_node_bytecode_layout: %p failure\\n"", CallLabelLayout);
#endif
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- pred lookup_proc_bytecode(proc_layout::in, bytecode::out) is det.

    % Default version for non-C backends.
lookup_proc_bytecode(_, dummy_bytecode).

:- pragma foreign_proc("C",
    lookup_proc_bytecode(ProcLayout::in, ByteCode::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    ByteCode = ProcLayout->MR_sle_body_bytes;
#ifdef MR_DEBUG_PROC_REP
    printf(""lookup_proc_bytecode: %p %p\\n"", ProcLayout, ByteCode);
#endif
").

:- semipure pred have_cached_proc_rep(proc_layout::in, proc_rep::out)
    is semidet.

    % Default version for non-C backends.
have_cached_proc_rep(_, _) :-
    semidet_fail.

:- pragma foreign_proc("C",
    have_cached_proc_rep(ProcLayout::in, ProcRep::out),
    [will_not_call_mercury, thread_safe, promise_semipure],
"
    ProcRep = MR_lookup_proc_rep(ProcLayout);
    if (ProcRep != 0) {
#ifdef MR_DEBUG_PROC_REP
        printf(""have_cached_proc_rep: %p success\\n"",
            ProcLayout);
#endif
        SUCCESS_INDICATOR = MR_TRUE;
    } else {
#ifdef MR_DEBUG_PROC_REP
        printf(""have_cached_proc_rep: %p failure\\n"",
            ProcLayout);
#endif
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

:- impure pred cache_proc_rep(proc_layout::in, proc_rep::in) is det.

    % Default version for non-C backends.
cache_proc_rep(_, _).

:- pragma foreign_proc("C",
    cache_proc_rep(ProcLayout::in, ProcRep::in),
    [will_not_call_mercury, thread_safe],
"
#ifdef MR_DEBUG_PROC_REP
    printf(""cache_proc_rep: %p %x\\n"", ProcLayout, ProcRep);
#endif
    MR_insert_proc_rep(ProcLayout, ProcRep);
").

%-----------------------------------------------------------------------------%

get_trace_exit_atom(node_exit(_, _, _, AtomArgs, _, Label, _, _)) = Atom :-
    ProcLayout = get_proc_layout_from_label_layout(Label),
    Atom = atom(ProcLayout, AtomArgs).

get_trace_call_atom(node_call(_, _, AtomArgs, _, _, _, _, Label, _, _))
        = Atom :-
    ProcLayout = get_proc_layout_from_label_layout(Label),
    Atom = atom(ProcLayout, AtomArgs).

%-----------------------------------------------------------------------------%

step_left_in_contour(Store, node_exit(_, Call, _, _, _, _, _, _)) = Prec :-
    call_node_from_id(Store, Call, CallNode),
    Prec = CallNode ^ call_preceding.
step_left_in_contour(Store, node_excp(_, Call, _, _, _, _, _)) = Prec :-
    call_node_from_id(Store, Call, CallNode),
    Prec = CallNode ^ call_preceding.
step_left_in_contour(_, node_switch(Prec, _)) = Prec.
step_left_in_contour(_, node_first_disj(Prec, _)) = Prec.
step_left_in_contour(Store, node_later_disj(_, _, FirstDisj)) = Prec :-
    first_disj_node_from_id(Store, FirstDisj, node_first_disj(Prec, _)).
step_left_in_contour(_, node_cond(Prec, _, Status)) = Node :-
    ( Status = failed ->
        throw(internal_error("step_left_in_contour", "failed COND node"))
    ;
        Node = Prec
    ).
step_left_in_contour(_, node_then(Prec, _, _)) = Prec.
step_left_in_contour(Store, node_else(_, Cond, _)) = Prec :-
    cond_node_from_id(Store, Cond, node_cond(Prec, _, _)).
step_left_in_contour(Store, node_neg_succ(_, Neg, _)) = Prec :-
    neg_node_from_id(Store, Neg, node_neg(Prec, _, _)).
    %
    % The following cases are possibly at the left end of a contour,
    % where we cannot step any further.
    %
step_left_in_contour(_, node_call(_, _, _, _, _, _, _, _, _, _)) = _ :-
    throw(internal_error("step_left_in_contour", "unexpected CALL node")).
step_left_in_contour(_, node_neg(Prec, _, Status)) = Next :-
    ( Status = undecided ->
        % An exception must have been thrown inside the negation,
        % so we don't consider it a separate context.
        Next = Prec
    ;
        throw(internal_error("step_left_in_contour", "unexpected NEGE node"))
    ).
    %
    % In the remaining cases we have reached a dead end, so we
    % step to the previous contour instead.
    %
step_left_in_contour(Store, Node) = Prec :-
    Node = node_fail(_, _, _, _, _, _),
    find_prev_contour(Store, Node, Prec).
step_left_in_contour(Store, Node) = Prec :-
    Node = node_redo(_, _, _, _, _),
    find_prev_contour(Store, Node, Prec).
step_left_in_contour(Store, Node) = Prec :-
    Node = node_neg_fail(_, _, _),
    find_prev_contour(Store, Node, Prec).

    % Given any node which is not on a contour, find a node on
    % the previous contour in the same stratum.
    %
:- pred find_prev_contour(S, trace_node(R), R) <= annotated_trace(S, R).
:- mode find_prev_contour(in, in, out) is semidet.
:- mode find_prev_contour(in, in(trace_node_reverse), out) is det.

:- inst trace_node_reverse
    --->    node_fail(ground, ground, ground, ground, ground, ground)
    ;       node_redo(ground, ground, ground, ground, ground)
    ;       node_neg_fail(ground, ground, ground).

find_prev_contour(Store, node_fail(_, Call, _, _, _, _), OnContour) :-
    call_node_from_id(Store, Call, CallNode),
    OnContour = CallNode ^ call_preceding.
find_prev_contour(Store, node_redo(_, Exit, _, _, _), OnContour) :-
    exit_node_from_id(Store, Exit, ExitNode),
    OnContour = ExitNode ^ exit_preceding.
find_prev_contour(Store, node_neg_fail(_, Neg, _), OnContour) :-
    neg_node_from_id(Store, Neg, node_neg(OnContour, _, _)).
    %
    % The following cases are at the left end of a contour,
    % so there are no previous contours in the same stratum.
    %
find_prev_contour(_, node_call(_, _, _, _, _, _, _, _, _, _), _) :-
    throw(internal_error("find_prev_contour", "reached CALL node")).
find_prev_contour(_, node_cond(_, _, _), _) :-
    throw(internal_error("find_prev_contour", "reached COND node")).
find_prev_contour(_, node_neg(_, _, _), _) :-
    throw(internal_error("find_prev_contour", "reached NEGE node")).

step_in_stratum(Store, node_exit(_, Call, MaybeRedo, _, _, _, _, _)) =
    step_over_redo_or_call(Store, Call, MaybeRedo).
step_in_stratum(Store, node_fail(_, Call, MaybeRedo, _, _, _)) =
    step_over_redo_or_call(Store, Call, MaybeRedo).
step_in_stratum(Store, node_excp(_, Call, MaybeRedo, _, _, _, _)) =
    step_over_redo_or_call(Store, Call, MaybeRedo).
step_in_stratum(Store, node_redo(_, Exit, _, _, _)) = Next :-
    exit_node_from_id(Store, Exit, ExitNode),
    Next = ExitNode ^ exit_preceding.
step_in_stratum(_, node_switch(Next, _)) = Next.
step_in_stratum(_, node_first_disj(Next, _)) = Next.
step_in_stratum(_, node_later_disj(Next, _, _)) = Next.
step_in_stratum(_, node_cond(Prec, _, Status)) = Next :-
    ( Status = failed ->
        throw(internal_error("step_in_stratum", "failed COND node"))
    ;
        Next = Prec
    ).
step_in_stratum(_, node_then(Next, _, _)) = Next.
step_in_stratum(Store, node_else(_, Cond, _)) = Next :-
    cond_node_from_id(Store, Cond, node_cond(Next, _, _)).
step_in_stratum(Store, node_neg_succ(_, Neg, _)) = Next :-
    neg_node_from_id(Store, Neg, node_neg(Next, _, _)).
step_in_stratum(Store, node_neg_fail(_, Neg, _)) = Next :-
    neg_node_from_id(Store, Neg, node_neg(Next, _, _)).
    %
    % The following cases mark the boundary of the stratum,
    % so we cannot step any further.
    %
step_in_stratum(_, node_call(_, _, _, _, _, _, _, _, _, _)) = _ :-
    throw(internal_error("step_in_stratum", "unexpected CALL node")).
step_in_stratum(_, node_neg(_, _, _)) = _ :-
    throw(internal_error("step_in_stratum", "unexpected NEGE node")).

:- func step_over_redo_or_call(S, R, R) = R <= annotated_trace(S, R).

step_over_redo_or_call(Store, Call, MaybeRedo) = Next :-
    ( maybe_redo_node_from_id(Store, MaybeRedo, Redo) ->
        Redo = node_redo(Next, _, _, _, _)
    ;
        call_node_from_id(Store, Call, CallNode),
        Next = CallNode ^ call_preceding
    ).

det_trace_node_from_id(Store, NodeId, Node) :-
    ( trace_node_from_id(Store, NodeId, Node0) ->
        Node = Node0
    ;
        throw(internal_error("det_trace_node_from_id", "NULL node id"))
    ).

call_node_from_id(Store, NodeId, Node) :-
    (
        trace_node_from_id(Store, NodeId, Node0),
        Node0 = node_call(_, _, _, _, _, _, _, _, _, _)
    ->
        Node = Node0
    ;
        throw(internal_error("call_node_from_id", "not a CALL node"))
    ).

maybe_redo_node_from_id(Store, NodeId, Node) :-
    trace_node_from_id(Store, NodeId, Node0),
    (
        Node0 = node_redo(_, _, _, _, _)
    ->
        Node = Node0
    ;
        throw(internal_error("maybe_redo_node_from_id",
            "not a REDO node or NULL"))
    ).

exit_node_from_id(Store, NodeId, Node) :-
    (
        trace_node_from_id(Store, NodeId, Node0),
        Node0 = node_exit(_, _, _, _, _, _, _, _)
    ->
        Node = Node0
    ;
        throw(internal_error("exit_node_from_id", "not an EXIT node"))
    ).

cond_node_from_id(Store, NodeId, Node) :-
    (
        trace_node_from_id(Store, NodeId, Node0),
        Node0 = node_cond(_, _, _)
    ->
        Node = Node0
    ;
        throw(internal_error("cond_node_from_id", "not a COND node"))
    ).

neg_node_from_id(Store, NodeId, Node) :-
    (
        trace_node_from_id(Store, NodeId, Node0),
        Node0 = node_neg(_, _, _)
    ->
        Node = Node0
    ;
        throw(internal_error("neg_node_from_id", "not a NEG node"))
    ).

first_disj_node_from_id(Store, NodeId, Node) :-
    (
        trace_node_from_id(Store, NodeId, Node0),
        Node0 = node_first_disj(_, _)
    ->
        Node = Node0
    ;
        throw(internal_error("first_disj_node_from_id",
            "not a first DISJ node"))
    ).

disj_node_from_id(Store, NodeId, Node) :-
    (
        trace_node_from_id(Store, NodeId, Node0),
        ( Node0 = node_first_disj(_, _)
        ; Node0 = node_later_disj(_, _, _)
        )
    ->
        Node = Node0
    ;
        throw(internal_error("disj_node_from_id",
            "not a DISJ node"))
    ).

%-----------------------------------------------------------------------------%

:- instance annotated_trace(trace_node_store, trace_node_id) where [
    pred(trace_node_from_id/3) is search_trace_node_store
].

    % The "map" is actually just an integer representing the version
    % of the map.  The empty map should be given the value 0, and
    % each time the map is destructively modified (by C code), the
    % value should be incremented.
    %
:- type trace_node_store
    --->    store(int).

    % The implementation of the identifiers is the same as what
    % is identified.  This fact is hidden, however, to force the
    % abstract map to be explicitly used whenever a new node is
    % accessed.
    %
:- type trace_node_id
    --->    id(c_pointer).

:- pred search_trace_node_store(trace_node_store::in, trace_node_id::in,
    trace_node(trace_node_id)::out) is semidet.

:- pragma foreign_proc("C",
    search_trace_node_store(_Store::in, Id::in, Node::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Node = Id;
    SUCCESS_INDICATOR = (Id != (MR_Word) NULL);
"
).
search_trace_node_store(_, _, _) :-
    private_builtin.sorry("search_trace_node_store").

    %
    % Following are some predicates that are useful for
    % manipulating the above instance in C code.
    %

:- func call_node_get_last_interface(trace_node(trace_node_id))
    = trace_node_id.
:- pragma foreign_export("C", call_node_get_last_interface(in) = out,
    "MR_DD_call_node_get_last_interface").

call_node_get_last_interface(Call) = Last :-
    ( Call = node_call(_, Last0, _, _, _, _, _, _, _, _) ->
        Last = Last0
    ;
        throw(internal_error("call_node_get_last_interface",
            "not a CALL node"))
    ).

:- func call_node_set_last_interface(trace_node(trace_node_id)::di,
    trace_node_id::di) = (trace_node(trace_node_id)::out) is det.

:- pragma foreign_export("C", call_node_set_last_interface(di, di) = out,
    "MR_DD_call_node_set_last_interface").

call_node_set_last_interface(Call0, Last) = Call :-
    ( Call0 = node_call(_, _, _, _, _, _, _, _, _, _) ->
        Call1 = Call0
    ;
        throw(internal_error("call_node_set_last_interface",
            "not a CALL node"))
    ),
    % The last interface is the second field, so we pass 1
    % (since argument numbers start from 0).
    %
    set_trace_node_arg(Call1, 1, Last, Call).

:- func call_node_update_implicit_tree_info(trace_node(trace_node_id)::di,
    int::di) = (trace_node(trace_node_id)::out) is det.

:- pragma foreign_export("C",
    call_node_update_implicit_tree_info(di, di) = out,
    "MR_DD_call_node_update_implicit_tree_info").

call_node_update_implicit_tree_info(Call0, IdealDepth) = Call :-
    ( Call0 = node_call(_, _, _, _, _, _, _, _, _, _) ->
        Call1 = Call0
    ;
        throw(internal_error("call_node_update_implicit_tree_info",
            "not a CALL node"))
    ),
    % call_at_max_depth is the sixth field, so we pass 5
    % (since argument numbers start from 0).
    %
    set_trace_node_arg(Call1, 5, yes(implicit_tree_info(IdealDepth)), Call).

:- func get_implicit_tree_ideal_depth(trace_node(trace_node_id)) = int.

:- pragma foreign_export("C",
    get_implicit_tree_ideal_depth(in) = out,
    "MR_DD_get_implicit_tree_ideal_depth").

get_implicit_tree_ideal_depth(Call) = IdealDepth :-
    ( MaybeImplicitTreeInfo = Call ^ call_at_max_depth ->
        (
            MaybeImplicitTreeInfo = yes(implicit_tree_info(IdealDepth))
        ;
            MaybeImplicitTreeInfo = no,
            throw(internal_error("get_implicit_tree_max_depth",
                "not at max depth"))
        )
    ;
        throw(internal_error("get_implicit_tree_max_depth", "not a CALL node"))
    ).

:- func cond_node_set_status(trace_node(trace_node_id)::di, goal_status::di)
    = (trace_node(trace_node_id)::out) is det.

:- pragma foreign_export("C", cond_node_set_status(di, di) = out,
    "MR_DD_cond_node_set_status").

cond_node_set_status(Cond0, Status) = Cond :-
    ( Cond0 = node_cond(_, _, _) ->
        Cond1 = Cond0
    ;
        throw(internal_error("cond_node_set_status", "not a COND node"))
    ),
    % The goal status is the third field, so we pass 2
    % (since argument numbers start from 0).
    %
    set_trace_node_arg(Cond1, 2, Status, Cond).

:- func neg_node_set_status(trace_node(trace_node_id)::di, goal_status::di)
    = (trace_node(trace_node_id)::out) is det.

:- pragma foreign_export("C", neg_node_set_status(di, di) = out,
    "MR_DD_neg_node_set_status").

neg_node_set_status(Neg0, Status) = Neg :-
    ( Neg0 = node_neg(_, _, _) ->
        Neg1 = Neg0
    ;
        throw(internal_error("neg_node_set_status", "not a NEGE node"))
    ),
    % The goal status is the third field, so we pass 2
    % (since argument numbers start from 0).
    %
    set_trace_node_arg(Neg1, 2, Status, Neg).

:- pred set_trace_node_arg(trace_node(trace_node_id)::di, int::in, T::di,
        trace_node(trace_node_id)::out) is det.

set_trace_node_arg(Node0, FieldNum, Val, Node) :-
    store.new(S0),
    store.new_ref(Node0, Ref, S0, S1),
    store.arg_ref(Ref, FieldNum, ArgRef, S1, S2),
    store.set_ref_value(ArgRef, Val, S2, S),
    store.extract_ref_value(S, Ref, Node).

:- func trace_node_port(trace_node(trace_node_id)) = trace_port.
:- pragma foreign_export("C", trace_node_port(in) = out,
    "MR_DD_trace_node_port").

trace_node_port(node_call(_, _, _, _, _, _, _, _, _, _)) = call.
trace_node_port(node_exit(_, _, _, _, _, _, _, _))       = exit.
trace_node_port(node_redo(_, _, _, _, _))        = redo.
trace_node_port(node_fail(_, _, _, _, _, _))     = fail.
trace_node_port(node_excp(_, _, _, _, _, _, _))  = exception.
trace_node_port(node_switch(_, _))               = switch.
trace_node_port(node_first_disj(_, _))           = disj.
trace_node_port(node_later_disj(_, _, _))        = disj.
trace_node_port(node_cond(_, _, _))              = ite_cond.
trace_node_port(node_then(_, _, _))              = ite_then.
trace_node_port(node_else(_, _, _))              = ite_else.
trace_node_port(node_neg(_, _, _))               = neg_enter.
trace_node_port(node_neg_succ(_, _, _))          = neg_success.
trace_node_port(node_neg_fail(_, _, _))          = neg_failure.

:- func trace_node_path(trace_node(trace_node_id)) = goal_path_string.
:- pragma foreign_export("C", trace_node_path(in) = out,
    "MR_DD_trace_node_path").

trace_node_path(Node) = Path :-
    Label = get_trace_node_label(Node),
    Path = get_goal_path_from_label_layout(Label).

:- func get_trace_node_label(trace_node(R)) = label_layout.

get_trace_node_label(node_call(_, _, _, _, _, _, _, Label, _, _)) = Label.
get_trace_node_label(node_exit(_, _, _, _, _, Label, _, _)) = Label.
get_trace_node_label(node_redo(_, _, _, Label, _)) = Label.
get_trace_node_label(node_fail(_, _, _, _, Label, _)) = Label.
get_trace_node_label(node_excp(_, _, _, _, _, Label, _)) = Label.
get_trace_node_label(node_switch(_, Label)) = Label.
get_trace_node_label(node_first_disj(_, Label)) = Label.
get_trace_node_label(node_later_disj(_, Label, _)) = Label.
get_trace_node_label(node_cond(_, Label, _)) = Label.
get_trace_node_label(node_then(_, _, Label)) = Label.
get_trace_node_label(node_else(_, _, Label)) = Label.
get_trace_node_label(node_neg(_, Label, _)) = Label.
get_trace_node_label(node_neg_succ(_, _, Label)) = Label.
get_trace_node_label(node_neg_fail(_, _, Label)) = Label.

:- pred trace_node_seqno(trace_node_store::in, trace_node(trace_node_id)::in,
    sequence_number::out) is semidet.

:- pragma foreign_export("C", trace_node_seqno(in, in, out),
    "MR_DD_trace_node_seqno").

trace_node_seqno(S, Node, SeqNo) :-
    ( SeqNo0 = Node ^ call_seq ->
        SeqNo = SeqNo0
    ;
        trace_node_call(S, Node, Call),
        call_node_from_id(S, Call, CallNode),
        SeqNo = CallNode ^ call_seq
    ).

:- pred trace_node_call(trace_node_store::in, trace_node(trace_node_id)::in,
    trace_node_id::out) is semidet.

:- pragma foreign_export("C", trace_node_call(in, in, out),
    "MR_DD_trace_node_call").

trace_node_call(_, node_exit(_, Call, _, _, _, _, _, _), Call).
trace_node_call(S, node_redo(_, Exit, _, _, _), Call) :-
    exit_node_from_id(S, Exit, ExitNode),
    Call = ExitNode ^ exit_call.
trace_node_call(_, node_fail(_, Call, _, _, _, _), Call).
trace_node_call(_, node_excp(_, Call, _, _, _, _, _), Call).

:- pred trace_node_first_disj(trace_node(trace_node_id)::in,
    trace_node_id::out) is semidet.

:- pragma foreign_export("C", trace_node_first_disj(in, out),
    "MR_DD_trace_node_first_disj").

trace_node_first_disj(node_first_disj(_, _), NULL) :-
    null_trace_node_id(NULL).
trace_node_first_disj(node_later_disj(_, _, FirstDisj), FirstDisj).

    % Export a version of this function to be called by C code
    % in trace/mercury_trace_declarative.c.
    %
:- func step_left_in_contour_store(trace_node_store, trace_node(trace_node_id))
    = trace_node_id.
:- pragma foreign_export("C", step_left_in_contour_store(in, in) = out,
    "MR_DD_step_left_in_contour").

step_left_in_contour_store(Store, Node) = step_left_in_contour(Store, Node).

    % Export a version of this function to be called by C code
    % in trace/declarative_debugger.c.  If called with a node
    % that is already on a contour, this function returns the
    % same node.  This saves the C code from having to perform
    % that check itself.
    %
:- func find_prev_contour_store(trace_node_store, trace_node_id)
    = trace_node_id.
:- pragma foreign_export("C", find_prev_contour_store(in, in) = out,
    "MR_DD_find_prev_contour").

find_prev_contour_store(Store, Id) = Prev :-
    det_trace_node_from_id(Store, Id, Node),
    ( find_prev_contour(Store, Node, Prev0) ->
        Prev = Prev0
    ;
        Prev = Id
    ).

    % Print a text representation of a trace node, useful
    % for debugging purposes.
    %
:- pred print_trace_node(io.output_stream::in, trace_node(trace_node_id)::in,
    io::di, io::uo) is det.

:- pragma foreign_export("C", print_trace_node(in, in, di, uo),
    "MR_DD_print_trace_node").

print_trace_node(OutStr, Node, !IO) :-
    convert_node(Node, CNode),
    io.write(OutStr, CNode, !IO).

%-----------------------------------------------------------------------------%

    % Each node type has a Mercury function which constructs a node of
    % that type. The functions are exported to C so that the back end
    % can build an execution tree.
    %
:- func construct_call_node(trace_node_id, list(trace_atom_arg),
    sequence_number, event_number, bool, maybe(label_layout),
    label_layout, int, suspicion_accumulator) = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_call_node(in, in, in, in, in, in, in, in, in) = out,
    "MR_DD_construct_call_node").

construct_call_node(Preceding, AtomArgs, SeqNo, EventNo, AtMaxDepth,
        MaybeReturnLabel, Label, IoSeqNum, Suspicion) = Call :-
    (
        AtMaxDepth = no,
        MaybeImplicitTreeInfo = no
    ;
        AtMaxDepth = yes,
        % The ideal depth of the implicit tree will be updated
        % when the corresponding EXIT, FAIL or EXCP event occurs,
        % so for now we just set it to 0.
        MaybeImplicitTreeInfo = yes(implicit_tree_info(0))
    ),
    null_trace_node_id(LastInterface),
    Call = node_call(Preceding, LastInterface, AtomArgs, SeqNo, EventNo,
        MaybeImplicitTreeInfo, MaybeReturnLabel, Label, IoSeqNum, Suspicion).

:- func make_yes_maybe_label(label_layout) = maybe(label_layout).
:- pragma foreign_export("C", make_yes_maybe_label(in) = out,
    "MR_DD_make_yes_maybe_label").

make_yes_maybe_label(Label) = yes(Label).

:- func make_no_maybe_label = maybe(label_layout).
:- pragma foreign_export("C", make_no_maybe_label = out,
    "MR_DD_make_no_maybe_label").

make_no_maybe_label = no.

:- func construct_exit_node(trace_node_id, trace_node_id, trace_node_id,
    list(trace_atom_arg), event_number, label_layout, int,
    suspicion_accumulator) = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_exit_node(in, in, in, in, in, in, in, in) = out,
    "MR_DD_construct_exit_node").

construct_exit_node(Preceding, Call, MaybeRedo, AtomArgs, EventNo, Label,
        IoSeqNum, Suspicion) =
    node_exit(Preceding, Call, MaybeRedo, AtomArgs, EventNo, Label, IoSeqNum,
        Suspicion).

:- func construct_redo_node(trace_node_id, trace_node_id, event_number,
    label_layout, suspicion_accumulator) = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_redo_node(in, in, in, in, in) = out,
    "MR_DD_construct_redo_node").

construct_redo_node(Preceding, Exit, Event, Label, Suspicion) =
   node_redo(Preceding, Exit, Event, Label, Suspicion).

:- func construct_fail_node(trace_node_id, trace_node_id, trace_node_id,
    event_number, label_layout, suspicion_accumulator)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_fail_node(in, in, in, in, in, in) = out,
    "MR_DD_construct_fail_node").

construct_fail_node(Preceding, Call, Redo, EventNo, Label, Suspicion) =
    node_fail(Preceding, Call, Redo, EventNo, Label, Suspicion).

:- pred construct_excp_node(trace_node_id::in, trace_node_id::in,
    trace_node_id::in, univ::in, event_number::in, label_layout::in,
    suspicion_accumulator::in, trace_node(trace_node_id)::out) is cc_multi.
:- pragma foreign_export("C",
    construct_excp_node(in, in, in, in, in, in, in, out),
    "MR_DD_construct_excp_node").

construct_excp_node(Preceding, Call, MaybeRedo, Exception, EventNo, Label,
        Suspicion, Excp) :-
    term_rep.univ_to_rep(Exception, ExceptionRep),
    Excp = node_excp(Preceding, Call, MaybeRedo, ExceptionRep, EventNo, Label,
        Suspicion).

:- func construct_switch_node(trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_switch_node(in, in) = out,
    "MR_DD_construct_switch_node").

construct_switch_node(Preceding, Label) = node_switch(Preceding, Label).

:- func construct_first_disj_node(trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_first_disj_node(in, in) = out,
    "MR_DD_construct_first_disj_node").

construct_first_disj_node(Preceding, Label) =
    node_first_disj(Preceding, Label).

:- func construct_later_disj_node(trace_node_store, trace_node_id,
    label_layout, trace_node_id) = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_later_disj_node(in, in, in, in) = out,
    "MR_DD_construct_later_disj_node").

construct_later_disj_node(Store, Preceding, Label, PrevDisj)
        = node_later_disj(Preceding, Label, FirstDisj) :-
    disj_node_from_id(Store, PrevDisj, PrevDisjNode),
    (
        PrevDisjNode = node_first_disj(_, _),
        FirstDisj = PrevDisj
    ;
        PrevDisjNode = node_later_disj(_, _, FirstDisj)
    ).

:- func construct_cond_node(trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_cond_node(in, in) = out,
    "MR_DD_construct_cond_node").

construct_cond_node(Preceding, Label) = node_cond(Preceding, Label, undecided).

:- func construct_then_node(trace_node_id, trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_then_node(in, in, in) = out,
    "MR_DD_construct_then_node").

construct_then_node(Preceding, Cond, Label) =
    node_then(Preceding, Cond, Label).

:- func construct_else_node(trace_node_id, trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_else_node(in, in, in) = out,
    "MR_DD_construct_else_node").

construct_else_node(Preceding, Cond, Label) =
    node_else(Preceding, Cond, Label).

:- func construct_neg_node(trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_neg_node(in, in) = out,
    "MR_DD_construct_neg_node").

construct_neg_node(Preceding, Label) = node_neg(Preceding, Label, undecided).

:- func construct_neg_succ_node(trace_node_id, trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_neg_succ_node(in, in, in) = out,
    "MR_DD_construct_neg_succ_node").

construct_neg_succ_node(Preceding, Neg, Label) =
    node_neg_succ(Preceding, Neg, Label).

:- func construct_neg_fail_node(trace_node_id, trace_node_id, label_layout)
    = trace_node(trace_node_id).
:- pragma foreign_export("C",
    construct_neg_fail_node(in, in, in) = out,
    "MR_DD_construct_neg_fail_node").

construct_neg_fail_node(Preceding, Neg, Label) =
    node_neg_fail(Preceding, Neg, Label).

:- pred null_trace_node_id(trace_node_id::out) is det.

:- pragma foreign_proc("C",
    null_trace_node_id(Id::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Id = (MR_Word) NULL;
").

null_trace_node_id(_) :-
    private_builtin.sorry("null_trace_node_id").

:- func init_trace_atom_args = list(trace_atom_arg).

:- pragma foreign_export("C", init_trace_atom_args = out,
    "MR_DD_init_trace_atom_args").

init_trace_atom_args = [].

    % add_trace_atom_arg_value(HldsNum, ProgVis, Val, !AtomArgs):
    %
    % Add the argument with value Val and HLDS number HldsNum to the beginning
    % of a list of arguments for an atom.  ProgVis is a C boolean, which is
    % true iff variable HldsNum is a user visible variable.
    %
:- pred add_trace_atom_arg_value(int::in, int::in, univ::in,
    list(trace_atom_arg)::in, list(trace_atom_arg)::out) is cc_multi.
:- pragma foreign_export("C", add_trace_atom_arg_value(in, in, in, in, out),
    "MR_DD_add_trace_atom_arg_value").

add_trace_atom_arg_value(HldsNum, ProgVis, Val, Args, [Arg | Args]) :-
    term_rep.univ_to_rep(Val, Rep),
    Arg = arg_info(c_bool_to_merc_bool(ProgVis), HldsNum, yes(Rep)).

    % Like add_trace_atom_arg_value, except that the specified variable
    % has no value (i.e. it is not bound).
    %
:- pred add_trace_atom_arg_no_value(int::in, int::in,
    list(trace_atom_arg)::in, list(trace_atom_arg)::out) is det.
:- pragma foreign_export("C", add_trace_atom_arg_no_value(in, in, in, out),
    "MR_DD_add_trace_atom_arg_no_value").

add_trace_atom_arg_no_value(HldsNum, ProgVis, Args, [Arg | Args]) :-
    Arg = arg_info(c_bool_to_merc_bool(ProgVis), HldsNum, no).

    % This code converts a C bool (represented as int) to a Mercury bool.
    %
:- func c_bool_to_merc_bool(int) = bool.

c_bool_to_merc_bool(ProgVis) =
    ( ProgVis = 0 ->
        no
    ;
        yes
    ).

    % Create a temporary placeholder until the code MR_decl_make_atom
    % can fill in all the argument slots.
    %
:- func dummy_arg_info = trace_atom_arg.

dummy_arg_info = arg_info(no, -1, no).

%-----------------------------------------------------------------------------%

    % The most important property of this instance is that it
    % can be written to or read in from a stream easily.  It
    % is not as efficient to use as the earlier instance, though.
    %
:- instance annotated_trace(trace_node_map, trace_node_key) where [
    pred(trace_node_from_id/3) is search_trace_node_map
].

:- type trace_node_map
    --->    map(map(trace_node_key, trace_node(trace_node_key))).

    % Values of this type are represented in the same way (in the
    % underlying C code) as corresponding values of the other
    % instance.
    %
:- type trace_node_key
    --->    key(int).

:- pred search_trace_node_map(trace_node_map::in, trace_node_key::in,
        trace_node(trace_node_key)::out) is semidet.

search_trace_node_map(map(Map), Key, Node) :-
    map.search(Map, Key, Node).

load_trace_node_map(Stream, Map, Key, !IO) :-
    io.read(Stream, ResKey, !IO),
    (
        ResKey = ok(Key)
    ;
        ResKey = eof,
        throw(io_error("load_trace_node_map", "unexpected EOF"))
    ;
        ResKey = error(Msg, _),
        throw(io_error("load_trace_node_map", Msg))
    ),
    io.read(Stream, ResMap, !IO),
    (
        ResMap = ok(Map)
    ;
        ResMap = eof,
        throw(io_error("load_trace_node_map", "unexpected EOF"))
    ;
        ResMap = error(Msg, _),
        throw(io_error("load_trace_node_map", Msg))
    ).

:- pragma foreign_export("C", save_trace_node_store(in, in, in, di, uo),
    "MR_DD_save_trace").

save_trace_node_store(Stream, Store, NodeId, !IO) :-
    map.init(Map0),
    node_id_to_key(NodeId, Key),
    node_map(Store, NodeId, map(Map0), Map),
    io.write(Stream, Key, !IO),
    io.write_string(Stream, ".\n", !IO),
    io.write(Stream, Map, !IO),
    io.write_string(Stream, ".\n", !IO).

:- pred node_map(trace_node_store::in, trace_node_id::in, trace_node_map::in,
    trace_node_map::out) is det.

node_map(Store, NodeId, map(Map0), Map) :-
    ( search_trace_node_store(Store, NodeId, Node1) ->
        node_id_to_key(NodeId, Key),
        convert_node(Node1, Node2),
        map.det_insert(Map0, Key, Node2, Map1),
        Next = preceding_node(Node1),
        node_map(Store, Next, map(Map1), Map)
    ;
        Map = map(Map0)
    ).

:- pred node_id_to_key(trace_node_id::in, trace_node_key::out) is det.

:- pragma foreign_proc("C",
    node_id_to_key(Id::in, Key::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Key = (MR_Integer) Id;
").

node_id_to_key(_, _) :-
    private_builtin.sorry("node_id_to_key").

:- pred convert_node(trace_node(trace_node_id)::in,
    trace_node(trace_node_key)::out) is det.

:- pragma foreign_proc("C",
    convert_node(N1::in, N2::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N2 = N1;
").

convert_node(_, _) :-
    private_builtin.sorry("convert_node").

    % Given a node in an annotated trace, return a reference to
    % the preceding node in the trace, or a NULL reference if
    % it is the first.
    %
:- func preceding_node(trace_node(T)) = T.

preceding_node(node_call(P, _, _, _, _, _, _, _, _, _))  = P.
preceding_node(node_exit(P, _, _, _, _, _, _, _))        = P.
preceding_node(node_redo(P, _, _, _, _))         = P.
preceding_node(node_fail(P, _, _, _, _, _))          = P.
preceding_node(node_excp(P, _, _, _, _, _, _))       = P.
preceding_node(node_switch(P, _))                = P.
preceding_node(node_first_disj(P, _))            = P.
preceding_node(node_later_disj(P, _, _))         = P.
preceding_node(node_cond(P, _, _))               = P.
preceding_node(node_then(P, _, _))               = P.
preceding_node(node_else(P, _, _))               = P.
preceding_node(node_neg(P, _, _))                = P.
preceding_node(node_neg_succ(P, _, _))           = P.
preceding_node(node_neg_fail(P, _, _))           = P.

%-----------------------------------------------------------------------------%

maybe_filter_headvars(Which, Args0, Args) :-
    (
        Which = all_headvars,
        Args = Args0
    ;
        Which = only_user_headvars,
        Args = list.filter(is_user_visible_arg, Args0)
    ).

chosen_head_vars_presentation = only_user_headvars.

is_user_visible_arg(arg_info(yes, _, _)).

select_arg_at_pos(ArgPos, Args0, Arg) :-
    (
        ArgPos = user_head_var(N),
        Which = only_user_headvars
    ;
        ArgPos = any_head_var(N),
        Which = all_headvars
    ;
        ArgPos = any_head_var_from_back(M),
        N = length(Args0) - M + 1,
        Which = all_headvars
    ),
    maybe_filter_headvars(Which, Args0, Args),
    list.index1_det(Args, N, Arg).

absolute_arg_num(any_head_var(ArgNum), _, ArgNum).
absolute_arg_num(user_head_var(N), atom(_, Args), ArgNum) :-
    head_var_num_to_arg_num(Args, N, 1, ArgNum).
absolute_arg_num(any_head_var_from_back(M), atom(_, Args), length(Args)-M+1).

user_arg_num(user_head_var(ArgNum), _, ArgNum).
user_arg_num(any_head_var(AnyArgNum), atom(_, Args), ArgNum) :-
    arg_num_to_head_var_num(Args, AnyArgNum, 1, ArgNum).
user_arg_num(any_head_var_from_back(AnyArgNumFromBack), atom(_, Args),
        ArgNum) :-
    arg_num_to_head_var_num(Args,
        list.length(Args) - AnyArgNumFromBack + 1, 1, ArgNum).

:- pred head_var_num_to_arg_num(list(trace_atom_arg)::in, int::in, int::in,
    int::out) is det.

head_var_num_to_arg_num([], _, _, _) :-
    throw(internal_error("head_var_num_to_arg_num",
        "nonexistent head_var_num")).
head_var_num_to_arg_num([Arg | Args], SearchUserHeadVarNum, CurArgNum,
        ArgNum) :-
    Arg = arg_info(UserVis, _, _),
    (
        UserVis = no,
        head_var_num_to_arg_num(Args, SearchUserHeadVarNum,
            CurArgNum + 1, ArgNum)
    ;
        UserVis = yes,
        ( SearchUserHeadVarNum = 1 ->
            ArgNum = CurArgNum
        ;
            head_var_num_to_arg_num(Args, SearchUserHeadVarNum - 1,
                CurArgNum + 1, ArgNum)
        )
    ).

:- pred arg_num_to_head_var_num(list(trace_atom_arg)::in, int::in, int::in,
    int::out) is det.

arg_num_to_head_var_num([], _, _, _) :-
    throw(internal_error("arg_num_to_head_var_num",
        "nonexistent arg num")).
arg_num_to_head_var_num([Arg | Args], ArgNum, CurArgNum, UserArgNum) :-
    Arg = arg_info(UserVis, _, _),
    (
        UserVis = no,
        arg_num_to_head_var_num(Args, ArgNum - 1, CurArgNum,
            UserArgNum)
    ;
        UserVis = yes,
        ( ArgNum = 1 ->
            UserArgNum = CurArgNum
        ;
            arg_num_to_head_var_num(Args, ArgNum - 1,
                CurArgNum + 1, UserArgNum)
        )
    ).

%-----------------------------------------------------------------------------%

:- type bytecode --->   dummy_bytecode.

:- pragma foreign_type("C", bytecode, "const MR_uint_least8_t *",
    [can_pass_as_mercury_type, stable]).
:- pragma foreign_type("Java", bytecode, "java.lang.Object", []). %stub only

:- pred read_proc_rep(bytecode::in, label_layout::in, proc_rep::out) is det.
:- pragma foreign_export("C", read_proc_rep(in, in, out),
    "MR_DD_trace_read_rep").

read_proc_rep(Bytecode, Label, ProcRep) :-
    some [!Pos] (
        !:Pos = 0,
        read_int32(Bytecode, !Pos, Limit),
        read_var_num_rep(Bytecode, !Pos, VarNumRep),
        read_string(Bytecode, Label, !Pos, FileName),
        Info = read_proc_rep_info(Limit, FileName),
        read_vars(VarNumRep, Bytecode, !Pos, HeadVars),
        read_goal(VarNumRep, Bytecode, Label, !Pos, Info, Goal),
        ProcRep = proc_rep(HeadVars, Goal),
        require(unify(!.Pos, Limit), "read_proc_rep: limit mismatch")
    ).

:- type read_proc_rep_info
    --->    read_proc_rep_info(
                limit       :: int,
                filename    :: string
            ).

:- pred read_goal(var_num_rep::in, bytecode::in, label_layout::in, int::in,
    int::out, read_proc_rep_info::in, goal_rep::out) is det.

read_goal(VarNumRep, Bytecode, Label, !Pos, Info, Goal) :-
    read_byte(Bytecode, !Pos, GoalTypeByte),
    ( byte_to_goal_type(GoalTypeByte) = GoalType ->
        (
            GoalType = goal_conj,
            read_goals(VarNumRep, Bytecode, Label, !Pos, Info,  Goals),
            Goal = conj_rep(Goals)
        ;
            GoalType = goal_disj,
            read_goals(VarNumRep, Bytecode, Label, !Pos, Info, Goals),
            Goal = disj_rep(Goals)
        ;
            GoalType = goal_neg,
            read_goal(VarNumRep, Bytecode, Label, !Pos, Info, SubGoal),
            Goal = negation_rep(SubGoal)
        ;
            GoalType = goal_ite,
            read_goal(VarNumRep, Bytecode, Label, !Pos, Info, Cond),
            read_goal(VarNumRep, Bytecode, Label, !Pos, Info, Then),
            read_goal(VarNumRep, Bytecode, Label, !Pos, Info, Else),
            Goal = ite_rep(Cond, Then, Else)
        ;
            GoalType = goal_switch,
            read_goals(VarNumRep, Bytecode, Label, !Pos, Info, Goals),
            Goal = switch_rep(Goals)
        ;
            GoalType = goal_assign,
            read_var(VarNumRep, Bytecode, !Pos, Target),
            read_var(VarNumRep, Bytecode, !Pos, Source),
            AtomicGoal = unify_assign_rep(Target, Source),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_construct,
            read_var(VarNumRep, Bytecode, !Pos, Var),
            read_cons_id(Bytecode, Label, !Pos, ConsId),
            read_vars(VarNumRep, Bytecode, !Pos, ArgVars),
            AtomicGoal = unify_construct_rep(Var, ConsId, ArgVars),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_deconstruct,
            read_var(VarNumRep, Bytecode, !Pos, Var),
            read_cons_id(Bytecode, Label, !Pos, ConsId),
            read_vars(VarNumRep, Bytecode, !Pos, ArgVars),
            AtomicGoal = unify_deconstruct_rep(Var, ConsId, ArgVars),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_partial_construct,
            read_var(VarNumRep, Bytecode, !Pos, Var),
            read_cons_id(Bytecode, Label, !Pos, ConsId),
            read_maybe_vars(VarNumRep, Bytecode, !Pos, MaybeVars),
            AtomicGoal = partial_construct_rep(Var, ConsId, MaybeVars),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_partial_deconstruct,
            read_var(VarNumRep, Bytecode, !Pos, Var),
            read_cons_id(Bytecode, Label, !Pos, ConsId),
            read_maybe_vars(VarNumRep, Bytecode, !Pos, MaybeVars),
            AtomicGoal = partial_deconstruct_rep(Var, ConsId, MaybeVars),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_simple_test,
            read_var(VarNumRep, Bytecode, !Pos, Var1),
            read_var(VarNumRep, Bytecode, !Pos, Var2),
            AtomicGoal = unify_simple_test_rep(Var1, Var2),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_scope,
            read_byte(Bytecode, !Pos, MaybeCutByte),
            ( MaybeCutByte = 0 ->
                MaybeCut = no_cut
            ; MaybeCutByte = 1 ->
                MaybeCut = cut
            ;
                error("read_goal: bad maybe_cut")
            ),
            read_goal(VarNumRep, Bytecode, Label, !Pos, Info, SubGoal),
            Goal = scope_rep(SubGoal, MaybeCut)
        ;
            GoalType = goal_ho_call,
            read_var(VarNumRep, Bytecode, !Pos, Var),
            read_vars(VarNumRep, Bytecode, !Pos, Args),
            AtomicGoal = higher_order_call_rep(Var, Args),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_method_call,
            read_var(VarNumRep, Bytecode, !Pos, Var),
            read_method_num(Bytecode, !Pos, MethodNum),
            read_vars(VarNumRep, Bytecode, !Pos, Args),
            AtomicGoal = method_call_rep(Var, MethodNum, Args),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_cast,
            read_var(VarNumRep, Bytecode, !Pos, OutputVar),
            read_var(VarNumRep, Bytecode, !Pos, InputVar),
            AtomicGoal = cast_rep(OutputVar, InputVar),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_plain_call,
            read_string(Bytecode, Label, !Pos, ModuleName),
            read_string(Bytecode, Label, !Pos, PredName),
            read_vars(VarNumRep, Bytecode, !Pos, Args),
            AtomicGoal = plain_call_rep(ModuleName, PredName, Args),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_builtin_call,
            read_string(Bytecode, Label, !Pos, ModuleName),
            read_string(Bytecode, Label, !Pos, PredName),
            read_vars(VarNumRep, Bytecode, !Pos, Args),
            AtomicGoal = builtin_call_rep(ModuleName, PredName, Args),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_event_call,
            read_string(Bytecode, Label, !Pos, EventName),
            read_vars(VarNumRep, Bytecode, !Pos, Args),
            AtomicGoal = event_call_rep(EventName, Args),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        ;
            GoalType = goal_foreign,
            read_vars(VarNumRep, Bytecode, !Pos, Args),
            AtomicGoal = pragma_foreign_code_rep(Args),
            read_atomic_info(VarNumRep, Bytecode, Label, !Pos,
                Info, AtomicGoal, Goal)
        )
    ;
        error("read_goal: invalid goal type")
    ).

:- pred read_atomic_info(var_num_rep::in, bytecode::in, label_layout::in,
    int::in, int::out, read_proc_rep_info::in, atomic_goal_rep::in,
    goal_rep::out) is det.

read_atomic_info(VarNumRep, Bytecode, Label, !Pos, Info, AtomicGoal, Goal) :-
    read_byte(Bytecode, !Pos, DetismByte),
    ( determinism_representation(DetismPrime, DetismByte) ->
        Detism = DetismPrime
    ;
        error("read_atomic_info: bad detism")
    ),
    read_string(Bytecode, Label, !Pos, FileName0),
    ( FileName0 = "" ->
        FileName = Info ^ filename
    ;
        FileName = FileName0
    ),
    read_lineno(Bytecode, !Pos, LineNo),
    read_vars(VarNumRep, Bytecode, !Pos, BoundVars),
    Goal = atomic_goal_rep(Detism, FileName, LineNo, BoundVars, AtomicGoal).

:- pred read_goals(var_num_rep::in, bytecode::in, label_layout::in, int::in,
    int::out, read_proc_rep_info::in, list(goal_rep)::out) is det.

read_goals(VarNumRep, Bytecode, Label, !Pos, Info, Goals) :-
    read_length(Bytecode, !Pos, Len),
    read_goals_2(VarNumRep, Bytecode, Label, !Pos, Info, Len, Goals).

:- pred read_goals_2(var_num_rep::in, bytecode::in, label_layout::in, int::in,
    int::out, read_proc_rep_info::in, int::in, list(goal_rep)::out) is det.

read_goals_2(VarNumRep, Bytecode, Label, !Pos, Info, N, Goals) :-
    ( N > 0 ->
        read_goal(VarNumRep, Bytecode, Label, !Pos, Info, Head),
        read_goals_2(VarNumRep, Bytecode, Label, !Pos, Info, N - 1, Tail),
        Goals = [Head | Tail]
    ;
        Goals = []
    ).

:- pred read_vars(var_num_rep::in, bytecode::in, int::in, int::out,
    list(var_rep)::out) is det.

read_vars(VarNumRep, Bytecode, !Pos, Vars) :-
    read_length(Bytecode, !Pos, Len),
    read_vars_2(VarNumRep, Bytecode, Len, !Pos, Vars).

:- pred read_vars_2(var_num_rep::in, bytecode::in, int::in, int::in, int::out,
    list(var_rep)::out) is det.

read_vars_2(VarNumRep, Bytecode, N, !Pos, Vars) :-
    ( N > 0 ->
        read_var(VarNumRep, Bytecode, !Pos, Head),
        read_vars_2(VarNumRep, Bytecode, N - 1, !Pos, Tail),
        Vars = [Head | Tail]
    ;
        Vars = []
    ).

:- pred read_maybe_vars(var_num_rep::in, bytecode::in, int::in, int::out,
    list(maybe(var_rep))::out) is det.

read_maybe_vars(VarNumRep, Bytecode, !Pos, MaybeVars) :-
    read_length(Bytecode, !Pos, Len),
    read_maybe_vars_2(VarNumRep, Bytecode, Len, !Pos, MaybeVars).

:- pred read_maybe_vars_2(var_num_rep::in, bytecode::in, int::in, int::in,
    int::out, list(maybe(var_rep))::out) is det.

read_maybe_vars_2(VarNumRep, Bytecode, N, !Pos, MaybeVars) :-
    ( N > 0 ->
        read_byte(Bytecode, !Pos, YesOrNo),
        ( YesOrNo = 1 ->
            read_var(VarNumRep, Bytecode, !Pos, Head),
            MaybeHead = yes(Head)
        ; YesOrNo = 0 ->
            MaybeHead = no
        ; throw(internal_error("read_maybe_vars_2",
            "invalid yes or no flag"))
        ),
        read_maybe_vars_2(VarNumRep, Bytecode, N - 1, !Pos, Tail),
        MaybeVars = [MaybeHead | Tail]
    ;
        MaybeVars = []
    ).

:- pred read_var(var_num_rep::in, bytecode::in, int::in, int::out,
    var_rep::out) is det.

read_var(VarNumRep, Bytecode, !Pos, Var) :-
    (
        VarNumRep = byte,
        read_byte(Bytecode, !Pos, Var)
    ;
        VarNumRep = short,
        read_short(Bytecode, !Pos, Var)
    ).

:- pred read_length(bytecode::in, int::in, int::out, var_rep::out) is det.

read_length(Bytecode, !Pos, Len) :-
    read_short(Bytecode, !Pos, Len).

:- pred read_lineno(bytecode::in, int::in, int::out, var_rep::out) is det.

read_lineno(Bytecode, !Pos, LineNo) :-
    read_short(Bytecode, !Pos, LineNo).

:- pred read_method_num(bytecode::in, int::in, int::out, var_rep::out) is det.

read_method_num(Bytecode, !Pos, MethodNum) :-
    read_short(Bytecode, !Pos, MethodNum).

:- pred read_cons_id(bytecode::in, label_layout::in, int::in, int::out,
    cons_id_rep::out) is det.

read_cons_id(Bytecode, Label, !Pos, ConsId) :-
    read_string(Bytecode, Label, !Pos, ConsId).

%-----------------------------------------------------------------------------%

:- pred read_byte(bytecode::in, int::in, int::out, int::out) is det.

:- pragma foreign_proc("C",
    read_byte(Bytecode::in, Pos0::in, Pos::out, Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = Bytecode[Pos0];
    Pos = Pos0 + 1;
").

:- pred read_short(bytecode::in, int::in, int::out, int::out) is det.

:- pragma foreign_proc("C",
    read_short(Bytecode::in, Pos0::in, Pos::out, Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = (Bytecode[Pos0] << 8) + Bytecode[Pos0+1];
    Pos = Pos0 + 2;
").

:- pred read_int32(bytecode::in, int::in, int::out, int::out) is det.

:- pragma foreign_proc("C",
    read_int32(Bytecode::in, Pos0::in, Pos::out, Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Value = (Bytecode[Pos0] << 24) + (Bytecode[Pos0+1] << 16) +
        (Bytecode[Pos0+2] << 8) + Bytecode[Pos0+3];
    Pos = Pos0 + 4;
").

:- pred read_string(bytecode::in, label_layout::in, int::in, int::out,
    string::out) is det.

:- pragma foreign_proc("C",
    read_string(Bytecode::in, Label::in, Pos0::in, Pos::out, Value::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    int         offset;
    const char  *str;

    offset = (Bytecode[Pos0] << 24) + (Bytecode[Pos0+1] << 16) +
        (Bytecode[Pos0+2] << 8) + Bytecode[Pos0+3];
    Pos = Pos0 + 4;
    str = Label->MR_sll_entry->MR_sle_module_layout->MR_ml_string_table
        + offset;
    MR_make_aligned_string(Value, str);
").

:- pred read_var_num_rep(bytecode::in, int::in, int::out, var_num_rep::out)
    is det.

read_var_num_rep(Bytecode, !Pos, VarNumRep) :-
    read_byte(Bytecode, !Pos, Byte),
    ( var_num_rep_byte(VarNumRep0, Byte) ->
        VarNumRep = VarNumRep0
    ;
        error("read_var_num_rep: unknown var_num_rep")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
