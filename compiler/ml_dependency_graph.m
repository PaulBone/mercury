%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_dependency_graph.m.
% Main authors: pbone
%
% This dependency graph records which mlds definitions depend on which other
% definitions.  It is a specialisation of the one in dependency_graph.m.
%
% NOTE that this only tracks dependencies between functions for the purpose
% of determining the call graph of the module.
%
% What is represented:
%
%   Edges from caller to callee when the call is first-order.
%
% What is not represented:
%
%   Data
%   Functions referenced in other ways, passed as arugments, put into
%   variables etc or referenced by rvals generally.
%
%-----------------------------------------------------------------------------%
:- module ml_backend.ml_dependency_graph.
:- interface.

:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module ml_backend.mlds.

:- import_module list.

%-----------------------------------------------------------------------------%

:- type ml_dependency_info      == dependency_info(mlds_proc_label).

:- type ml_dependency_ordering  == dependency_ordering(mlds_proc_label).
:- type ml_dependency_graph     == dependency_graph(mlds_proc_label).
:- type ml_dependency_graph_key == dependency_graph_key(mlds_proc_label).

%-----------------------------------------------------------------------------%

:- func ml_make_dependency_info(list(mlds_defn)) = ml_dependency_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
:- implementation.

:- import_module ml_backend.ml_util.

:- import_module digraph.
:- import_module maybe.
:- import_module require.

%-----------------------------------------------------------------------------%

ml_make_dependency_info(Defns) = make_dependency_info(!:Graph) :-
    !:Graph = init,
    foldl(make_graph_defn, Defns, !Graph).

:- pred make_graph_defn(mlds_defn::in,
    ml_dependency_graph::in, ml_dependency_graph::out) is det.

make_graph_defn(Defn, !Graph) :-
    Entity = Defn ^ md_entity_defn,
    (
        Entity = mlds_data(_, _, _)
    ;
        Entity = mlds_function(_, _, Body, _, _, _),
        Name = Defn ^ md_entity_name,
        (
            Name = entity_function(PredLabel, ProcId, _, _),
            ProcLabel = mlds_proc_label(PredLabel, ProcId)
        ;
            ( Name = entity_type(_, _)
            ; Name = entity_data(_)
            ; Name = entity_export(_)
            ),
            unexpected($file, $pred,
                "Functions must have function names")
        ),
        % Allways add a vertex, otherwise if the function has no callees
        % and no callers then this vertex would be missing from the graph.
        digraph.add_vertex(ProcLabel, ProcLabelVertex, !Graph),
        (
            Body = body_defined_here(Stmt),
            make_graph_stmt(ProcLabelVertex, Stmt, !Graph)
        ;
            Body = body_external
        )
    ;
        Entity = mlds_class(ClassDefn),
        % CTors probably wont be part of mutual recursion, but we examine
        % them anyway since they are functions.
        foldl(make_graph_defn, ClassDefn ^ mcd_ctors, !Graph),
        foldl(make_graph_defn, ClassDefn ^ mcd_members, !Graph)
    ).

:- pred make_graph_stmt(digraph_key(mlds_proc_label)::in, statement::in,
    ml_dependency_graph::in, ml_dependency_graph::out) is det.

make_graph_stmt(CallerKey, statement(Stmt, _), !Graph) :-
    (
        Stmt = ml_stmt_block(Defns, Stmts),
        foldl(make_graph_defn, Defns, !Graph),
        foldl(make_graph_stmt(CallerKey), Stmts, !Graph)
    ;
        Stmt = ml_stmt_while(_, _, SubStmt),
        make_graph_stmt(CallerKey, SubStmt, !Graph)
    ;
        Stmt = ml_stmt_if_then_else(_, Then, MaybeElse),
        make_graph_stmt(CallerKey, Then, !Graph),
        (
            MaybeElse = yes(Else),
            make_graph_stmt(CallerKey, Else, !Graph)
        ;
            MaybeElse = no
        )
    ;
        Stmt = ml_stmt_switch(_, _, _, Cases, Default),
        (
            Default = default_is_unreachable
        ;
            Default = default_do_nothing
        ;
            Default = default_case(SubStmt),
            make_graph_stmt(CallerKey, SubStmt, !Graph)
        ),
        foldl(make_graph_case(CallerKey), Cases, !Graph)
    ;
        Stmt = ml_stmt_label(_)
    ;
        Stmt = ml_stmt_goto(_)
    ;
        Stmt = ml_stmt_computed_goto(_, _)
    ;
        Stmt = ml_stmt_call(_, CalleeRval, _, _, _, _, _),
        ( if CalleeRval = ml_const(mlconst_code_addr(CodeAddr)) then
            CalleeLabel = code_address_get_proc_label(CodeAddr),
            digraph.add_vertex(CalleeLabel, CalleeKey, !Graph),
            digraph.add_edge(CallerKey, CalleeKey, !Graph)
        else
            true
        )
    ;
        Stmt = ml_stmt_return(_)
    ;
        Stmt = ml_stmt_try_commit(_, SubStmt1, SubStmt2),
        make_graph_stmt(CallerKey, SubStmt1, !Graph),
        make_graph_stmt(CallerKey, SubStmt2, !Graph)
    ;
        Stmt = ml_stmt_do_commit(_)
    ;
        Stmt = ml_stmt_atomic(_)
    ).

:- pred make_graph_case(digraph_key(mlds_proc_label)::in, mlds_switch_case::in,
    ml_dependency_graph::in, ml_dependency_graph::out) is det.

make_graph_case(CallerKey, mlds_switch_case(_, _, Stmt), !Graph) :-
    make_graph_stmt(CallerKey, Stmt, !Graph).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
