%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2007, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File stack_alloc.m.
% Authors: zs, conway.
%
% This module allocates stack slots to the variables that need to be saved
% across a call, across a goal that may fail, or in a parallel conjunction.
%
% The jobs is done in two steps. First we traverse the predicate definition
% looking for sets of variables that must be saved on the stack at the same
% time. If --optimize-stack-slots is set, then this phase is done by
% stack_opt.m; if --optimize-stack-slots is not set, then it is done by this
% module. Then we use a graph colouring algorithm to find an allocation of
% stack slots (colours) to variables such that in each set of variables that
% must be saved at the same time, each variable has a different colour.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.stack_alloc.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots_in_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.live_vars.
:- import_module ll_backend.liveness.
:- import_module ll_backend.llds.
:- import_module ll_backend.trace_gen.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module array.
:- import_module bool.
:- import_module enum.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

%-----------------------------------------------------------------------------%

allocate_stack_slots_in_proc(ModuleInfo, proc(PredId, ProcId), !ProcInfo) :-
    initial_liveness(!.ProcInfo, PredId, ModuleInfo, Liveness0),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_trace_level(Globals, TraceLevel),
    (
        eff_trace_level_needs_fail_vars(ModuleInfo, PredInfo, !.ProcInfo,
            TraceLevel) = yes
    ->
        trace_fail_vars(ModuleInfo, !.ProcInfo, FailVars)
    ;
        FailVars = set_of_var.init
    ),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness),
    globals.lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    build_dummy_type_array(ModuleInfo, VarTypes, DummyTypeArray, DummyVars),
    AllocData = alloc_data(ModuleInfo, !.ProcInfo, proc(PredId, ProcId),
        TypeInfoLiveness, OptNoReturnCalls, DummyTypeArray),
    NondetLiveness0 = set_of_var.init,
    SimpleStackAlloc0 = stack_alloc(set.make_singleton_set(FailVars)),
    proc_info_get_goal(!.ProcInfo, Goal0),
    build_live_sets_in_goal_no_par_stack(Goal0, Goal, FailVars, AllocData,
        SimpleStackAlloc0, SimpleStackAlloc, Liveness0, _Liveness,
        NondetLiveness0, _NondetLiveness),
    proc_info_set_goal(Goal, !ProcInfo),
    SimpleStackAlloc = stack_alloc(LiveSets0),

    do_we_need_maxfr_slot(Globals, ModuleInfo, PredInfo, !ProcInfo),
    trace_reserved_slots(ModuleInfo, PredInfo, !.ProcInfo, Globals,
        NumReservedSlots, MaybeReservedVarInfo),
    (
        MaybeReservedVarInfo = yes(ResVar - _),
        ResVarSet = set_of_var.make_singleton(ResVar),
        set.insert(ResVarSet, LiveSets0, LiveSets)
    ;
        MaybeReservedVarInfo = no,
        LiveSets = LiveSets0
    ),
    graph_colour_group_elements(LiveSets, ColourSets),
    set.to_sorted_list(ColourSets, ColourList),

    CodeModel = proc_info_interface_code_model(!.ProcInfo),
    MainStack = code_model_to_main_stack(CodeModel),
    allocate_stack_slots(ColourList, Globals, MainStack, VarTypes,
        NumReservedSlots, MaybeReservedVarInfo, StackSlots1),
    allocate_dummy_stack_slots(DummyVars, MainStack, -1,
        StackSlots1, StackSlots),
    proc_info_set_stack_slots(StackSlots, !ProcInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type stack_alloc
    --->    stack_alloc(
                % The sets of vars that need to be on the stack
                % at the same time.
                set(set_of_progvar)
            ).

:- instance stack_alloc_info(stack_alloc) where [
    pred(at_call_site/4) is alloc_at_call_site,
    pred(at_resume_site/4) is alloc_at_resume_site,
    pred(at_par_conj/4) is alloc_at_par_conj,
    pred(at_recursive_call_for_loop_control/4) is
        alloc_at_recursive_call_for_loop_control
].

:- pred alloc_at_call_site(need_across_call::in, alloc_data::in,
    stack_alloc::in, stack_alloc::out) is det.

alloc_at_call_site(NeedAtCall, AllocData, !StackAlloc) :-
    NeedAtCall = need_across_call(ForwardVars, ResumeVars, NondetLiveVars),
    LiveSet0 = set_of_var.union_list([ForwardVars, ResumeVars,
        NondetLiveVars]),
    filter_out_dummy_vars(AllocData, LiveSet0, LiveSet),

    !.StackAlloc = stack_alloc(LiveSets0),
    LiveSets = set.insert(LiveSets0, LiveSet),
    !:StackAlloc = stack_alloc(LiveSets).

:- pred alloc_at_resume_site(need_in_resume::in, alloc_data::in,
    stack_alloc::in, stack_alloc::out) is det.

alloc_at_resume_site(NeedAtResume, AllocData, !StackAlloc) :-
    NeedAtResume = need_in_resume(ResumeOnStack, ResumeVars, NondetLiveVars),
    (
        ResumeOnStack = no
    ;
        ResumeOnStack = yes,
        LiveSet0 = set_of_var.union(ResumeVars, NondetLiveVars),
        filter_out_dummy_vars(AllocData, LiveSet0, LiveSet),

        !.StackAlloc = stack_alloc(LiveSets0),
        LiveSets = set.insert(LiveSets0, LiveSet),
        !:StackAlloc = stack_alloc(LiveSets)
    ).

:- pred alloc_at_par_conj(need_in_par_conj::in, alloc_data::in,
    stack_alloc::in, stack_alloc::out) is det.

alloc_at_par_conj(NeedParConj, AllocData, !StackAlloc) :-
    NeedParConj = need_in_par_conj(StackVars0),
    filter_out_dummy_vars(AllocData, StackVars0, StackVars),

    !.StackAlloc = stack_alloc(LiveSets0),
    LiveSets = set.insert(LiveSets0, StackVars),
    !:StackAlloc = stack_alloc(LiveSets).

:- pred alloc_at_recursive_call_for_loop_control(need_for_loop_control::in,
    alloc_data::in, stack_alloc::in, stack_alloc::out) is det.

alloc_at_recursive_call_for_loop_control(NeedLC, AllocData, !StackAlloc) :-
    NeedLC = need_for_loop_control(StackVarsSets),
    list.foldl(set_for_loop_control(AllocData), StackVarsSets, !StackAlloc).

:- pred set_for_loop_control(alloc_data::in, set_of_progvar::in,
    stack_alloc::in, stack_alloc::out) is det.

set_for_loop_control(AllocData, Set0, !StackAlloc) :-
    !.StackAlloc = stack_alloc(LiveSets0),
    filter_out_dummy_vars(AllocData, Set0, Set),
    LiveSets = set.insert(LiveSets0, Set),
    !:StackAlloc = stack_alloc(LiveSets).

:- pred filter_out_dummy_vars(alloc_data::in,
    set_of_progvar::in, set_of_progvar::out) is det.

filter_out_dummy_vars(AllocData, Vars, NonDummyVars) :-
    DummyVarArray = AllocData ^ ad_dummy_var_array,
    set_of_var.filter(var_is_not_dummy(DummyVarArray), Vars, NonDummyVars).

:- pred var_is_not_dummy(array(is_dummy_type)::in, prog_var::in) is semidet.

var_is_not_dummy(DummyVarArray, Var) :-
    VarNum = to_int(Var),
    array.lookup(DummyVarArray, VarNum, IsDummy),
    IsDummy = is_not_dummy_type.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred allocate_stack_slots(list(set_of_progvar)::in, globals::in,
    main_stack::in, vartypes::in, int::in, maybe(pair(prog_var, int))::in,
    stack_slots::out) is det.

allocate_stack_slots(ColourList, Globals, MainStack, VarTypes,
        NumReservedSlots, MaybeReservedVarInfo, StackSlots) :-
    % The reserved slots are referred to by fixed number
    % (e.g. framevar(1)) in trace.setup.
    FirstVarSlot = NumReservedSlots + 1,
    allocate_stack_slots_2(ColourList, Globals, MainStack, VarTypes,
        MaybeReservedVarInfo, FirstVarSlot, map.init, StackSlots).

:- pred allocate_stack_slots_2(list(set_of_progvar)::in, globals::in,
    main_stack::in, vartypes::in, maybe(pair(prog_var, int))::in,
    int::in, stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_2([], _, _, _, _, _, !StackSlots).
allocate_stack_slots_2([Vars | VarSets], Globals, MainStack, VarTypes,
        MaybeReservedVarInfo, N0, !StackSlots) :-
    ( float_width_on_stack(Globals, MainStack) = double_width ->
        set_of_var.divide(var_is_float(VarTypes), Vars,
            DoubleWidthVars, SingleWidthVars)
    ;
        SingleWidthVars = Vars,
        DoubleWidthVars = set_of_var.init
    ),
    ( set_of_var.is_non_empty(SingleWidthVars) ->
        allocate_stack_slots_3(SingleWidthVars, MainStack, single_width,
            MaybeReservedVarInfo, N0, N1, !StackSlots)
    ;
        N1 = N0
    ),
    ( set_of_var.is_non_empty(DoubleWidthVars) ->
        % XXX We do NOT currently allow single-width vars to overlap with
        % double-width vars. The code generator does not understand that
        % clobbering one of the pair of slots is equivalent to clobbering
        % both of them.
        align_double_width_slots(N1, N2),
        allocate_stack_slots_3(DoubleWidthVars, MainStack, double_width,
            MaybeReservedVarInfo, N2, N, !StackSlots)
    ;
        N = N1
    ),
    allocate_stack_slots_2(VarSets, Globals, MainStack, VarTypes,
        MaybeReservedVarInfo, N, !StackSlots).

:- func float_width_on_stack(globals, main_stack) = stack_slot_width.

float_width_on_stack(Globals, Stack) = FloatWidth :-
    % We only store unboxed double-width floats on the det stack.
    % It would be possible to do on the nondet stack but we would probably
    % need to pad the frame allocation at run time to ensure that any double
    % variables in the frame will be at aligned memory addresses.
    (
        Stack = det_stack,
        double_width_floats_on_det_stack(Globals, yes)
    ->
        FloatWidth = double_width
    ;
        FloatWidth = single_width
    ).

    % Conform to memory alignment requirements for double-word values.
    % We maintain the invariant that the stack pointer is double-aligned.
    % The first stack variable is numbered 1 so all odd-numbered stack slots
    % are aligned. In a downwards-growing stack a higher slot number has a
    % lower address. When allocating two consecutive slots, we therefore want
    % the highered-numbered slot to be ODD. [N, N+1] shall be the next slots
    % to be allocated, therefore N must be EVEN and N+1 must be ODD.
    %
:- pred align_double_width_slots(int::in, int::out) is det.

align_double_width_slots(N0, N) :-
    ( int.even(N0) ->
        N = N0
    ;
        N = N0 + 1
    ).

:- pred var_is_float(vartypes::in, prog_var::in) is semidet.

var_is_float(VarTypes, Var) :-
    lookup_var_type(VarTypes, Var, float_type).

:- pred allocate_stack_slots_3(set_of_progvar::in, main_stack::in,
    stack_slot_width::in, maybe(pair(prog_var, int))::in, int::in, int::out,
    stack_slots::in, stack_slots::out) is det.

allocate_stack_slots_3(Vars, MainStack, StackSlotWidth, MaybeReservedVarInfo,
        !N, !StackSlots) :-
    (
        MaybeReservedVarInfo = yes(ResVar - ResSlotNum),
        set_of_var.member(Vars, ResVar)
    ->
        expect(unify(StackSlotWidth, single_width), $module, $pred,
            "reserved multiple stack slots"),
        SlotNum = ResSlotNum
    ;
        SlotNum = !.N,
        next_slot(StackSlotWidth, !N)
    ),
    (
        MainStack = det_stack,
        Locn = det_slot(SlotNum, StackSlotWidth)
    ;
        MainStack = nondet_stack,
        Locn = nondet_slot(SlotNum)
    ),
    VarList = set_of_var.to_sorted_list(Vars),
    allocate_same_stack_slot(VarList, Locn, !StackSlots).

:- pred next_slot(stack_slot_width::in, int::in, int::out) is det.

next_slot(single_width, N, N + 1).
next_slot(double_width, N, N + 2).

:- pred allocate_same_stack_slot(list(prog_var)::in, stack_slot::in,
    stack_slots::in, stack_slots::out) is det.

allocate_same_stack_slot([], _Slot, !StackSlots).
allocate_same_stack_slot([Var | Vars], Slot, !StackSlots) :-
    map.det_insert(Var, Slot, !StackSlots),
    allocate_same_stack_slot(Vars, Slot, !StackSlots).

    % We must not allocate the same stack slot to dummy variables. If we do,
    % then the code that saves variables on the stack at calls will get
    % confused. After saving one dummy variable on the stack, it will try
    % to save the next in the same stack slot; believing the first variable
    % to still be live, it will move it away.
    %
    % In ordinary grades, it is possible to have one value of type io.state
    % and another of type store.store live at the same time; in debugging
    % grades, due to our policy of extending variable lifetimes, more than
    % one io.state may be live at the same time.
    %
:- pred allocate_dummy_stack_slots(list(prog_var)::in, main_stack::in,
    int::in, stack_slots::in, stack_slots::out) is det.

allocate_dummy_stack_slots([], _, _, !StackSlots).
allocate_dummy_stack_slots([Var | Vars], MainStack, N0, !StackSlots) :-
    (
        MainStack = det_stack,
        Locn = det_slot(N0, single_width)
    ;
        MainStack = nondet_stack,
        Locn = nondet_slot(N0)
    ),
    allocate_same_stack_slot([Var], Locn, !StackSlots),
    allocate_dummy_stack_slots(Vars, MainStack, N0 - 1, !StackSlots).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.stack_alloc.
%-----------------------------------------------------------------------------%
