%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% string_switch.m

% For switches on strings, we generate a hash table using open addressing
% to resolve hash conflicts.

% Author: fjh.

%-----------------------------------------------------------------------------%

:- module ll_backend__string_switch.

:- interface.

:- import_module backend_libs__code_model.
:- import_module backend_libs__switch_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_goal.
:- import_module ll_backend__code_info.
:- import_module ll_backend__llds.
:- import_module parse_tree__prog_data.

:- pred string_switch__generate(cases_list, prog_var, code_model,
	can_fail, hlds_goal_info, label, branch_end, branch_end, code_tree,
	code_info, code_info).
:- mode string_switch__generate(in, in, in, in, in, in, in, out, out, in, out)
	is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__builtin_ops.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_llds.
:- import_module libs__tree.
:- import_module ll_backend__code_gen.
:- import_module ll_backend__trace.

:- import_module bool, int, string, list, map, std_util, assoc_list, require.

string_switch__generate(Cases, Var, CodeModel, _CanFail, SwitchGoalInfo,
		EndLabel, MaybeEnd0, MaybeEnd, Code, !CodeInfo) :-
	code_info__produce_variable(Var, VarCode, VarRval, !CodeInfo),
	code_info__acquire_reg(r, SlotReg, !CodeInfo),
	code_info__acquire_reg(r, StringReg, !CodeInfo),
	code_info__get_next_label(LoopLabel, !CodeInfo),
	code_info__get_next_label(FailLabel, !CodeInfo),
	code_info__get_next_label(JumpLabel, !CodeInfo),

	% Determine how big to make the hash table.
	% Currently we round the number of cases up to the nearest
	% power of two, and then double it.  This should hopefully
	% ensure that we don't get too many hash collisions.
	%
	list__length(Cases, NumCases),
	int__log2(NumCases, LogNumCases),
	int__pow(2, LogNumCases, RoundedNumCases),
	TableSize = 2 * RoundedNumCases,
	HashMask = TableSize - 1,

	% Compute the hash table
	%
	switch_util__string_hash_cases(Cases, HashMask, HashValsMap),
	map__to_assoc_list(HashValsMap, HashValsList),
	switch_util__calc_hash_slots(HashValsList, HashValsMap,
		HashSlotsMap),

		% Note that it is safe to release the registers now,
		% even though we haven't yet generated all the code
		% which uses them, because that code will be executed
		% before the code for the cases (which might reuse those
		% registers), and because that code is generated manually
		% (below) so we don't need the reg info to be valid when
		% we generate it.
	code_info__release_reg(SlotReg, !CodeInfo),
	code_info__release_reg(StringReg, !CodeInfo),

		% Generate the code for when the hash lookup fails.
		% This must be done before gen_hash_slots, since
		% we want to use the exprn_info corresponding to
		% the start of the switch, not to the end of the last case.
	code_info__generate_failure(FailCode, !CodeInfo),

		% Generate the code etc. for the hash table
		%
	string_switch__gen_hash_slots(0, TableSize, HashSlotsMap, CodeModel,
		SwitchGoalInfo, FailLabel, EndLabel, MaybeEnd0, MaybeEnd,
		Strings, Labels, NextSlots, SlotsCode, !CodeInfo),

		% Generate code which does the hash table lookup
	(
		add_static_cell_natural_types(NextSlots, NextSlotsTableAddr,
			!CodeInfo),
		NextSlotsTable = const(data_addr_const(NextSlotsTableAddr)),
		add_static_cell_natural_types(Strings, StringTableAddr,
			!CodeInfo),
		StringTable = const(data_addr_const(StringTableAddr)),
		HashLookupCode = node([
			comment("hashed string switch") -
			  "",
			assign(SlotReg, binop(&, unop(hash_string, VarRval),
						const(int_const(HashMask)))) -
			  "compute the hash value of the input string",
			label(LoopLabel) -
			  "begin hash chain loop",
			assign(StringReg, binop(array_index(elem_type_string),
					StringTable, lval(SlotReg))) -
			  "lookup the string for this hash slot",
			if_val(binop(and, lval(StringReg),
				binop(str_eq, lval(StringReg), VarRval)),
					label(JumpLabel)) -
			  "did we find a match?",
			assign(SlotReg, binop(array_index(elem_type_int),
					NextSlotsTable, lval(SlotReg))) -
			  "not yet, so get next slot in hash chain",
			if_val(binop(>=, lval(SlotReg), const(int_const(0))),
				label(LoopLabel)) -
			  "keep searching until we reach the end of the chain",
			label(FailLabel) -
			  "no match, so fail"
		])
	),
	JumpCode = node([
		label(JumpLabel) -
			"we found a match",
		computed_goto(lval(SlotReg), Labels) -
			"jump to the corresponding code"
	]),
		% Collect all the generated code fragments together
	Code =
		tree(VarCode,
		tree(HashLookupCode,
		tree(FailCode,
		tree(JumpCode,
		     SlotsCode)))).

:- pred string_switch__gen_hash_slots(int, int, map(int, hash_slot),
	code_model, hlds_goal_info, label, label, branch_end, branch_end,
	list(rval), list(label), list(rval), code_tree, code_info, code_info).
:- mode string_switch__gen_hash_slots(in, in, in, in, in, in, in,
	in, out, out, out, out, out, in, out) is det.

string_switch__gen_hash_slots(Slot, TableSize, HashSlotMap, CodeModel,
		SwitchGoalInfo, FailLabel, EndLabel, MaybeEnd0, MaybeEnd,
		Strings, Labels, NextSlots, Code) -->
	( { Slot = TableSize } ->
		{
			MaybeEnd = MaybeEnd0,
			Strings = [],
			Labels = [],
			NextSlots = [],
			Code = node([
				label(EndLabel) - "end of hashed string switch"
			])
		}
	;
		string_switch__gen_hash_slot(Slot, TableSize, HashSlotMap,
			CodeModel, SwitchGoalInfo, FailLabel, EndLabel,
			MaybeEnd0, MaybeEnd1,
			String, Label, NextSlot, SlotCode),
		{ Slot1 = Slot + 1 },
		{ 
			Strings = [String | Strings0],
			Labels = [Label | Labels0],
			NextSlots = [NextSlot | NextSlots0],
			Code = tree(SlotCode, Code0)
		},
		string_switch__gen_hash_slots(Slot1, TableSize, HashSlotMap,
			CodeModel, SwitchGoalInfo, FailLabel, EndLabel,
			MaybeEnd1, MaybeEnd,
			Strings0, Labels0, NextSlots0, Code0)
	).

:- pred string_switch__gen_hash_slot(int, int, map(int, hash_slot),
	code_model, hlds_goal_info, label, label, branch_end, branch_end,
	rval, label, rval, code_tree,
	code_info, code_info).
:- mode string_switch__gen_hash_slot(in, in, in, in, in, in, in,
	in, out, out, out, out, out, in, out) is det.

string_switch__gen_hash_slot(Slot, TblSize, HashSlotMap, CodeModel,
		SwitchGoalInfo, FailLabel, EndLabel, MaybeEnd0, MaybeEnd,
		StringRval, Label, NextSlotRval, Code) -->
	( { map__search(HashSlotMap, Slot, hash_slot(Case, Next)) } ->
		{ NextSlotRval = const(int_const(Next)) },
		{ Case = case(_, ConsTag, _, Goal) },
		{ ConsTag = string_constant(String0) ->
			String = String0
		;
			error("string_switch__gen_hash_slots: string expected")
		},
		{ StringRval = const(string_const(String)) },
		code_info__get_next_label(Label),
		{ string__append_list(["case """, String, """"], Comment) },
		{ LabelCode = node([
			label(Label) - Comment
		]) },
		code_info__remember_position(BranchStart),
		trace__maybe_generate_internal_event_code(Goal, SwitchGoalInfo,
			TraceCode),
		code_gen__generate_goal(CodeModel, Goal, GoalCode),
		{ goal_info_get_store_map(SwitchGoalInfo, StoreMap) },
		code_info__generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd,
			SaveCode),
		(
			{ string_switch__this_is_last_case(Slot, TblSize,
				HashSlotMap) }
		->
			[]
		;
			code_info__reset_to_position(BranchStart)
		),
		{ FinishCode = node([
			goto(label(EndLabel)) - "jump to end of switch"
		]) },
		{ Code =
			tree(LabelCode,
			tree(TraceCode,
			tree(GoalCode,
			tree(SaveCode,
			     FinishCode))))
		}
	;
		{ MaybeEnd = MaybeEnd0 },
		{ StringRval = const(int_const(0)) },
		{ Label = FailLabel },
		{ NextSlotRval = const(int_const(-2)) },
		{ Code = empty }
	).

:- pred string_switch__this_is_last_case(int, int, map(int, hash_slot)).
:- mode string_switch__this_is_last_case(in, in, in) is semidet.

string_switch__this_is_last_case(Slot, TableSize, Table) :-
	Slot1 = Slot + 1,
	( Slot1 >= TableSize ->
		true
	;
		\+ map__contains(Table, Slot1),
		string_switch__this_is_last_case(Slot1, TableSize, Table)
	).

%-----------------------------------------------------------------------------%
