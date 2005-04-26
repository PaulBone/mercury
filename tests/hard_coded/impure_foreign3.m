% Test promise_impure_implicit in DCGs.

:- module impure_foreign3.

:- interface.

:- import_module io.

:- impure pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require.

main -->
	promise_impure_implicit (
			% Inlining this call forces recomputation
			% of purity for main/2 (because of the
			% `:- pragma promise_pure').
			% In some versions of this compiler, this
			% recomputation would erroneously infer
			% that the inlined calls to incr/1 below
			% were `pure'. Duplicate call elimination
			% would then remove all but one of them.
		{ unsafe_get(Val0) },

		io__write_int(Val0),
		io__nl,

		{ impure incr(_) },
		{ impure incr(_) },
		{ impure incr(_) },
		{ semipure get(Val) },
		io__write_int(Val),
		io__nl
	).

:- pragma foreign_decl("C",
"
	int counter;
").

:- pragma foreign_code("C",
"
	int counter = 1;
").

:- pragma foreign_code("C#", "static int counter = 1;").

:- impure pred incr(int::out) is det.

incr(_::out) :- error("incr/1 called for language other than C").

:- pragma foreign_proc("C", incr(Val::out), [will_not_call_mercury],
			"counter++; Val = counter;").
:- pragma foreign_proc("C#", incr(Val::out), [will_not_call_mercury],
			"counter++; Val = counter;").

:- semipure pred get(int::out) is det.

get(_::out) :- error("get/1 called for language other than C").

:- pragma foreign_proc("C", get(Val::out),
		[will_not_call_mercury, promise_semipure],
		"Val = counter").
:- pragma foreign_proc("C#", get(Val::out),
		[will_not_call_mercury, promise_semipure],
		"Val = counter;").
	
:- pred unsafe_get(int::out) is det.
:- pragma promise_pure(unsafe_get/1).

unsafe_get(X) :- semipure get(X).
