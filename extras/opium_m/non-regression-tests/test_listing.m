%------------------------------------------------------------------------------%
% Copyright (C) 1999 INRIA/INSA.
%
% Auteur : Erwan Jahier <jahier@irisa.fr>
%
% This file is just intended to test the listing Mercury program.
% (listing test_listing test)

:- pred test(term, string).		    %pred
:- mode test(in, out) is det.		    %pred_mode
:- func test(string) = int.		    %func
:- mode test(out) = in is det.		    %func_mode
:- pred test(term::in, string::out) is det. %pred_and_mode
:- func test(string::in) = int::out is det. %func_and_mode
test(X, V) :-
	p(X,V).
test(X, V) -->
	p(X,V).
test(1,2,3).
test(4) = 4.
test = 4.
:- pragma c_code(test(S1::in, S2::in, S3::in, Foo::in),
		[will_not_call_mercury, thread_safe], "{
	size_t len_1 = strlen(S1);
	SUCCESS_INDICATOR = (
		strncmp(S1, S3, len_1) == 0 &&
		strcmp(S2, S3 + len_1) == 0
	);
}").
:- type  test
	--->	pair_of_lines(int, int).
:- type test(T)
	--->	no
	;	yes(T).
test(Bar) = Output :-
	test(Bar, Output).
test(Bar, Bar1, Bar2, 
	Bar3, Bar4) = Output -->
	test(Bar, Output).
test(Bar, Bar2) = Output.

