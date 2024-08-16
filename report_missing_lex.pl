:- use_module(library(charsio)).


go :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	atom_codes(Atom,Chars),
	report_missing_lex(Atom),
	fail
    ),
    !.

%% het
report_missing_lex(Atom) :-
    lex_lexicon(_,_,[Atom],[],_),
    !,
    fail.
%% Het
report_missing_lex(AtomCap) :-
    alpino_unknowns:decap_first(AtomCap,Atom),
    lex_lexicon(_,_,[Atom],[],_),
    !,
    fail.
%% HET
report_missing_lex(AtomCap) :-
    alpino_unknowns:only_capitals(AtomCap,Atom),
    lex_lexicon(_,_,[Atom],[],_),
    !,
    fail.
%% AMSTERDAM
report_missing_lex(AtomCap) :-
    alpino_unknowns:only_capitals_but_one(AtomCap,Atom),
    lex_lexicon(_,_,[Atom],[],_),
    !,
    fail.
report_missing_lex(Atom) :-
    format("unknown: ~w~n",[Atom]).
