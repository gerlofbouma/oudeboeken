:- use_module(generate_alt_spelling).

go_nouns :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	check_noun(Chars),
	fail
    ),
    !.

check_noun(Oogenblik) :-
    apply_spelling_rules_(Oogenblik,Ogenblik,[],_), % zero or more rules
    atom_codes(Atom,Ogenblik),
    alpino_lex:lexicon(Cat,_,[Atom],[],_),
    allowed_cat(Cat),
    \+ (  alpino_lex:lexicon(Cat,_,[Atom],[],_),
	   forbidden_cat(Cat)
       ),
    format("~s~n",[Oogenblik]).
    
allowed_cat(noun(_,_,_)).
allowed_cat(tmp_noun(_,_,_)).

forbidden_cat(verb(_,_,_)).
forbidden_cat(er_wh_loc_adverb).
