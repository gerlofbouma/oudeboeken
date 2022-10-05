:- use_module(o).

adj :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	check_adj(Chars),
	fail
    ),
    !.

check_adj(Hoogen) :-
    lists:append(Hooge,"n",Hoogen),
    apply_spelling_rules_(Hooge,Hoge), % zero or more rules
    atom_codes(HogeAtom,Hoge),
    alpino_lex:lexicon(Cat,_,[HogeAtom],[],His),
    allowed_cat(Cat),
    allowed_adj_his(His),
    format("~s ~s~n",[Hoogen,Hoge]).
    
allowed_cat(adjective(ende(_))).
allowed_cat(adjective(e)).
allowed_cat(adjective(ge_e)).
allowed_cat(adjective(ere)).
allowed_cat(adjective(ste)).
allowed_cat(number(rang)).
allowed_cat(nominalized_adjective).
allowed_cat(determiner(pron)).
allowed_cat(adjective(stof)).

allowed_adj_his(normal).
allowed_adj_his('V-de'(_)).
allowed_adj_his(number_rang).
