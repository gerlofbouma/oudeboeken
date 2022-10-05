:- use_module(library(charsio)).


go :-
    repeat,
    read_line(Chars),
    (   Chars == end_of_file
    ->  true
    ;	check_spelling(Chars),
	fail
    ),
    !.


check_spelling(Chars) :-
    findall(Word1,find_variant(Chars,Word1),List0),
    sort(List0,[Word|Words]),    
    (   Words == []
    ->  format("~s ~w~n",[Chars,Word])
    ;   format(user_error,"multiple results: ~w~n",[Words])	
    ).


find_variant(Chars,Word1) :-
%%%    format(user_error,"~s~n",[Chars]),
    \+ skip_word(Chars),
    apply_spelling_rules(Chars,Chars1),
    atom_codes(Word1,Chars1),
    alpino_lex:lexicon(_,_,[Word1],[],His),
    allowed_his(His).

find_variant([Upper|Codes],Word1) :-
    alpino_latin1:isupper(Upper),
    alpino_latin1:tolower(Upper,Lower), Upper \= Lower, % prevent loop for Čelinek
    find_variant([Lower|Codes],Word0),
    atom_codes(Word0,[L|Codes0]),
    alpino_latin1:islower(L),
    alpino_latin1:toupper(L,U),
    atom_codes(Word1,[U|Codes0]).

skip_word("vorsch").    % moet ook vers en kikvors worden
skip_word("heeschen").  % want wordt ook hese
skip_word("hoogen").    % want wordt ook hoge
skip_word("grooten").   % want wordt ook grote
skip_word("asch").      % want als we er "as" van maken wordt dat vervolgens "als"
skip_word("zooeven").   % moet zoëven worden, niet zoeven

apply_spelling_rules(Chars0,Chars) :-
    spelling_rule(Chars0,Chars1),
    apply_spelling_rules_(Chars1,Chars).

apply_spelling_rules_(Cs,Cs).
apply_spelling_rules_(Cs0,Cs) :-
    spelling_rule(Cs0,Cs1),
    apply_spelling_rules_(Cs1,Cs).

allowed_his(normal).
allowed_his('part-V').
allowed_his('V-d'(normal)).

%%% 105: i
%%% 103: g
%%%  leenig -> lenig
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,105,103],
    Suffix1=   [V,C,105,103],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 105: i
%%% 103: g
%%%  leenige -> lenige
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,105,103,101],
    Suffix1=   [V,C,105,103,101],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%%  beenen -> benen
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101,110],
    Suffix1=   [V,C,101,110],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 114: r
%%%  grootere -> grotere
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101,114,101],
    Suffix1=   [V,C,101,114,101],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 114: r
%%%  grooter -> groter
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101,114],
    Suffix1=   [V,C,101,114],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%% 116: t
%%%  reekent -> rekent
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101,110,116],
    Suffix1=   [V,C,101,110,116],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%% 100: d
%%%  streelend -> strelend
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101,110,100],
    Suffix1=   [V,C,101,110,100],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%% 100: d
%%%  streelende -> strelende
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101,110,100,101],
    Suffix1=   [V,C,101,110,100,101],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%%  belooning -> beloning
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,105,110,103],
    Suffix1=   [V,C,105,110,103],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%%  belooningen -> beloningen
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,105,110,103,101,110],
    Suffix1=   [V,C,105,110,103,101,110],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% geene -> gene
spelling_rule(Chars,Chars1) :-
    Suffix = [V,V,C,101],
    Suffix1=   [V,C,101],
    double_v(V),
    open_c(C),
    lists:append(Pref,Suffix,Chars),
    lists:append(Pref,Suffix1,Chars1).

%%% sch -> s
spelling_rule(Chars,Chars1) :-
    append(Begin,End,Chars),
    append("sch",Rest,End),
    append("s",Rest,End1),
    append(Begin,End1,Chars1).

%%% y -> ij
spelling_rule(Chars,Chars1) :-
    append(Begin,End,Chars),
    append("y",Rest,End),
    append("ij",Rest,End1),
    append(Begin,End1,Chars1).

%%% ae -> aa
spelling_rule(Chars,Chars1) :-
    append(Begin,End,Chars),
    append("ae",Rest,End),
    append("aa",Rest,End1),
    append(Begin,End1,Chars1).

%%% oolijk -> olijk
spelling_rule(Chars,Chars1) :-
    append(Begin,End,Chars),
    append("lijk",_Rest,End),
    append(Vr,[V,V],Begin),
    double_v(V),
    append(Vr,[V],Begin2),
    append(Begin2,End,Chars1).

%%% gch -> ch
spelling_rule(Chars,Chars1) :-
    append(Begin,End,Chars),
    append("gch",Rest,End),
    append("ch",Rest,End2),
    append(Begin,End2,Chars1).

%%% zoo -> zo (aan het begin)
spelling_rule(Chars,Chars1) :-
    append("zoo",Rest,Chars),
    append("zo",Rest,Chars1).

double_v(97).   % a
double_v(101).  % e
double_v(111).  % o
double_v(117).  % u

open_c(100).    % d
open_c(103).    % g
open_c(107).    % k
open_c(108).    % l
open_c(109).    % m
open_c(110).    % n
open_c(112).    % p
open_c(114).    % r
open_c(115).    % s  Europeesche -> Europeese -> Europese
open_c(116).    % t
open_c(118).    % v  gelooven -> geloven
open_c(122).    % z
