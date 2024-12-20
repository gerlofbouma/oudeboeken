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
    findall(Word1/His,find_variant(Chars,Word1,[],His),List0),
    sort(List0,List1),
    filter_list(List1,List2),
    report(List2,Chars).

%%% by default, only keep the candidate(s) with the fewest number of
%%% rule applications. In case of an equal number, use ~ to specify multiple
%%% alternatives
filter_list([kwadraat/_,quadraat/_],List) :-
    !,
    List = [kwadraat].
filter_list(Pairs0,List) :-
    add_l(Pairs0,Pairs1),
    sort(Pairs1,Pairs2),
    filter_longer(Pairs2,List).

add_l([],[]).
add_l([H|T],[NH|NT]) :-
    add_l1(H,NH),
    add_l(T,NT).

add_l1(Word/List0,Len-Word) :-
    sort(List0,List),  % NB really should not remove doubles, but probably never happens in crucial cases?
    length(List,Len).

filter_longer([],[]).
filter_longer([Len-W|T0],[W|T]) :-
    filter_longer(T0,Len,T).

filter_longer([],_,[]).
filter_longer([Len2-_|_],Len,T):-
    Len2 > Len,
    !,
    T = [].
filter_longer([Len-W|T0],Len,[W|T]):-
    filter_longer(T0,Len,T).


%report([],Chars) :-
%    format(user_error,"no results: ~s~n",[Chars]).
report([Word|Words],Chars):-
    report_alts(Words,Word,Chars).

%%% single result:
report_alts([],Word,Chars) :-
    format("~s ~w~n",[Chars,Word]).
%%% two results:  (so far, no cases of more than two have been observed)
report_alts([Word2|Words],Word1,Chars) :-
    format(user_error,"multiple results: ~s --> ~w~n",[Chars,[Word1,Word2|Words]]),
    format("~s ~~~w~~~w~n",[Chars,Word1,Word2]).

find_variant(Chars,Word1,Before,After) :-
    atom_codes(Word,Chars),
    \+ skip_word(Word),
    apply_spelling_rules(Chars,Chars1,Before,After),
    atom_codes(Word1,Chars1),
    alpino_lex:lexicon(_,_,[Word1],[],His),
    allowed_his(His,Before).

find_variant([Upper|Codes],Word1,Before,After) :-
    alpino_latin1:isupper(Upper),
    alpino_latin1:tolower(Upper,Lower), Upper \= Lower, % prevent loop for Čelinek
    find_variant([Lower|Codes],Word0,[cap|Before],After),
    atom_codes(Word0,[L|Codes0]),
    alpino_latin1:islower(L),
    alpino_latin1:toupper(L,U),
    atom_codes(Word1,[U|Codes0]).

skip_word(overheyt).
skip_word(ooverheit).
skip_word(weeder).
skip_word(weederom).
skip_word(ghehackt).  % en niet gehackt
skip_word(zoooo).
skip_word(zooght).
skip_word(gheschopt). % only geschopt, not gesopt

skip_word(quacken).   % geen idee of kwakken of kwaken of nog iets anders?
skip_word(quadraat).  % naam?
skip_word(quadraet).  % naam?
skip_word(lights).    % probably mostly in English quotes

skip_word(scheyt).
skip_word(eenen).     % moet een of ene worden
skip_word(eenigen).   % moet enige of enigen worden
skip_word(vorsch).    % moet ook vers en kikvors worden
skip_word(heeschen).  % want wordt ook hese
skip_word(hoogen).    % want wordt ook hoge
skip_word(grooten).   % want wordt ook grote
skip_word(asch).      % want als we er "as" van maken wordt dat vervolgens "als"
skip_word(zooeven).   % moet zoëven worden, niet zoeven

%% thanks Gerlof
% skip_word(schoepen).  is now captured elsewhere
% skip_word(schoort).   is now captured elsewhere
skip_word(onderschoort).
skip_word(geschel).
skip_word(geschelde).
skip_word(geschelt).
skip_word(geene).  % both geen and gene
skip_word(geenen). % both geen and genen

apply_spelling_rules(Chars0,Chars,Before,His) :-
    spelling_rule(Chars0,Chars1,Before,His0),
    apply_spelling_rules_(Chars1,Chars,His0,His), % output _His for debug purposes
    length(His,Len),
    (   Len > 10
    ->  format(user_error,"cycle detected: ~s ~s ~w~n",[Chars0,Chars,His]),
	halt
    ;   true
    ).

apply_spelling_rules_(Cs,Cs,H,H).
apply_spelling_rules_(Cs0,Cs,His0,His) :-
    spelling_rule(Cs0,Cs1,His0,His1),
    apply_spelling_rules_(Cs1,Cs,His1,His).

%% bellen
allowed_his(normal,_).
%% opbellen
allowed_his('part-V',_).
%% moois
allowed_his('Adj-s',[]).
%% bellend
%% opbellend
allowed_his('V-d'(His),B) :-
    allowed_his(His,B).
%% bellende
%% opbellende
allowed_his('V-de'(His),B) :-
    allowed_his(His,B).

%%% The first rule is for "de-doubling" vowels.
%%% In modern spelling, long vowels are written
%%% with a single vowel in open syllables.
%%% old: vaaren new: varen
%%%
%%% Problem of course is that we don't have access to syllable structure.
%%%
%%% This version uses a few frequent suffixes
%%% that are (almost) certainly indicative of
%%% open syllables:
%%% -Cig -Cen -Cer -Cing -Ce$ -lijk -Celijk
spelling_rule(Chars,Chars1,His,[eenig|His]):-
    append(Pref,[V,V,C|Rest],Chars),
    double_v(V),
    open_c(C),
    open_suffix(C,Rest),
    append(Pref,[V,C|Rest],Chars1).

%%% leeraar -> leraar
%%% only suffix -Caar gives too many false hits (teelaarde,wreedaard,kwaadaardig)
spelling_rule(Chars,Chars1,His,[leeraar|His]) :-
    append(Begin,[108,101,101,114,97,97,114|End],Chars),
    append(Begin,[108,101,114,97,97,114|End],Chars1).

%%% tooneel -> toneel
spelling_rule(Chars,Chars1,His,[tooneel|His]) :-
    append(Begin,[116,111,111,110,101,101,108|End],Chars),
    append(Begin,[116,111,110,101,101,108|End],Chars1).

%%% sch -> s
%%% this rule should not apply at the beginning of a word:
%%% schaamen  ->  *samen
%%% schaemen  ->  *samen
%%% schandael ->  *sandaal
%%% scheering ->  *sering
spelling_rule(Chars,Chars1,His,[schaamen|His]) :-
    append([H|Begin],[115,99,104|End],Chars), 
    append([H|Begin],[115|End],Chars1).

%%% y -> ij
spelling_rule(Chars,Chars1,His,[y|His]) :-
    append(Begin,[121|End],Chars),
    append(Begin,[105,106|End],Chars1).

%%% ae -> aa
spelling_rule(Chars,Chars1,His,[ae|His]) :-
    append(Begin,[97,101|End],Chars),
    append(Begin,[97,97|End],Chars1).

%%% qua -> kwa
spelling_rule(Chars,Chars1,His,[qua|His]) :-
    append(Begin,[113,117,97|End],Chars),
    append(Begin,[107,119,97|End],Chars1).

%%% ph -> f
spelling_rule(Chars,Chars1,His,[ph|His]) :-
    append(Begin,[112,104|End],Chars),
    append(Begin,[102|End],Chars1).

%%% gch -> ch
spelling_rule(Chars,Chars1,His,[gch|His]) :-
    append(Begin,[103,99,104|End],Chars),
    append(Begin,[99,104|End],Chars1).

%%% gt -> cht
spelling_rule(Chars,Chars1,His,[gt|His]) :-
    append(Begin,[103,116|End],Chars),
    append(Begin,[99,104,116|End],Chars1).

%%% ck -> k or kk
%%% bit of a mess, special cases in ck_rule below
spelling_rule(Chars,Chars1,His,[ck|His]):-
    append(Begin,[99,107|Rest],Chars),
    ck_rule(Rest,Begin,Chars1).

%%% uy -> ui
spelling_rule(Chars,Chars1,His,[uy|His]) :-
    \+ member(cap,His), % those are mostly names: Bruyne Ruyter Zuylen
    append(Begin,[117,121|End],Chars),
    append(Begin,[117,105|End],Chars1).

%%% ey -> ei
spelling_rule(Chars,Chars1,His,[ey|His]) :-
    \+ member(cap,His), % those are mostly names: Deyssel Weyerman
                        % but Leyden Majesteyt Heylige
    append(Begin,[101,121|End],Chars),
    append(Begin,[101,105|End],Chars1).

%%% aaij -> aai
spelling_rule(Chars,Chars1,His,[aaij|His]) :-
    append(Begin,[97,97,105,106|End],Chars),
    append(Begin,[97,97,105|End],Chars1).

%%% ooij -> ooi
spelling_rule(Chars,Chars1,His,[ooij|His]) :-
    append(Begin,[111,111,105,106|End],Chars),
    append(Begin,[111,111,105|End],Chars1).

%%% Coij -> Cooi
spelling_rule(Chars,Chars1,His,['Coij'|His]) :-
    append(Begin,[Cons,111,105,106|End],Chars),
    cons(Cons),
    append(Begin,[Cons,111,111,105|End],Chars1).

%%% oeij -> oei
spelling_rule(Chars,Chars1,His,[oeij|His]) :-
    append(Begin,[111,101,105,106|End],Chars),
    append(Begin,[111,101,105|End],Chars1).

%%% aauw -> auw
spelling_rule(Chars,Chars1,His,[aauw|His]) :-
    append(Begin,[97,97,117,119,101|End],Chars),
    append(Begin,[97,117,119,101|End],Chars1).

%%% weder -> weer
spelling_rule(Chars,Chars1,His,[weder|His]) :-
    append(Begin,[119,101,100,101,114|End],Chars),
    \+ member(Chars,["Wedert","Zweder"]),
    append(Begin,[119,101,101,114|End],Chars1).

%%% gh -> g
spelling_rule(Chars,Chars1,His,[gh|His]) :-
    append(Begin,[103,104|End],Chars),
    \+ member(Chars,["saghen"]),
    append(Begin,[103|End],Chars1).

%%% heit$ -> heid
spelling_rule(Chars,Chars1,His,[heit|His]):-
    append(Begin,"heit",Chars),
    append(Begin,"heid",Chars1).

%%% ^zoo -> zo
spelling_rule([122,111,111|Chars],[122,111|Chars],His,[zoo|His]) :-
    \+ member(Chars,["ght","gh","g"]).

%%% neder -> neer
spelling_rule(Chars,Chars1,His,[neder|His]):-
    append(Begin,[110,101,100,101,114|End],Chars),
    append(Begin,[110,101,101,114|End],Chars1).

%%% lik -> lijk (in sommige contexten)
spelling_rule(Chars,Chars1,His,[lik|His]) :-
    append([B1,B2|Begin],[108,105,107|Rest],Chars),
    lik_rest(Rest),
    append([B1,B2|Begin],[108,105,106,107|Rest],Chars1).

%%% elijk -> lijk (in sommige contexten)
%%% 'lijk -> lijk
spelling_rule(Chars,Chars1,His,[moeielijk|His]) :-
    \+ member(vorstlijk,His), % no cycles
    append([B1,B2|Begin],[QE,108,105,106,107|Rest],Chars),
    lists:member(QE,[101,39]),
    lik_rest(Rest),
    append([B1,B2|Begin],[108,105,106,107|Rest],Chars1).

%%% lijk -> elijk (in sommige contexten)
spelling_rule(Chars,Chars1,His,[vorstlijk|His]) :-
    \+ member(moeielijk,His),	% no cycles with the other
    append([B1|Begin],[B2,108,105,106,107|Rest],Chars),
    \+ vow(B2),                 % not if there already is an e/a/
    lik_rest(Rest),
    append([B1|Begin],[B2,101,108,105,106,107|Rest],Chars1).

%%% ieele -> iële
spelling_rule(Chars,Chars1,His,[ieele|His]) :-
    append(Begin,[105,101,101,108,101|End],Chars),
    append(Begin,[105,235,108,101|End],Chars1).

%%% en[dt]lijk -> enlijk
spelling_rule(Chars,Chars1,His,[gezamendlijk|His]) :-
    append(Begin,[101,110,DT,108,105,106,107|End],Chars),
    member(DT,[100,116]),
    append(Begin,[101,110,108,105,106,107|End],Chars1).

%%% [ns][dt][lr]en -> [ns][dt]e[lr]en
spelling_rule(Chars,Chars1,His,[handlen|His]) :-
    append(Begin,[NS,DT,LR,101,110|End],Chars),
    ns(NS),
    lr(LR),
    dt(DT),
    append(Begin,[NS,DT,101,LR,101,110|End],Chars1).

%%% ^saam -> samen
spelling_rule(Chars,Chars1,His,[saam|His]):-
    append("saam",End,Chars),
    append("samen",End,Chars1).

%%% ^zamen -> samen
spelling_rule(Chars,Chars1,His,[zamen|His]):-
    append("zamen",End,Chars),
    append("samen",End,Chars1).

%%% ^sw -> zw
spelling_rule(Chars,Chars1,His,[sw|His]):-
    append("sw",End,Chars),
    append("zw",End,Chars1).

%%% dt$ -> d
%%% generates (too?) many ambiguous results in case of verbs
%%% rydt -> rijd rijdt
%%%spelling_rule(Chars,Chars1,His,[dt|His]):-
%%%    append(Begin,"dt",Chars),
%%%    append(Begin,"d",Chars1).

%%% things to try
%%% nt$ -> nd

dt(100).
dt(116).

lr(108).
lr(114).

ns(110).
ns(115).

double_v(97).   % a
double_v(101).  % e
double_v(111).  % o
double_v(117).  % u

vow(97).        % a
vow(101).       % e
vow(105).       % i
vow(111).       % o
vow(117).       % u

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

cons(98).
cons(99).
cons(100).
cons(102).
cons(103).
cons(104).
cons(106).
cons(107).
cons(108).
cons(109).
cons(110).
cons(112).
cons(113).
cons(114).
cons(115).
cons(116).
cons(118).
cons(119).
cons(120).
cons(122).

open_suffix(C,[N1|Rest]) :-                    % Cig Cing Cen Cer Celijk Ce$
    open_c(C),
    open_suffix_after_c(N1,Rest).
open_suffix(108,[105,106,107|_]).              % lijk

open_suffix_after_c(105,[103|_]).              % ig
open_suffix_after_c(105,[110,103|_]).          % ing
open_suffix_after_c(101,[110|_]).              % en
open_suffix_after_c(101,[114|_]).              % er
open_suffix_after_c(101,[108,105,106,107|_]).  % elijk
open_suffix_after_c(101,[]).                   % e$

%%% 1. ck$ -> k (ick,oock,druck)
ck_rule([],Begin,Result) :-
    append(Begin,"k",Result).
%%% 2. ckC -> k (maeckte,rijckdom)
ck_rule([H|T],Begin,Result) :-
    cons(H),
    append(Begin,[107,H|T],Result).
%%% 3. VVckV -> k (maecken, boecken, spraecke)
ck_rule([V3|T],Begin,Result) :-
    vow(V3),
    append(_,[V1,V2],Begin),
    vow(V1),
    vow(V2),
    append(Begin,[107,V3|T],Result).
%%% 4. Cck -> k (welcken, sulcke, wercken)
ck_rule([H|T],Begin,Result) :-
    append(_,[C],Begin),
    cons(C),
    append(Begin,[107,H|T],Result).
%%% 5. CVckV -> kk (getrocken, vertrecken, tacken)
ck_rule([V3|T],Begin,Result) :-
    vow(V3),
    append(_,[C,V1],Begin),
    cons(C),
    vow(V1),
    append(Begin,[107,107,V3|T],Result).

lik_rest("").
lik_rest("e").
lik_rest("en").
lik_rest("er").
lik_rest("s").
lik_rest("heid").
lik_rest("heden").
lik_rest("t").
lik_rest("te").
lik_rest("ten").
lik_rest("end").
lik_rest("ing").
lik_rest("ingen").
lik_rest("st").
lik_rest("ste").

