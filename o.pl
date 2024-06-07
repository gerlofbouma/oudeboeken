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
    findall(Word1,find_variant(Chars,Word1,[]),List0),
    sort(List0,[Word|Words]),    
    (   Words == []
    ->  format("~s ~w~n",[Chars,Word])
    ;   format(user_error,"multiple results: ~s --> ~w~n",[Chars,[Word|Words]])	
    ).


find_variant(Chars,Word1,Before) :-
%%%    format(user_error,"~s~n",[Chars]),
    \+ skip_word(Chars),
    apply_spelling_rules(Chars,Chars1,Before),
    atom_codes(Word1,Chars1),
    alpino_lex:lexicon(_,_,[Word1],[],His),
    allowed_his(His).

find_variant([Upper|Codes],Word1,Before) :-
    alpino_latin1:isupper(Upper),
    alpino_latin1:tolower(Upper,Lower), Upper \= Lower, % prevent loop for Čelinek
    find_variant([Lower|Codes],Word0,[cap|Before]),
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

%% thanks Gerlof
% skip_word("schoepen").  is now captured elsewhere
% skip_word("schoort").   is now captured elsewhere
skip_word("onderschoort").
skip_word("geschel").
skip_word("geschelde").
skip_word("geschelt").
skip_word("geene").  % both geen and gene
skip_word("geenen"). % both geen and genen

apply_spelling_rules(Chars0,Chars,Before) :-
    spelling_rule(Chars0,Chars1,Before,His0),
    apply_spelling_rules_(Chars1,Chars,His0,_His).

apply_spelling_rules_(Cs,Cs,H,H).
apply_spelling_rules_(Cs0,Cs,His0,His) :-
    spelling_rule(Cs0,Cs1,His0,His1),
    apply_spelling_rules_(Cs1,Cs,His1,His).

allowed_his(normal).
allowed_his('part-V').
allowed_his('V-d'(normal)).

%%% 105: i
%%% 103: g
%%%  leenig -> lenig
spelling_rule(Chars,Chars1,His,[leenig|His]) :-
    Suffix = [V,V,C,105,103],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,105,103],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 105: i
%%% 103: g
%%%  leenige -> lenige
spelling_rule(Chars,Chars1,His,[leenige|His]) :-
    Suffix = [V,V,C,105,103,101],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,105,103,101],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%%  beenen -> benen
spelling_rule(Chars,Chars1,His,[beenen|His]) :-
    Suffix = [V,V,C,101,110],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101,110],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 114: r
%%%  grootere -> grotere
spelling_rule(Chars,Chars1,His,[grootere|His]) :-
    Suffix = [V,V,C,101,114,101],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101,114,101],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 114: r
%%%  grooter -> groter
spelling_rule(Chars,Chars1,His,[grooter|His]) :-
    Suffix = [V,V,C,101,114],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101,114],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%% 116: t
%%%  reekent -> rekent
spelling_rule(Chars,Chars1,His,[reekent|His]) :-
    Suffix = [V,V,C,101,110,116],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101,110,116],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%% 100: d
%%%  streelend -> strelend
spelling_rule(Chars,Chars1,His,[streelend|His]) :-
    Suffix = [V,V,C,101,110,100],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101,110,100],
    append(Pref,Suffix1,Chars1).

%%% 101: e
%%% 110: n
%%% 100: d
%%%  streelende -> strelende
spelling_rule(Chars,Chars1,His,[streelende|His]) :-
    Suffix = [V,V,C,101,110,100,101],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101,110,100,101],
    append(Pref,Suffix1,Chars1).

%%%  belooning -> beloning
spelling_rule(Chars,Chars1,His,[belooning|His]) :-
    Suffix = [V,V,C,105,110,103],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,105,110,103],
    append(Pref,Suffix1,Chars1).

%%%  belooningen -> beloningen
spelling_rule(Chars,Chars1,His,[belooningen|His]) :-
    Suffix = [V,V,C,105,110,103,101,110],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,105,110,103,101,110],
    append(Pref,Suffix1,Chars1).

%%% geene -> gene
spelling_rule(Chars,Chars1,His,[geene|His]) :-
    Suffix = [V,V,C,101],
    append(Pref,Suffix,Chars),
    double_v(V),
    open_c(C),
    Suffix1 = [V,C,101],
    append(Pref,Suffix1,Chars1).

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

%%% oolijk -> olijk
spelling_rule(Chars,Chars1,His,[oolijk|His]) :-
    append(Begin,[108,105,106,107|End],Chars),
    append(Vr,[V,V],Begin),
    double_v(V),
    append(Vr,[V],Begin2),
    append(Begin2,[108,105,106,107|End],Chars1).

%%% gch -> ch
spelling_rule(Chars,Chars1,His,[gch|His]) :-
    append(Begin,[103,99,104|End],Chars),
    append(Begin,[99,104|End],Chars1).

%%% ck -> k
spelling_rule(Chars,Chars1,His,[ck|His]) :-
    append(Begin,[99,107|End],Chars),
    append(Begin,[107|End],Chars1).

%%% uy -> ui
spelling_rule(Chars,Chars1,His,[uy|His]) :-
    \+ member(cap,His), % those are mostly (also) names: Bruyne Ruyter Zuylen
    append(Begin,[117,121|End],Chars),
    append(Begin,[117,105|End],Chars1).

%%% ey -> ei
spelling_rule(Chars,Chars1,His,[uy|His]) :-
    \+ member(cap,His), % those are mostly (also) names: Bruyne Ruyter Zuylen
    append(Begin,[101,121|End],Chars),
    append(Begin,[101,105|End],Chars1).

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

spelling_rule(Chars,Chars1,His,[heit|His]):-
    append(Begin,"heit",Chars),
    append(Begin,"heid",Chars1).

%%% zoo -> zo (aan het begin)
spelling_rule([122,111,111|Chars],[122,111|Chars],His,[zoo|His]) :-
    \+ member(Chars,["gh","g"]).

%%% lik -> lijk (aan het eind)
spelling_rule(Chars,Chars1,His,[lik|His]) :-
    \+ member(Chars,["lik","Lik","blik","Blik","slik","Slik"]),
    append(Begin,"lik",Chars),
    append(Begin,"lijk",Chars1).

%%% like -> lijke (aan het eind)
spelling_rule(Chars,Chars1,His,[like|His]) :-
    append(Begin,"like",Chars),
    append(Begin,"lijke",Chars1).

%%% liks -> lijks (aan het eind)
spelling_rule(Chars,Chars1,His,[liks|His]) :-
    append(Begin,"liks",Chars),
    append(Begin,"lijks",Chars1).

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
