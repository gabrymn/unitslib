:- consult('symbols.pl').

is_base_si_unit(S) :- unit(S, _, _, base, _).

is_si_unit(S) :- is_base_si_unit(S).
is_si_unit(S) :- unit(S, _, _, derived, _).

si_unit_name(S, N) :-
	unit(S, N, _, _, _).

si_unit_symbol(N, S) :-
	unit(S, N, _, _, _).

si_unit_base_expansion(S, Expansion) :-
	unit(S, _, DerExp, _, _),
	only_base(DerExp, BaseExp),
	norm(BaseExp, Expansion).

only_base(X, Y) :-
	unit(X, _, H, derived, _),
	only_base(H, Y).

only_base(X, X) :-
	unit(X, _, X, base, _).

only_base(X * Y, Z * K) :-
	only_base(X, Z),
	only_base(Y, K).

only_base(X ** N, Z ** N) :-
	only_base(X, Z),
	number(N).
	 
is_dimension(D) :-
	is_si_unit(D),
	!.

is_dimension(X * Y) :-
	is_dimension(X),
	is_dimension(Y).

is_dimension(X ** N) :-
	is_dimension(X),
	number(N).

%%% nel caso di divisione tra due quantità otteniamo sempre 
%%% una quantità la cui unità di misura però è adimensionale, 
%%% quindi è corretto definire questo caso base	
is_quantity(q(N, 1)) :- number(N).

is_quantity(q(N, D)) :- number(N), is_dimension(D).


norm(1, 1) :- !.

%%% caso base: unità adimensionale elevata a qualsiasi numero rimane
%%% un'unità adimensionale (1 ** N => 1
norm(1 ** _, 1) :- !.

norm(Expr, Newdim) :-
	expr_to_list(Expr, FlatList),
	uniform_pow(FlatList, UniformedList),
	sum_exp(UniformedList, SummedList),
	sort_list(SummedList, OrderedList),
	simplify_pow(OrderedList, SempList),
	list_to_expr(SempList, Newdim),
	!.


uniform_pow([], []).

uniform_pow([Var ** Exp | T], [Var ** Exp | TU]) :-
    uniform_pow(T, TU).

uniform_pow([Var | T], [Var ** 1 | TU]) :-
    atom(Var),
    uniform_pow(T, TU).

simplify_pow([], []).

simplify_pow([_ ** 0 | T], R) :-
    simplify_pow(T, R).

simplify_pow([Var ** 1 | T], [Var | R]) :-
    simplify_pow(T, R).

simplify_pow([Var ** Exp | T], [Var ** Exp | R]) :-
    Exp \= 0,
    Exp \= 1,
    simplify_pow(T, R).

sort_list(List, Sorted) :-
	predsort(compare_units_only, List, Sorted).

compare_units_only(Result, U1 ** _, U2 ** _) :-
	compare_units(Result, U1, U2).

compare_units_only(Result, U1 ** _, U2) :-
	compare_units(Result, U1, U2).

compare_units_only(Result, U1, U2 ** _) :-
	compare_units(Result, U1, U2).

compare_units_only(Result, U1, U2) :-
	compare_units(Result, U1, U2).
	
compare_units(>, U1, U2) :-
    unit(U1, _, _, _, N1),
    unit(U2, _, _, _, N2),
    N1 > N2,
	!.

compare_units(<, U1, U2) :-
    unit(U1, _, _, _, N1),
    unit(U2, _, _, _, N2),
    N1 < N2,
    !.

compare_units(=, U1, U2) :-
    unit(U1, _, _, _, N1),
    unit(U2, _, _, _, N2),
    N1 =:= N2,
    !.	

expr_to_list(Expr, FlatList) :-
    expr_to_list_(Expr, List),
    flatten(List, Flat),
    exclude(=(1), Flat, FlatList).  %%% rimuove tutti gli 1 dalla lista

expr_to_list_(A * 1, List) :-
	expr_to_list_(A ** 1, List).

expr_to_list_(A * B, List) :-
    expr_to_list_(A, LA),
    expr_to_list_(B, LB),
    append(LA, LB, List), !.

expr_to_list_((A * B) ** N, List) :-
    integer(N),
    expr_to_list_(A, LA),
    expr_to_list_(B, LB),
    maplist(raise_power(N), LA, LAN),
    maplist(raise_power(N), LB, LBN),
    append(LAN, LBN, List), !.

expr_to_list_((Base ** A) ** B, List) :-
    number(A),
    number(B),
    AB is A * B,
    expr_to_list_(Base ** AB, List), !.

expr_to_list_(_ ** 0, [1]) :- !.

expr_to_list_(Base ** 1, [Base]) :- !.

expr_to_list_(Base ** N, [Base ** N]) :-
    integer(N), !.

expr_to_list_(X, [X]) :-
	X \= 1,
    atomic(X), !.
	
raise_power(0, _X, 1) :- !.

raise_power(1, X, X) :- !.

raise_power(_, _ ** 0, 1) :- !.

raise_power(N, Base ** 1, Base ** N) :- !.

raise_power(N, Base ** M, Base ** P) :-
    number(M),
    P is M * N, !.

raise_power(N, X, X ** N).

sum_exp(Lista, Risultato) :-
    sum_exp(Lista, [], Acc),
    acc_to_potenze(Acc, Risultato).

sum_exp([], Acc, Acc).

sum_exp([Var ** Exp | T], AccIn, AccOut) :-
    add_or_sum(Var, Exp, AccIn, AccNuovo),
    sum_exp(T, AccNuovo, AccOut).

add_or_sum(Var, Exp, [], [Var-Exp]).

add_or_sum(Var, Exp, [Var-Old | T], [Var-New | T]) :-
    New is Old + Exp.

add_or_sum(Var, Exp, [X | T], [X | NT]) :-
    X = Other-_,
    Var \= Other,
    add_or_sum(Var, Exp, T, NT).

acc_to_potenze([], []).

acc_to_potenze([Var-Exp | T], [Var ** Exp | RT]) :-
    acc_to_potenze(T, RT).
    
list_to_expr([], 1).
list_to_expr([X], X).
list_to_expr([H|T], H * R) :-
    list_to_expr(T, R).



%%% caso unità moltiplicate
uniform_quantity(q(V, U1 * U2), q(VNew, U1New * U2New)) :-
    uniform_quantity(q(1, U1), q(F1, U1New)),
    uniform_quantity(q(1, U2), q(F2, U2New)),
    VNew is V * F1 * F2,
    !.

%%% caso unità elevate a potenza
uniform_quantity(q(V, U ** Exp), q(VNew, UNew ** Exp)) :-
    number(Exp),
    uniform_quantity(q(1, U), q(F, UNew)),
    FactorExp is F ** Exp,
    VNew is V * FactorExp,
    !.

%%% caso base per unità adimensionali
uniform_quantity(q(V, 1), q(V, 1)) :- !.

%%% caso base per unità presente in unit/5
uniform_quantity(q(V, Unit), q(V, Unit)) :-
	unit(Unit, _, _, _, _),
	!.

%%% caso base per la gestione dag -> kg
uniform_quantity(q(V, dag), q(VNew, kg)) :-
    VNew is V * 0.01, 
    !.

%%% caso base per la gestione g -> kg
uniform_quantity(q(V, g), q(VNew, kg)) :-
	VNew is V * 0.001,
	!.

%%% caso base per la gestione _g -> kg	
uniform_quantity(q(V, PreUnit), q(VNew, kg)) :-
    atom(PreUnit),
    atom_chars(PreUnit, [C,g]),
    prefix(C, FactorToG),
    FactorToKG is FactorToG * 0.001,
    VNew is V * FactorToKG, 
    !.

uniform_quantity(q(V, PreUnit), q(VNew, BaseUnit)) :-
    atom(PreUnit),
    atom_chars(PreUnit, [C|Rest]),
    prefix(C, Factor),
    atom_chars(BaseUnit, Rest),
    unit(BaseUnit, _, _, _, _),
    VNew is V * Factor, !.

uniform_quantity(q(V, PreUnit), q(VNew, BaseUnit)) :-
    atom(PreUnit),
    atom_chars(PreUnit, [C1,C2|Rest]),
    atom_chars(Prefix, [C1,C2]),
    prefix(Prefix, Factor),
    atom_chars(BaseUnit, Rest),
    unit(BaseUnit, _, _, _, _),
    VNew is V * Factor, !.

%%% Unità base/derivate con prefisso singolo (1 carattere) ed esponente
uniform_quantity(q(V, PreUnit ** Exp), q(VNew, BaseUnit ** Exp)) :-
    atom(PreUnit),
    atom_chars(PreUnit, [C|Rest]),
    prefix(C, Factor),
    atom_chars(BaseUnit, Rest),
    unit(BaseUnit, _, _, _, _),
    FactorExp is Factor ** Exp,
    VNew is V * FactorExp,
    !.

%%% Unità base/derivate con prefisso doppio (2 caratteri) ed esponente
uniform_quantity(q(V, PreUnit ** Exp), q(VNew, BaseUnit ** Exp)) :-
    atom(PreUnit),
    atom_chars(PreUnit, [C1, C2 | Rest]),
    atom_chars(Prefix, [C1, C2]),
    prefix(Prefix, Factor),
    atom_chars(BaseUnit, Rest),
    atom_chars(BaseUnitAtom, Rest),
    atom_chars(PrefixAtom, [C1, C2]),
    atom_concat(PrefixAtom, BaseUnitAtom, PreUnit),
    unit(BaseUnit, _, _, _, _),
    FactorExp is Factor ** Exp,
    VNew is V * FactorExp,
    !.

%%% Caso speciale: prefisso + 'g' (grammo), che va normalizzato a 'kg'
uniform_quantity(q(V, PreUnit ** Exp), q(VNew, kg ** Exp)) :-
    atom(PreUnit),
    atom_chars(PreUnit, [C, g]),
    prefix(C, FactorToG),
    FactorToKG is FactorToG * 0.001,
    FactorExp is FactorToKG ** Exp,
    VNew is V * FactorExp,
    !.


qadd(q(V1, D1), q(V2, D2), q(VR, DR)) :-
    uniform_quantity(q(V1, D1), q(V1_R, D1_U)),
    uniform_quantity(q(V2, D2), q(V2_R, D2_U)),
    norm(D1_U, D1_R),
    norm(D2_U, D2_R),
    is_quantity(q(V1_R, D1_R)),
    is_quantity(q(V2_R, D2_R)),
    D1_R == D2_R,
    VR is V1_R + V2_R,
    DR = D1_R,
	is_quantity(q(VR, DR)).
	
	
qsub(q(V1, D1), q(V2, D2), q(VR, DR)) :-
	NV2 is -V2,
	qadd(q(V1, D1), q(NV2, D2), q(VR, DR)).
	
qmul(q(V1, D1), q(V2, D2), q(VR, DR)) :-
    uniform_quantity(q(V1, D1), q(V1_R, D1_U)),
    uniform_quantity(q(V2, D2), q(V2_R, D2_U)),
    norm(D1_U, D1_R),
    norm(D2_U, D2_R),
    is_quantity(q(V1_R, D1_R)),
    is_quantity(q(V2_R, D2_R)),
    VR is V1_R * V2_R,
    norm(D1_R * D2_R, DR),
    is_quantity(q(VR, DR)).	

qdiv(q(V1, D1), q(V2, D2), q(VR, DR)) :-
	V2 =\= 0,
	InvV2 is 1 / V2,
	qmul(q(V1, D1), q(InvV2, D2 ** -1), q(VR, DR)).  

qexp(q(V1, D1), N, q(VR, DR)) :-
	number(N),
	uniform_quantity(q(V1, D1), q(V1_R, D1_U)),
	norm(D1_U, D1_R),
	is_quantity(q(V1_R, D1_R)),
	VR is V1_R ** N,
	norm(D1_R ** N, DR),
	is_quantity(q(VR, DR)).








