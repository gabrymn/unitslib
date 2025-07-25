%%%% 990418 John Zaydan


%%% unit(+Symbol, +Name, +Definition, +Type, *Position).
%%% -------------------------------------------------------------------
unit(kg,      kilogram,        kg,  					base,    1).
unit(m,       metre,           m,   					base,    2).
unit(s,       second,   	   s,   					base,    3).
unit('A',     'Ampere',        'A', 					base,    4).
unit('K',     'Kelvin',        'K', 					base,    5).
unit(cd,      candela,         cd,  					base,    6).
unit(mol,     mole,            mol, 					base,    7).

unit('Bq',    'Becquerel',     s ** -1,          		derived, 8).
unit(dc,      degreecelsius,   'K',              		derived, 9).
unit('C',     'Coulomb',       'A' * s,                	derived, 10).
unit('F',     'Farad',         'C' * ('V' ** -1),       derived, 11).
unit('Gy',    'Gray',          'J' * (kg ** -1),        derived, 12).
unit('Hz',    'Hertz',         s ** -1,          		derived, 13).
unit('H',     'Henry',         'V' * s * ('A' ** -1),   derived, 14).
unit('J',     'Joule',         m * 'N',                 derived, 15).
unit(kat,     'Katal',         mol * (s ** -1),         derived, 16).
unit(lm,      lumen,           cd * sr,          	    derived, 17).
unit(lx,      lux,             lm * (m ** -2),     	    derived, 18).
unit('N',     'Newton',        kg * m * (s ** -2), 	    derived, 19).
unit('Omega', ohm,             'V' * ('A' ** -1),       derived, 20).
unit('Pa',    'Pascal',        'N' * (m ** -2),         derived, 21).
unit(rad,     radian,          1,                		derived, 22).
unit('S',     'Siemens',       'Omega' ** -1,           derived, 23).
unit('Sv',    'Sievert',       'J' * (kg ** -1),        derived, 24).
unit(sr,      steradian,       1,                       derived, 25).
unit('T',     'Tesla',         'Wb' * (m ** -2),        derived, 26).
unit('V',     'Volt',          'J' * ('C' ** -1),       derived, 27).
unit('W',     'Watt',          'J' * (s ** -1),         derived, 28).
unit('Wb',    'Weber',         'V' * s,                 derived, 29).
% -------------------------------------------------------------------




% casi ricorsivi merge_norms:

norm3(X1, Y1, R) :-
	norm3(X1 * 1, Y1 * 1, R).

norm3(X1 * Xs, Y1, R) :-
	norm3(X1 * Xs, Y1 * 1, R).

norm3(X1, Y1 * Ys, R) :-
	norm3(X1 * 1, Y1 * Ys, R).

norm3(X1 * Xs, Y1 * Ys, R) :-
    unit(X1, _, _, _, Nx),
    unit(Y1, _, _, _, Ny),
    Nx < Ny,
    norm3(Xs, Y1 * Ys, R1),
    norm(X1 * R1, R).

norm3(X1 * Xs, Y1 * Ys, R) :-
    unit(X1, _, _, _, Nx),
    unit(Y1, _, _, _, Ny),
    Nx > Ny,
    norm3(X1 * Xs, Ys, R1),
    norm(Y1 * R1, R).	

norm3(X1 * Xs, Y1 * Ys, R) :-
    unit(X1, _, _, _, Nx),
    unit(Y1, _, _, _, Ny),
    Nx == Ny,
    norm(X1 * Y1, M),
    norm3(Xs, Ys, R1),
    norm(M * R1, R).

norm3((X1 ** N) * Xs, (Y1 ** M) * Ys, R) :-
    unit(X1, _, _, _, Nx),
    unit(Y1, _, _, _, Ny),
	number(N), number(M),
	Nx < Ny,
	norm3(Xs, (Y1 ** M) * Ys, R1),
	norm((X1 ** N) * R1, R).
	
norm3((X1 ** N) * Xs, (Y1 ** M) * Ys, R) :-
    unit(X1, _, _, _, Nx),
    unit(Y1, _, _, _, Ny),
	number(N), number(M),
	Nx < Ny,
	norm3((X1 ** N) * Xs, Ys, R1),
	norm((Y1 ** N) * R1, R).
	
norm3((X1 ** N) * Xs, Y1 ** M, R) :-
    unit(X1, _, _, _, Nx),
    unit(Y1, _, _, _, Ny),
    number(N), number(M),
    
	
% casi base merge_norms:

norm3(1, Y, Y).
	
norm3(X, 1, X).


% casi ricorsivi

norm((X * Y) ** N, R) :-
    number(N),
    is_dimension(X * Y),
    norm(X ** N, NX),
    norm(Y ** N, NY),
    norm(NX * NY, R).

norm(X * Y, NewDim) :-
	norm(X, NewDimX),
	norm(Y, NewDimY),
	norm3(NewDimX, NewDimY, NewDim).

% casi base:

norm(1, 1).

norm(X, X) :-
	unit(X, _, _, _, _).


norm(X * Y, X * Y) :-
	unit(X, _, _, _, Nx),
	unit(Y, _, _, _, Ny),
	Nx < Ny.

norm(X * Y, Y * X) :-
	unit(X, _, _, _, Nx),
	unit(Y, _, _, _, Ny),
	Nx > Ny.

norm(X ** N, X ** N) :-
	unit(X, _, _, _, _),
	number(N). 

norm((X ** N) * (Y ** M), (X ** N) * (Y ** M)) :-
	unit(X, _, _, _, Nx),
	unit(Y, _, _, _, Ny),
	Nx < Ny.
	
norm((X ** N) * (Y ** M), (Y ** M) * (X ** N)) :-
	unit(X, _, _, _, Nx),
	unit(Y, _, _, _, Ny),
	Nx > Ny.

norm(X * X, X ** 2) :-
	unit(X, _, _, _, _).

% non soddisfa norm(m * m, K) oppure norm(s * s, K) ...
% perchè non è come m ** 1, e non è come s ** 1, ...
norm((X ** N) * (X ** M), X ** L) :-
	unit(X, _, _, _, _),
	number(N),
	number(M),
	L is N + M,
	L =\= 0.
	
norm((X ** N) * (X ** M), 1) :-
    unit(X, _, _, _, _),
    number(N),
    number(M),
    L is N + M,
    L =:= 0.
   


% Symbols
% -------------------------------------------------------------------
is_base_si_unit(S) :- unit(S, _, _, base, _).

is_si_unit(S) :- is_base_si_unit(S).
is_si_unit(S) :- unit(S, _, _, derived, _).

si_unit_name(S, N) :-
	unit(S, N, _, _, _, _).

si_unit_symbol(N, S) :-
	unit(S, N, _, _, _, _).

si_unit_base_expansion(S, Expansion) :-
	unit(S, _, Es, _, _),
	obs(Es, Expansion).

obs(X, Y) :-
	unit(X, _, H, derived, _),
	obs(H, Y).

obs(X, X) :-
	unit(X, _, X, base, _).

obs(X * Y, Z * K) :-
	obs(X, Z),
	obs(Y, K).

obs(X ** N, Z ** N) :-
	obs(X, Z),
	number(N).
	

o3(Expr) :- is_base_si_unit(Expr).

o3(Expr) :-
	unit(Expr, _, Dev, derived, _),
	o3(Dev).
	
o3(X * Y) :-
	o3(X),
	o3(Y).	
	
o3(X ** N) :-
	o3(X),
	number(N).
	 
% -------------------------------------------------------------------
	

% Dimension & Quantity
% -------------------------------------------------------------------
is_dimension(D) :-
	is_si_unit(D).

is_dimension(X * Y) :-
	is_dimension(X),
	is_dimension(Y).

is_dimension(X ** N) :-
	is_dimension(X),
	number(N).
	
is_quantity(q(N, D)) :- number(N), is_dimension(D).
% -------------------------------------------------------------------














