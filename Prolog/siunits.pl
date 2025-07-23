%%%% 990418 John Zaydan


%%% unit(+Symbol, +Name, +Definition, +Type).
%%% -------------------------------------------------------------------
unit(kg,      kilogram,        kg,  					base).
unit(m,       metre,           m,   					base).
unit(s,       second,   	   s,   					base).
unit('A',     'Ampere',        'A', 					base).
unit('K',     'Kelvin',        'K', 					base).
unit(cd,      candela,         cd,  					base).
unit(mol,     mole,            mol, 					base).

unit('Bq',    'Becquerel',     s ** -1,          		derived).
unit(dc,      degreecelsius,   'K',              		derived).
unit('C',     'Coulomb',       'A' * s,                	derived).
unit('F',     'Farad',         'C' * ('V' ** -1),       derived).
unit('Gy',    'Gray',          'J' * (kg ** -1),        derived).
unit('Hz',    'Hertz',         s ** -1,          		derived).
unit('H',     'Henry',         'V' * s * ('A' ** -1),   derived).
unit('J',     'Joule',         m * 'N',                 derived).
unit(kat,     'Katal',         mol * (s ** -1),         derived).
unit(lm,      lumen,           cd * sr,          	    derived).
unit(lx,      lux,             lm * (m ** -2),     	    derived).
unit('N',     'Newton',        kg * m * (s ** -2), 	    derived).
unit('Omega', 'Ohm',           'V' * ('A' ** -1),       derived).
unit('Pa',    'Pascal',        'N' * (m ** -2),         derived).
unit(rad,     radian,          1,                		derived).
unit('S',     'Siemens',       'Omega' ** -1,           derived).
unit('Sv',    'Sievert',       'J' * (kg ** -1),        derived).
unit(sr,      steradian,       1,                       derived).
unit('T',     'Tesla',         'Wb' * (m ** -2),        derived).
unit('V',     'Volt',          'J' * ('C' ** -1),       derived).
unit('W',     'Watt',          'J' * (s ** -1),         derived).
unit('Wb',    'Weber',         'V' * s,                 derived).
% -------------------------------------------------------------------


% Symbols
% -------------------------------------------------------------------
is_base_si_unit(S) :- unit(S, _, _, base).

is_si_unit(S) :- is_base_si_unit(S).
is_si_unit(S) :- unit(S, _, _, derived).

si_unit_name(S, N) :-
	unit(S, N, _, _).

si_unit_symbol(N, S) :-
	unit(S, N, _, _).

si_unit_base_expansion(S, Expansion) :-
	unit(S, _, Es, _),
	obn(Es, Expansion).

obn(X, Y) :-
	unit(X, _, H, derived),
	obn(H, Y).

obn(X, X) :-
	unit(X, _, X, base).

obn(X * Y, Z * K) :-
	obn(X, Z),
	obn(Y, K),

obn(X ** N, Z ** N) :-
	obn(X, Z),
	number(N).
	

o3(Expr) :- is_base_si_unit(Expr).

o3(Expr) :-
	unit(Expr, _, Dev, derived),
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














