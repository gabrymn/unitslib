%%%% 990418 John Zaydan


% unit(symbol, name, {base, derived})
% -------------------------------------------------------------------
unit(kg,  kilogram, kg,  base).
unit(m,   metre,    m,   base).
unit(s,   second,   s,   base).
unit('A', 'Ampere', 'A', base).
unit('K', 'Kelvin', 'K', base).
unit(cd,  candela,  cd,  base).
unit(mol, mole,     mol, base).

% 0 == da fare ancora
-------------------------------------------------------------------
unit('Bq',    'Becquerel',     s ** -1,          derived).
unit(dc,      degreecelsius,   'K',              derived).
unit('C',     'Coulomb',       0,                derived).
unit('F',     'Farad',         0,                derived).
unit('Gy',    'Gray',          0,                derived).
unit('Hz',    'Hertz',         s ** -1,          derived).
unit('H',     'Henry',         0,                derived).
unit('J',     'Joule',         0,                derived).
unit(kat,     'Katal',         0,                derived).
unit(lm,      lumen,           cd * sr,          derived).
unit(lx,      lux,             lm * m ** -2,     derived).
unit('N',     'Newton',        kg * m * s ** -2, derived).
unit('Omega', 'Ohm',           0,                derived).
unit('Pa',    'Pascal',        0,                derived).
unit(rad,     radian,          0,                derived).
unit('S',     'Siemens',       0,                derived).
unit('Sv',    'Sievert',       0,                derived).
unit(sr,      steradian,       1,                derived).
unit('T',     'Tesla',         0,                derived).
unit('V',     'Volt',          0,                derived).
unit('W',     'Watt',          0,                derived).
unit('Wb',    'Weber',         0,                derived).
% -------------------------------------------------------------------

is_base_si_unit(S) :- unit(S, _, base).

is_si_unit(S) :- is_base_si_unit(S).
is_si_unit(S) :- unit(S, _, derived).


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














