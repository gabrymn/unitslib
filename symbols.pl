
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

prefix('G', 1.0e9).
prefix('M', 1.0e6).
prefix(k, 1.0e3).
prefix(h, 1.0e2).
prefix(da, 1.0e1).
prefix(d, 1.0e-1).
prefix(c, 1.0e-2).
prefix(m, 1.0e-3).
prefix(u, 1.0e-6).
prefix(n, 1.0e-9).


