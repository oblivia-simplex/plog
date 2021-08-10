:- module(serializer, [
                       serialize/2      % +PL, -YAML
                      ]).

/** <module> YAML serializer.
The serialized result is fully YAML compatible although
not utilizing every feature of YAML spec.

@author Hongxin Liang
@license Apache License Version 2.0
*/

%% serialize(+PL, -YAML) is semidet.
%
% Serialize given Prolog term to YAML atom.

serialize(PL, YAML) :-
    serialize0('', PL, '', YAML).

serialize0(LeadingSpaces, PL, YAML0, YAML) :-
    is_dict(PL), !,
    newline(YAML0, YAML1),
    dict_pairs(PL, _, Pairs),
    (   Pairs = []
    ->  atomic_list_concat([YAML0, '{}\n'], YAML)
    ;   serialize_pairs(LeadingSpaces, Pairs, YAML1, YAML)
    ).

serialize0(LeadingSpaces, PL, YAML0, YAML) :-
    is_list(PL), !,
    newline(YAML0, YAML1),
    (   PL = []
    ->  atomic_list_concat([YAML0, '[]\n'], YAML)
    ;   serialize_list(LeadingSpaces, PL, YAML1, YAML)
    ).

serialize0(_, PL, YAML0, YAML) :-
    once((number(PL); PL = true; PL = false)), !,
    atomic_list_concat([YAML0, PL, '\n'], YAML).

serialize0(LeadingSpaces, PL, YAML0, YAML) :-
    atom(PL), !,
    atomic_list_concat(List, '\n', PL),
    (   List = [_]
    ->  atomic_list_concat([YAML0, PL, '\n'], YAML)
    ;   atom_concat(YAML0, '| ', YAML1),
        indent(LeadingSpaces, LeadingSpaces1),
        serialize_atom_with_line_separator(LeadingSpaces1, List, YAML1, YAML)
    ).

serialize0(_, PL, YAML0, YAML) :-
    string(PL), !,
    atom_string(Atom, PL),
    atomic_list_concat([YAML0, '"', Atom, '"', '\n'], YAML).

serialize0(_, PL, YAML0, YAML) :-
    compound(PL), !,
    format_time(atom(Atom), '%FT%T%z', PL), % iso_8601
    atomic_list_concat([YAML0, Atom, '\n'], YAML).

serialize_pairs(_, [], YAML, YAML) :- !.
serialize_pairs(LeadingSpaces, [K-V|T], YAML0, YAML) :-
    atomic_list_concat([YAML0, LeadingSpaces, K, ': '], YAML1),
    indent(LeadingSpaces, LeadingSpaces1),
    serialize0(LeadingSpaces1, V, YAML1, YAML2),
    serialize_pairs(LeadingSpaces, T, YAML2, YAML).

serialize_list(_, [], YAML, YAML) :- !.
serialize_list(LeadingSpaces, [H|T], YAML0, YAML) :-
    atomic_list_concat([YAML0, LeadingSpaces, '- '], YAML1),
    indent(LeadingSpaces, LeadingSpaces1),
    atom_concat(LeadingSpaces1, ' ', LeadingSpaces2),
    serialize0(LeadingSpaces2, H, YAML1, YAML2),
    serialize_list(LeadingSpaces, T, YAML2, YAML).

serialize_atom_with_line_separator(LeadingSpaces, [H], YAML0, YAML) :- !,
    atomic_list_concat([YAML0, '\n', LeadingSpaces, H, '\n'], YAML).

serialize_atom_with_line_separator(LeadingSpaces, [H|T], YAML0, YAML) :-
    atomic_list_concat([YAML0, '\n', LeadingSpaces, H], YAML1),
    serialize_atom_with_line_separator(LeadingSpaces, T, YAML1, YAML).

newline(YAML0, YAML) :-
    atom_concat(YAML0, '\n', YAML).

indent(LeadingSpaces0, LeadingSpaces) :-
    atom_concat(LeadingSpaces0, '  ', LeadingSpaces).