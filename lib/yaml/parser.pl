:- module(parser, [
                   parse/2      % +YAML, -PL
                  ]).

/** <module> YAML parser base on https://github.com/openbohemians/diet-yaml.
Core part of YAML spec is implemented.

@author Hongxin Liang
@license Apache License Version 2.0
*/

:- use_module(library(date)).

:- thread_local
    condition/1.

%% parse(+YAML, -PL) is semidet.
%
% Parse given =YAML= atom or chars to Prolog term.
% No comment is allowed to appear in =YAML=. util:read_yaml/2
% can be used to help read in YAML file.

parse(YAML, PL) :-
    atom(YAML), !,
    atom_chars(YAML, YAML0),
    yaml(PL, YAML0, []).

parse(YAML, PL) :-
    yaml(PL, YAML, []).


yaml(YAML) --> start, optional_whitespaces, data(YAML), optional_whitespaces, end.


start --> newline, dash, dash, dash, !.
start --> [].

end --> newline, ['.', '.', '.'], !.
end --> newline, dash, dash, dash, !.
end --> [].

data(Data) --> sequence(Data), !.
data(Data) --> mapping(Data), !.
data([]) --> ['[', ']'], !.
data(_{}) --> ['{', '}'], !.
data(Data) --> optional_whitespaces, scalar(Data), !.

scalar(Scalar) --> string(String),
    {
     (   atom(String)
     ->  (   atom_number(String, Scalar)
         ->  true
         ;   (   parse_time(String, iso_8601, T)
             ->  stamp_date_time(T, Scalar, 'UTC')
             ;   (   String = '~'
                 ->  Scalar = nil
                 ;   Scalar = String
                 )
             )
         )
     ;   Scalar = String
     )
    }.

sequence([H|T], ['['|Chars], Rest) :-
    setup_call_cleanup(asserta(condition(sequence)),
                       (
                        optional_whitespaces(Chars, Rest1),
                        data(H, Rest1, Rest2),
                        comma_data(T, Rest2, Rest3),
                        optional_whitespaces(Rest3, [']'|Rest])
                       ),
                       retract(condition(sequence))), !.

sequence([H|T], Chars, Rest) :-
    optional_spaces(Count, Chars, Rest1),
    dash(Rest1, Rest2),
    Count1 is Count + 1,
    pushback(Count1, Rest2, Rest3),
    Rest4 = ['\n'|Rest3],
    whitespaces_with_pushback(Rest4, Rest5),
    data(H, Rest5, Rest6),
    dash_data(Count, T, Rest6, Rest).

mapping(Mapping, ['{'|Chars], Rest) :-
    setup_call_cleanup(asserta(condition(mapping)),
                       (
                        optional_whitespaces(Chars, Rest1),
                        key(Key, Rest1, Rest2),
                        optional_spaces(_, Rest2, Rest3),
                        colon(Rest3, Rest4),
                        at_least_one_whitespace(Rest4, Rest5),
                        data(Data, Rest5, Rest6),
                        colon_data(Dict, Rest6, Rest7),
                        optional_whitespaces(Rest7, ['}'|Rest]),
                        Mapping = Dict.put(Key, Data)
                       ),
                       retract(condition(mapping))), !.

mapping(Mapping) -->
    optional_spaces(Count),
    key(Key),
    optional_spaces(_),
    colon,
    whitespaces_with_pushback,
    data(Data),
    tab_data(Count, Dict),
    {
     Mapping = Dict.put(Key, Data)
    }.

optional_spaces(Count) --> space, optional_spaces(X), !,
    {
     Count is X + 1
    }.
optional_spaces(0) --> [].

string(String) --> ['"'], chars_exclude_trailing_double_quote(Chars), ['"'], !,
    {
     atom_string(Chars, String)
    }.

string(String) --> ['\''], chars_exclude_trailing_single_quote(Chars), ['\''], !,
    {
     atom_string(Chars, String)
    }.

string(String, ['>'|Chars], Rest) :-
    optional_spaces(_, Chars, ['\n'|Rest1]),
    optional_spaces(Count, Rest1, Rest2),
    line(X, Rest2, Rest3),
    next_line(Y, Count, Rest3, Rest4), !,
    (   sub_atom(Y, 0, 1, _, '\n')
    ->  atom_concat(X, Y, String)
    ;   atomic_list_concat([X, ' ', Y], String)
    ),
    Rest = ['\n'|Rest4].

string(String, ['|'|Chars], Rest) :-
    optional_spaces(_, Chars, ['\n'|Rest1]),
    optional_spaces(Count, Rest1, Rest2),
    line(X, Rest2, Rest3),
    next_line_preserving_newline(Y, Count, Rest3, Rest4), !,
    atomic_list_concat([X, '\n', Y], String),
    Rest = ['\n'|Rest4].

string(String) --> valid_start(Char), chars(Chars),
    {
     atom_concat(Char, Chars, String)
    }.

space --> [' '].

key(Key) -->
    valid_key_char(X),
    key(Y), !,
    {
     atom_concat(X, Y, Key)
    }.
key(Key) --> valid_key_char(Key).

dash --> ['-'].
colon --> [':'].
comma --> [','].
newline --> ['\n'].

at_least_one_newline --> newline, at_least_one_newline, !.
at_least_one_newline --> newline.

newlines(V) --> newline, newlines(Y), !,
    {
     atom_concat('\n', Y, V)
    }.
newlines('\n') --> newline.

optional_whitespaces --> space, optional_whitespaces, !.
optional_whitespaces --> newline, optional_whitespaces, !.
optional_whitespaces --> [].

at_least_one_whitespace --> space, optional_whitespaces, !.
at_least_one_whitespace --> newline, optional_whitespaces.

whitespaces_with_pushback --> space, whitespaces_with_pushback, !.
whitespaces_with_pushback --> newline, whitespaces_with_pushback0(0), !.
whitespaces_with_pushback --> space, !.
whitespaces_with_pushback --> newline.

whitespaces_with_pushback0(Leading) --> space, !,
    {
     Leading1 is Leading + 1
    },
    whitespaces_with_pushback0(Leading1).
whitespaces_with_pushback0(_) --> newline, !,
    whitespaces_with_pushback0(0).
whitespaces_with_pushback0(Leading, [H|T], Rest) :-
    H \= ' ',
    H \= '\n',
    pushback(Leading, [H|T], Rest).

pushback(0, List, List) :- !.
pushback(Count, List0, List) :-
    List1 = [' '|List0],
    Count1 is Count - 1,
    pushback(Count1, List1, List).

comma_data([H|T]) --> comma, optional_whitespaces, data(H), comma_data(T).
comma_data([]) --> [].

dash_data(Leading, [H|T], Chars, Rest) :-
    at_least_one_newline(Chars, Rest0),
    optional_spaces(Leading, Rest0, Rest1),
    dash(Rest1, Rest2),
    Count is Leading + 1,
    pushback(Count, Rest2, Rest3),
    Rest4 = ['\n'|Rest3],
    whitespaces_with_pushback(Rest4, Rest5),
    data(H, Rest5, Rest6),
    dash_data(Leading, T, Rest6, Rest), !.
dash_data(_, []) --> [].

colon_data(V) -->
    comma,
    optional_whitespaces,
    key(Key),
    optional_spaces(_),
    colon,
    at_least_one_whitespace,
    data(Data),
    colon_data(Dict), !,
    {
     V = Dict.put(Key, Data)
    }.
colon_data(_{}) --> [].

tab_data(Leading, V) -->
    at_least_one_newline,
    optional_spaces(Leading),
    key(Key),
    optional_spaces(_),
    colon,
    whitespaces_with_pushback,
    data(Data),
    tab_data(Leading, Dict), !,
    {
     V = Dict.put(Key, Data)
    }.
tab_data(_, _{}) --> [].

chars_exclude_trailing_double_quote(V, [H|T], Rest) :-
    lookahead(T, C),
    (   C = '"'
    ->  (   H = '\\'
        ->  chars_exclude_trailing_double_quote(X, T, Rest),
            atom_concat(H, X, V)
        ;   V = H,
            Rest = T
        )
    ;   chars_exclude_trailing_double_quote(X, T, Rest),
        atom_concat(H, X, V)
    ), !.
chars_exclude_trailing_double_quote('') --> [].

chars_exclude_trailing_single_quote(V, [H|T], Rest) :-
    (   H = '\''
    ->  T = [C|T1],
        (   C = '\''
        ->  chars_exclude_trailing_single_quote(X, T1, Rest),
            atom_concat('\'', X, V)
        ;   V = '',
            Rest = [H|T]
        )
    ;   chars_exclude_trailing_single_quote(X, T, Rest),
        atom_concat(H, X, V)
    ), !.
chars_exclude_trailing_single_quote('') --> [].

chars(V, [X|Chars], Rest) :-
    (   condition(sequence), !
    ->  X \= ']',
        X \= ','
    ;   (   condition(mapping), !
        ->  X \= '}',
            X \= ','
        ;   true
        )
    ),
    X \= '\n', !,
    chars(Y, Chars, Rest),
    atom_concat(X, Y, V).
chars('') --> [].

line(V) -->
    [X],
    {
     X \= '\n'
    },
    line(Y),
    {
     atom_concat(X, Y, V)
    }.
line('') --> [].

next_line(V, Leading) -->
    ['\n'],
    optional_spaces(Leading),
    line(X),
    next_line(Y, Leading), !,
    {
     (   sub_atom(Y, 0, 1, _, '\n')
     ->  atom_concat(X, Y, V)
     ;   atomic_list_concat([X, ' ', Y], V)
     )
    }.
next_line(V, Leading, ['\n'|Chars], Rest) :-
    newlines(X, Chars, Rest1),
    next_line(Y, Leading, ['\n'|Rest1], Rest), !,
    atom_concat(X, Y, V).
next_line('', _) --> [].

next_line_preserving_newline(V, Leading) -->
    ['\n'],
    optional_spaces(Leading),
    line(X),
    next_line_preserving_newline(Y, Leading), !,
    {
     atomic_list_concat([X, '\n', Y], V)
    }.
next_line_preserving_newline(V, Leading, ['\n'|Chars], Rest) :-
    newlines(X, Chars, Rest1),
    next_line_preserving_newline(Y, Leading, ['\n'|Rest1], Rest), !,
    atom_concat(X, Y, V).
next_line_preserving_newline('', _) --> [].

valid_start(V) --> [V], {V \= '-', V \= '[', V \= '{'}.

valid_key_char(V) --> [V],
    {
     code_type(V, Type),
     (Type = csym; Type = period)
    }.

lookahead([H|_], H).
