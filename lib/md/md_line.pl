:- module(md_line, [
    merge_lines/2,               % +Lines, -Codes
    indent//0,
    non_empty_line//1,           % -Codes
    discard_to_line_end//0,
    empty_lines//0,
    empty_line//0,
    inline_string//1,            % -Codes
    ln_or_eos//0,
    ln//0,
    string_limit//2,             % -Codes, +Limit
    lookahead//1,                % ?Code
    lookahead_ln//0,
    lookahead_ln_or_eos//0
]).

/** <module> Line-based parsing primitives.

Contains line-based parsing primitives.
*/

:- use_module(library(dcg/basics)).

%! merge_lines(+Lines, -Codes) is det.
%
% Merges list of lines into
% a flat code list.

merge_lines([], []).

merge_lines([Line], Line):- !.

merge_lines([Line|Lines], Codes):-
    merge_lines(Lines, Merged),
    append(Line, [0'\n|Merged], Codes).

%! indent// is semidet.
%
% Recognizes normal indent which
% is a tab or 4 spaces.

indent --> "\t".
indent --> "    ".

%! non_empty_line(-Codes)// is semidet.
%
% Single non-empty line ending with newline
% or end-of-stream.

non_empty_line([Code|Codes]) -->
    [Code], { Code \= 0'\n },
    non_empty_line_rest(Codes).

non_empty_line_rest([Code|Codes]) -->
    [Code], { Code \= 0'\n }, !,
    non_empty_line_rest(Codes).

non_empty_line_rest([]) -->
    "\n", !.

non_empty_line_rest([]) -->
    "".

%! discard_to_line_end// is det.
%
% Discards zero or more symbol
% codes untill the first line
% end or eos is reached.

discard_to_line_end -->
    ln_or_eos, !.

discard_to_line_end -->
    [_], discard_to_line_end.

%! empty_lines// is det.
%
% List of consequtive empty lines.
% Consumes as many empty lines as
% possible.

empty_lines -->
    eos, !.

empty_lines -->
    empty_line, !,
    empty_lines.

empty_lines --> "".

%! empty_line// is semidet.
%
% Recognizes a single empty line.

empty_line -->
    whites, ln_or_eos.

%! lookahead(?Code)// is semidet.
%
% Looks ahead a single symbol code.

lookahead(Code), [Code] -->
    [Code].

%! string_limit(-Codes, +Limit)// is multidet.
%
% Same as string//1 but with
% a length limit.

string_limit([], Limit) -->
    { Limit =< 0 }, !.

string_limit([], Limit) -->
    { Limit >= 0 }.

string_limit([Code|Codes], Limit) -->
    [Code],
    { Next is Limit - 1 },
    string_limit(Codes, Next).

%! inline_string(-Codes)// is multidet.
%
% Takes as few symbol codes as possible
% up to line end.

inline_string([]) --> "".

inline_string([]) -->
    lookahead_ln, !.

inline_string([Code|Codes]) -->
    [Code],
    inline_string(Codes).

%! lookahead_ln_or_eos// is semidet.
%
% Looks ahead a line end or
% end-of-stream. Puts back `\n`
% when a line end is recognized.

lookahead_ln_or_eos -->
    lookahead_ln, !.

lookahead_ln_or_eos -->
    eos.

%! lookahead_ln// is semidet.
%
% Looks ahead a line end. Puts
% back `\n` when it is recognized.

lookahead_ln, "\n" --> ln.

%! ln_or_eos// is semidet.
%
% Recognizes either a line end
% or eos.

ln_or_eos -->
    "\n", !.

ln_or_eos -->
    eos.

%! ln// is semidet.
%
% Recognizes line ending.

ln --> "\n".
