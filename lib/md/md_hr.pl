:- module(md_hr, [
    md_hr//0,
    md_lookahead_hr//0
]).

/** <module> Parser for Markdown horizontal rulers

Recognizes horizontal rulers.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_line).

%! md_hr// is semidet.
%
% Recognizes an horizontal ruler.

md_hr -->
    code_hr(0'*, 3, _), !.

md_hr -->
    code_hr(0'-, 3, _).

%! md_lookahead_hr// is semidet.
%
% Looks ahead an horizontal ruler.

md_lookahead_hr, Codes -->
    code_hr(0'*, 3, Codes), !.

md_lookahead_hr, Codes -->
    code_hr(0'-, 3, Codes).

% Recognizes given number of codes
% separated on a single line by 0
% or more spaces or tabs.

code_hr(_, 0, []) -->
    eos, !.

code_hr(_, 0, [0'\n]) -->
    "\n", !.

code_hr(Code, 0, [Code|Codes]) -->
    [Code], !, code_hr(Code, 0, Codes).

code_hr(Code, N, [0' |Codes]) -->
    " ", !, code_hr(Code, N, Codes).

code_hr(Code, N, [0'\t|Codes]) -->
    "\t", !, code_hr(Code, N, Codes).

code_hr(Code, N, [Code|Codes]) -->
    [Code], !, { N1 is N - 1 },
    code_hr(Code, N1, Codes).
