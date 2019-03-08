:- module(md_escape, [
    md_escaped_string//1, % -Codes,
    md_escaped_code//1,   % ?Code
    md_escaped_code/1     % ?Code
]).

/** <module> Markdown slash-escaped sequences

Recognizes Markdown slash-escaped sequences. More info:
http://daringfireball.net/projects/markdown/syntax#backslash
*/

%! md_escaped_string(-Codes)// is det.
%
% Recognizes string with escapes inside it.
% Consumes new code/escape sequence on backtracking.

md_escaped_string([]) --> "".

md_escaped_string([Code|Codes]) -->
    "\\", md_escaped_code(Code), !,
    md_escaped_string(Codes).

md_escaped_string([Code|Codes]) -->
    [Code], md_escaped_string(Codes).

%! md_escaped_code(-Codes)// is det.
%
% Recognizes single code that could
% have been escaped.

md_escaped_code(Code) -->
    [Code], { md_escaped_code(Code) }.

%! md_escaped_code(?Code) is nondet.
%
% List of possibly escaped symbols.
% More info:
% http://daringfireball.net/projects/markdown/syntax#backslash

md_escaped_code(0'\\).
md_escaped_code(0'`).
md_escaped_code(0'*).
md_escaped_code(0'_).
md_escaped_code(0'{).
md_escaped_code(0'}).
md_escaped_code(0'[).
md_escaped_code(0']).
md_escaped_code(0'().
md_escaped_code(0')).
md_escaped_code(0'#).
md_escaped_code(0'+).
md_escaped_code(0'-).
md_escaped_code(0'.).
md_escaped_code(0'!).
md_escaped_code(0'~).
