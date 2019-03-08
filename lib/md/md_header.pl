:- module(md_header, [
    md_header//1 % -Header
]).

/** <module> Markdown header parser.

Recognizes atx and setext-styled headers.
*/

:- use_module(library(dcg/basics)).

:- use_module(md_line).
:- use_module(md_trim).

%! md_header(-Header)// is semidet.
%
% Recognizes either setext
% or atx-styled headings.

md_header(Header) -->
    setext_header(Header), !.

md_header(Header) -->
    atx_header(Header).

% Recognizes setext-styled headings.
% Output is a term like h1("heading").

setext_header(Header) -->
    non_empty_line(Codes),
    (   equals_line,
        { Name = h1 }
    ;   dashes_line,
        { Name = h2 }),
    {
        string_codes(Title, Codes),
        Header =.. [Name, Title]
    }.

% Recognizes atx-styled heading.
% Output is a term like h1(heading).

atx_header(Header) -->
    atx_level(Level), whites,
    atx_header_text(Codes), !,
    discard_to_line_end,
    {
        trim(Codes, Trimmed),
        string_codes(Title, Trimmed),
        atomic_concat(h, Level, Name),
        Header =.. [Name,Title]
    }.

% Recognizes atx-styled header
% prefix.

atx_level(1) --> "# ".
atx_level(2) --> "## ".
atx_level(3) --> "### ".
atx_level(4) --> "#### ".
atx_level(5) --> "##### ".
atx_level(6) --> "###### ".

% Recognizes header text for
% atx-styles headings. Such text
% ends with #, line end, or eos.
% Escapes \# must be also processed.
% Line ending is not consumed.

atx_header_text([]) -->
    "#", !.

atx_header_text([]) -->
    lookahead_ln_or_eos, !.

atx_header_text([0'#|Codes]) -->
    "\\#", !,
    atx_header_text(Codes).

atx_header_text([Code|Codes]) -->
    [Code], { Code \= 0'# }, !,
    atx_header_text(Codes).

% Line filles with one or more dashes.

dashes_line -->
    "-", dashes_line_rest.

dashes_line_rest -->
    eos, !.

dashes_line_rest -->
    "\n", !.

dashes_line_rest -->
    "-", dashes_line_rest.

equals_line -->
    "=", equals_line_rest.

equals_line_rest -->
    eos, !.

equals_line_rest -->
    "\n", !.

equals_line_rest -->
    "=", equals_line_rest.
