:- module(md_links, [
    md_links/3, % +CodesIn, -CodesOut, -Links
    md_links/2, % +CodesIn, -CodesOut,
    md_link/3   % ?Id, ?Url, ?Title
]).

/** <module> Markdown reference link parser

Parses and removes reference links from
the stream of symbol codes. Replaces
line ends with canonical line ends.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_line).

% link_definition(Id, Url, Title).

:- thread_local(link_definition/3).

%! md_link(?Id, ?Url, ?Title) is det.
%
% Retrieves recorded link from the last
% invocation of md_links/2.

md_link(Id, Url, Title):-
    link_definition(Id, Url, Title).

%! md_links(+CodesIn, -CodesOut) is det.
%
% Same as md_links/3 but stores links
% in threadlocal predicate which is cleared
% on each invocation of this predicate.

md_links(CodesIn, CodesOut):-
    retractall(link_definition(_, _, _)),
    md_links(CodesIn, CodesOut, Links),
    maplist(assert_link, Links).

assert_link(link(Id, Url, Title)):-
    assertz(link_definition(Id, Url, Title)).

%! md_links(+CodesIn, -CodesOut, -Links) is det.
%
% Markdown reference link definition parser.
% Removes link definitions from the symbol code list.

md_links(CodesIn, CodesOut, Links):-
    phrase(links_begin(TmpCodes, TmpLinks), CodesIn),
    CodesOut = TmpCodes,
    Links = TmpLinks.

links_begin(Codes, [Link|Links]) -->
    link(Link), !, links(Codes, Links).

links_begin(Codes, Links) -->
    links(Codes, Links).

links([Code|Codes], Links) -->
    [Code], { \+code_type(Code, end_of_line) }, !,
    links(Codes, Links).

links(Codes, [Link|Links]) -->
    ln_full, link(Link), !, links(Codes, Links).

links([0'\n|Codes], Links) -->
    ln_full, !, links(Codes, Links).

links([], []) --> eos, !.

ln_full --> "\r\n", !.
ln_full --> "\n", !.
ln_full --> "\r".

% Recognizes a reference link definition.
% Example: [foo]: http://example.com/ "Optional Title Here"
% Records the link but outputs nothing.

link(link(Id, Url, Title)) -->
    link_indent, link_id(Id),
    whites, link_url(Url),
    whites, link_title(Title).

% Link might be indented with
% up to 3 spaces. More info:
% http://daringfireball.net/projects/markdown/syntax#link

link_indent --> "   ".
link_indent --> "  ".
link_indent --> " ".
link_indent --> "".

% Recognizes a link title.
% When no title is found, Title is
% an empty atom ('').

link_title(Title) -->
    link_title_same_line(Title), !.

link_title(Title) -->
    ln_full, whites, link_title_same_line(Title), !.

link_title('') --> "".

link_title_same_line(Title) -->
    "'", !, inline_string(Codes), "'",
    whites, lookahead_ln_or_eos,
    { atom_codes(Title, Codes) }.

link_title_same_line(Title) -->
    "(", !, inline_string(Codes), ")",
    whites, lookahead_ln_or_eos,
    { atom_codes(Title, Codes) }.

link_title_same_line(Title) -->
    "\"", inline_string(Codes), "\"",
    whites, lookahead_ln_or_eos,
    { atom_codes(Title, Codes) }.

% Recognizes a link identifier.

link_id(Id) -->
    "[", whites, inline_string(Codes), whites, "]:",
    {
        atom_codes(Tmp, Codes),
        downcase_atom(Tmp, Id)
    }.

% Recognizes a link URL.

link_url(Url) -->
    "<", !, inline_string(Codes), ">",
    { atom_codes(Url, Codes) }.

link_url(Url) -->
    string_without([0'\n, 0'\t, 0' ], Codes),
    { atom_codes(Url, Codes) }.
