:- module(md_span_link, [
    md_span_link//1 % -Html
]).

/** <module> Markdown span-level link parsing

Parses Markdown span-level links and images. Separated
from the `md_span` module for code clarity.
*/

:- use_module(library(dcg/basics)).

:- use_module(md_links).
:- use_module(md_line).

%! md_span_link(-Link)// is det.
%
% Recognizes different types of
% links from the stream of symbol codes.

md_span_link(Link) -->
    plain_http_link(Link), !.

md_span_link(Link) -->
    angular_link(Link), !.

md_span_link(Link) -->
    angular_mail_link(Link), !.

md_span_link(Link) -->
    normal_link(Link), !.

md_span_link(Link) -->
    reference_link(Link), !.

md_span_link(Image) -->
    reference_image(Image), !.

md_span_link(Image) -->
    image(Image).

% Recognizes a plain link.

plain_http_link(Link) -->
    http_prefix(Prefix), inline_string(Codes), link_end,
    {
        atom_codes(Atom, Codes),
        atom_concat(Prefix, Atom, Url),
        link(Url, '', Url, Link)
    }.

link_end -->
    eos, !.

link_end -->
    lookahead(Code),
    { code_type(Code, space) }.

http_prefix('http://') -->
    "http://".

http_prefix('https://') -->
    "https://".

% Recognizes inline automatic http(s) <link>.

angular_link(Link) -->
    "<", http_prefix(Prefix),
    inline_string(Codes), ">",
    {
        atom_codes(Atom, Codes),
        atom_concat(Prefix, Atom, Url),
        link(Url, '', Url, Link)
    }.

% Recognizes inline mail link <address@example.com>

angular_mail_link(Link) -->
    "<", inline_string(User),
    "@", inline_string(Host), ">",
    {
        append(User, [0'@|Host], Codes),
        atom_codes(Address, Codes),
        mail_link(Address, Link)
    }.

% Recognizes a normal Markdown link.
% Example: [an example](http://example.com/ "Title")
% More info:
% http://daringfireball.net/projects/markdown/syntax#link

normal_link(Link) -->
    label(Label),
    url_title(Url, Title),
    { link(Url, Title, Label, Link) }.

% Recognizes image ![Alt text](/path/to/img.jpg).
% With optional title: ![Alt text](/path/to/img.jpg "Optional title").
% More info: http://daringfireball.net/projects/markdown/syntax#img

image(Image) -->
    "!", label(Alt),
    url_title(Url, Title),
    {
        (   Title = ''
        ->  Image = img([src=Url, alt=Alt])
        ;   Image = img([src=Url, alt=Alt, title=Title]))
    }.

% Recognizes a reference link.
% Example: This is [an example][id] reference-style link.
% Fails when the reference is not defined. Identifier
% can be empty, then the lowercase label is used as the
% identifier.

reference_link(Link) -->
    label(Label),
    whites, identifier(Id),
    {
        (   Id = ''
        ->  downcase_atom(Label, RealId)
        ;   RealId = Id),
        md_link(RealId, Url, Title),
        link(Url, Title, Label, Link)
    }.

% Recognizes a reference-linked image.
% Example: ![Alt text][id]

reference_image(Image) -->
    "!", label(Label),
    whites, identifier(Id),
    {
        (   Id = ''
        ->  downcase_atom(Label, RealId)
        ;   RealId = Id),
        md_link(RealId, Url, Title),
        (   Title = ''
        ->  Image = img([src=Url, alt=Label])
        ;   Image = img([src=Url, alt=Label, title=Title]))
    }.

% Same as label block but the result
% is lowercased.

identifier(Id) -->
    label(Label),
    { downcase_atom(Label, Id) }.

% Recognizes a label block.
% Example: [This is a link].

label(Label) -->
    "[", whites, inline_string(Codes), whites, "]", !,
    { atom_codes(Label, Codes) }.

% Recognizes a normal link URL/title
% block. Example: (http://example.com "Title").

url_title(Url, Title) -->
    "(", inline_string(UrlCodes),
    whites, "\"", inline_string(TitleCodes), "\"", whites, ")", !,
    {
        atom_codes(Url, UrlCodes),
        atom_codes(Title, TitleCodes)
    }.

url_title(Url, Title) -->
    "(", inline_string(UrlCodes),
    whites, "'", inline_string(TitleCodes), "'", whites, ")", !,
    {
        atom_codes(Url, UrlCodes),
        atom_codes(Title, TitleCodes)
    }.

url_title(Url, '') -->
    "(", inline_string(Codes), ")", !,
    { atom_codes(Url, Codes) }.

% Creates an HTML link element. Has no title
% attribute when title is empty.

link(Url, '', Label, Element):- !,
    Element = a([href=Url], Label).

link(Url, Title, Label, Element):-
    Element = a([href=Url, title=Title], Label).

% Creates an HTML link element for an email address.
% Does not try to mangle the address as the original
% spec. XXX maybe should do that?

mail_link(Address, Element):-
    atom_concat('mailto:', Address, Href),
    Element = a([href=Href], Address).
