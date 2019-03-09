:- module(md_span, [
    md_span_codes/2, % +Codes, -HtmlTerms
    md_span_string/2 % +String, -HtmlTerms
]).

/** <module> Span-level Markdown parser

Parses span-level Markdown elements: emphasis,
inline-code, links and others. More info:
http://daringfireball.net/projects/markdown/syntax#span
*/

:- use_module(library(dcg/basics)).
:- use_module(library(apply)).

:- use_module(md_trim).
:- use_module(md_links).
:- use_module(md_span_link).
:- use_module(md_span_decorate).
:- use_module(md_escape).
:- use_module(md_line).

%! md_span_string(+String, -HtmlTerms) is det.
%
% Same as md_span_codes/2 but uses a string
% ans input.

md_span_string(String, HtmlTerms):-
    string_codes(String, Codes),
    md_span_codes(Codes, HtmlTerms).

%! md_span_codes(+Codes, -HtmlTerms) is det.
%
% Turns the list of codes into a structure acceptable
% by SWI-Prolog's html//1 predicate. More info:
% http://www.swi-prolog.org/pldoc/doc_for?object=html/1

md_span_codes(Codes, HtmlTerms):-
    md_span_codes(Codes, [strong, em, code, del], HtmlTerms).

md_span_codes(Codes, Allow, Out):-
    phrase(span(Spans, Allow), Codes), !,
    phrase(atomize(Out), Spans).

% Optimized case for normal text.

span([Code1,Code2|Spans], Allow) -->
    [Code1,Code2],
    {
        code_type(Code1, alnum),
        code_type(Code2, alnum),
        Code1 \= 0'h,
        Code2 \= 0't
    }, !,
    span(Spans, Allow).

% Escape sequences.
% More info:
% http://daringfireball.net/projects/markdown/syntax#backslash
% Processed first.

span([Atom|Spans], Allow) -->
    "\\", [Code],
    {
        md_escaped_code(Code),
        atom_codes(Atom, [Code])
    }, !,
    span(Spans, Allow).

% Entities. These must be left alone.
% More info:
% http://daringfireball.net/projects/markdown/syntax#autoescape

span([\[Atom]|Spans], Allow) -->
    "&", string_limit(Codes, 10), ";",
    {
        maplist(alnum, Codes),
        append([0'&|Codes], [0';], Entity),
        atom_codes(Atom, Entity)
    }, !,
    span(Spans, Allow).

% Special characters & and <.
% More info:
% http://daringfireball.net/projects/markdown/syntax#autoescape

span(['&'|Spans], Allow) -->
    "&", !, span(Spans, Allow).

% As inline HTML is allowed, < is only escaped
% when the following character is not a letter and / or
% < appears at end of stream.

span(['<'|Spans], Allow) -->
    "<", lookahead(Code),
    {
        \+ code_type(Code, alpha),
        Code \= 47
    }, !,
    span(Spans, Allow).

span(['<'], _) -->
    "<", eos, !.

% Line break with two or more spaces.
% More info:
% http://daringfireball.net/projects/markdown/syntax#p

span([br([])|Spans], Allow) -->
    "  ", whites, ln, !,
    span(Spans, Allow).

% Recognizes links and images.

span([Link|Spans], Allow) -->
    lookahead(Code),
    {
        % performance optimization
        (   Code = 0'[
        ;   Code = 0'!
        ;   Code = 0'<
        ;   Code = 0'h)
    },
    md_span_link(Link), !,
    span(Spans, Allow).

% Recognizes <script ... </script>.
% Protects script contents from being processed as Markdown.

span([\[String]|Spans], Allow) -->
    "<script", string(Codes), "</script>", !,
    {
        string_codes(Content, Codes),
        atomics_to_string(['<script', Content, '</script>'], String)
    },
    span(Spans, Allow).

% Prevent in-word underscores to trigger
% emphasis.

span([Code, 0'_, 0'_|Spans], Allow) -->
    [Code], "__",
    { code_type(Code, alnum) }, !,
    span(Spans, Allow).

span([Code, 0'_|Spans], Allow) -->
    [Code], "_",
    { code_type(Code, alnum) }, !,
    span(Spans, Allow).

% Recognizes text stylings like
% strong, emphasis and inline code.

span([Span|Spans], Allow) -->
    lookahead(Code),
    {
        % performance optimization
        (   Code = 0'`
        ;   Code = 0'_
        ;   Code = 0'*
        ;   Code = 0'~)
    },
    md_span_decorate(Dec, Allow), !,
    {
        Dec =.. [Name, Codes],
        (   Name = code
        ->  string_codes(Atom, Codes),
            Span =.. [Name, Atom]
        ;   select(Name, Allow, AllowNest),
            md_span_codes(Codes, AllowNest, Nested),
            Span =.. [Name, Nested])
    },
    span(Spans, Allow).

span([Code|Spans], Allow) -->
    [Code], !,
    span(Spans, Allow).

span([], _) -->
    eos.

% Collects remaining codes into atoms suitable
% for SWI-s html//1.
% Atoms will appear as \[text] as they can contain
% raw HTML which must not be escaped.

atomize([]) -->
    eos, !.

atomize([\[Atom]|Tokens]) -->
    [Num], { number(Num) }, !,
    text_codes(Codes),
    { string_codes(Atom, [Num|Codes]) },
    atomize(Tokens).

atomize([Token|Tokens]) -->
    [Token], atomize(Tokens).

text_codes([Code|Codes]) -->
    [Code], { number(Code) }, !,
    text_codes(Codes).

text_codes([]) --> "".

% Recognizes single symbol code of
% type alnum.

alnum(Code):-
    code_type(Code, alnum).
