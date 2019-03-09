:- module(md_span_decorate, [
    md_span_decorate//2 % -Span
]).

/** <module> Parser for span-level styles

Predicates for recognizing span-level
style formatting like strong, emphasis and
code.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_trim).

%! md_span_decorate(-Span, +Allow)// is det.
%
% Recognizes style formatting
% in the middle of span text.
% Span is a term `functor(Codes)`
% where the functor is one of:
% `strong`, `em` or `code`.
% Allow is a list of allowed
% span elements. May contain
% `strong`, `em`, `del` and
% `code`.

md_span_decorate(Span, Allow) -->
    { memberchk(strong, Allow) },
    star_strong(Span), !.

md_span_decorate(Span, Allow) -->
    { memberchk(strong, Allow) },
    underscore_strong(Span), !.

md_span_decorate(Span, Allow) -->
    { memberchk(em, Allow) },
    star_emphasis(Span), !.

md_span_decorate(Span, Allow) -->
    { memberchk(em, Allow) },
    underscore_emphasis(Span), !.

md_span_decorate(Span, Allow) -->
    { memberchk(code, Allow) },
    code(Span).

md_span_decorate(Span, Allow) -->
    { memberchk(del, Allow) },
    strikethrough(Span).

% Recognizes strikethrough ~~something~~.

strikethrough(del(Codes)) -->
    "~~", string(Codes), "~~".

% Recognizes strong **something**.

star_strong(strong(Codes)) -->
    "**", string(Codes), "**".

% Recognizes strong __something__.

underscore_strong(strong(Codes)) -->
    "__", string(Codes), "__".

% Recognizes emhasis *something*.
% The first character following * must be a non-space.

star_emphasis(em([Code|Codes])) -->
    "*", nonblank(Code), string(Codes), "*".

% Recognizes emphasis _something_.
% The first character following _ must be a non-space.

underscore_emphasis(em([Code|Codes])) -->
    "_", nonblank(Code), string(Codes), "_".

% Recognizes inline code ``code``.

code(code(Trimmed)) -->
    "``", string(Raw), "``",
    { trim(Raw, Trimmed) }.

% Recognizes inline code `code`.

code(code(Trimmed)) -->
    "`", string(Raw), "`",
    { trim(Raw, Trimmed) }.
