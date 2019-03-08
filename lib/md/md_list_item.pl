:- module(md_list_item, [
    md_bullet_list_item//2, % -Codes
    md_ordered_list_item//2 % -Codes
]).

/** <module> List item parser

Parser for items of bulleted and ordered lists.
Separated into its own module for code clarity.
*/

:- use_module(library(dcg/basics)).
:- use_module(md_line).
:- use_module(md_hr).

%! md_bullet_list_item(+Codes, -Mode)// is det.
%
% Recognizes a single bulleted list item.

% Lookahead for horisontal ruler prevents
% recognizing * * * as a list item.

md_bullet_list_item(Codes, Mode) -->
    (   md_lookahead_hr
    ->  { fail }
    ;   bullet_start(Indent, _), whites, !,
        list_item_unintented(Indent, Codes, Mode)).

%! md_ordered_list_item(-Codes, -Mode)// is det.
%
% Recognizes a single ordered list item.

md_ordered_list_item(Codes, Mode) -->
    ordered_start(Indent, _), whites, !,
    list_item_unintented(Indent, Codes, Mode).

% Bulleted-list item start.
% Gives codes that make up the start.

bullet_start(Indent, Codes) -->
    item_indent(Indent),
    list_bullet(Bullet),
    marker_follow(Follow),
    { flatten([Indent, Bullet, Follow], Codes) }.

% Ordered-list item start.
% Gives codes that make up the start.

ordered_start(Indent, Codes) -->
    item_indent(Indent),
    one_or_more_digits(Number), ".",
    marker_follow(Follow),
    { flatten([Indent, Number, [0'.|Follow]], Codes) }.

% Looks ahead an item start.
% Used for detecting where the
% previous list item ends.

lookahead_item_start(Indent), Codes -->
    item_start(Indent, Codes).

item_start(Indent, Codes) -->
    bullet_start(Indent, Codes), !.

item_start(Indent, Codes) -->
    ordered_start(Indent, Codes).

% List bullet might be indented
% with up to 3 spaces.

item_indent([0' ,0' ,0' ]) --> "   ".
item_indent([0' ,0' ]) --> "  ".
item_indent([0' ]) --> " ".
item_indent([]) --> "".

% List item marker must be followed
% by a space or tab.

marker_follow([0' ]) --> " ".
marker_follow([0'\t]) --> "\t".

% Sames as list_item_text but
% removes possible indentation.

list_item_unintented(Indent, Codes, Mode) -->
    list_item_text(Indent, Indented, Mode), !,
    {
        (   phrase(find_indent(BodyIndent), Indented, _)
        ->  phrase(strip_indent(BodyIndent, Codes), Indented)
        ;   Codes = Indented)
    }.

% Recognizes list item body and mode.
% Mode can be either normal or para.
% This is implemented by recognizing
% end conditions first.

list_item_text(Indent, [], Mode) -->
    list_item_end(Indent, Mode), !.

% Other cases, just consume input.

list_item_text(Indent, [Code|Codes], Mode) -->
    [Code], list_item_text(Indent, Codes, Mode).

% Recognizes list item end and
% item mode.

list_item_end(_, normal) -->
    eos.

% Item and next item are separated
% with an empty line.

list_item_end(Indent, para) -->
    ln, empty_line,
    lookahead_item_start(StartIndent),
    {
        length(Indent, I),
        length(StartIndent, S),
        S =< I
    }.

% No empty line before next item.

list_item_end(Indent, normal) -->
    ln, lookahead_item_start(StartIndent),
    {
        length(Indent, I),
        length(StartIndent, S),
        S =< I
    }.

% Next line is horisontal ruler.

list_item_end(_, normal) -->
    ln, md_lookahead_hr.

% Empty line and next line has
% no indent.

list_item_end(_, normal) -->
    ln, empty_line, lookahead_no_indent.

% Looks ahead non-indented line begin.

lookahead_no_indent -->
    lookahead_no_white.

lookahead_no_white, [Code] -->
    [Code], { \+ code_type(Code, white) }.

% Recognizes bulleted list item
% token.

list_bullet(0'*) --> "*".
list_bullet(0'-) --> "-".
list_bullet(0'+) --> "+".

% Recognizes sequence of
% one or more digits. Used for
% recognizing ordered list items.

one_or_more_digits([Digit]) -->
    digit(Digit).

one_or_more_digits([Digit|Digits]) -->
    digit(Digit),
    one_or_more_digits(Digits).

% Detects indent from the second
% line.

find_indent(Indent) -->
    non_empty_line(_),
    detect_indent(Indent).

detect_indent([0'\t]) --> "\t".

detect_indent([0' ,0' ,0' ,0' ]) --> "    ".

detect_indent([0' ,0' ,0' ]) --> "   ".

detect_indent([0' ,0' ]) --> "  ".

detect_indent([0' ]) --> " ".

detect_indent([]) --> "".

% Strips indent
% from line beginnings.

strip_indent(BodyIndent, Codes) -->
    strip_indent_begin(BodyIndent, Codes).

strip_indent_begin(BodyIndent, Codes) -->
    strip_line_indent(BodyIndent), !,
    strip_rest_indent(BodyIndent, Codes).

strip_indent_begin(BodyIndent, Codes) -->
    strip_rest_indent(BodyIndent, Codes).

strip_rest_indent(BodyIndent, [0'\n|Codes]) -->
    ln, strip_line_indent(BodyIndent), !,
    strip_rest_indent(BodyIndent, Codes).

strip_rest_indent(BodyIndent, [Code|Codes]) -->
    [Code], !, strip_rest_indent(BodyIndent, Codes).

strip_rest_indent(_, []) -->
    eos.

% Strip a tab when the target indent
% was also a tab.

strip_line_indent([0'\t]) --> "\t".

% Strip a tab when the target indent
% was 4 spaces.

strip_line_indent([0' ,0' ,0' ,0' ]) --> "\t".

% Strip 4 spaces when the target indent
% was 4 spaces.

strip_line_indent([0' ,0' ,0' ,0' ]) --> "    ".

% Strip 4 spaces when the target indent
% was a tab.

strip_line_indent([0'\t]) --> "    ".

% Strip 3 spaces when the target indent
% was at least 3 spaces.

strip_line_indent([0' ,0' ,0' |_]) --> "   ".

% Strip 2 spaces when the target indent
% was 2 spaces.

strip_line_indent([0' ,0' |_]) --> "  ".

% Strip a space when the target indent
% was at least 1 space.

strip_line_indent([0' |_]) --> " ".
