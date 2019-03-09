:- module(md_blocks, [
    md_blocks//1 % -Blocks
]).

/** <module> Block-level parser for Markdown

Parses Markdown block-level constructs like
paragraphs, lists, code blocks, blockquotes etc.
Applies span-level parsing for all blocks.
*/

:- use_module(library(dcg/basics)).

:- use_module(md_list_item).
:- use_module(md_header).
:- use_module(md_span).
:- use_module(md_trim).
:- use_module(md_line).
:- use_module(md_hr).

%! md_blocks(-Blocks)// is det.
%
% Parses given Markdown into a structure
% accepted by html//1.

md_blocks(Blocks) -->
    blocks(top, [], Blocks).

% Contextified block parsing.
% Some types of blocks are not
% allowed in contexts other than top.
% Currently used contexts are: top,
% list and bq.

md_blocks(Ctx, Blocks) -->
    blocks(Ctx, [], Blocks).

% Recognizes all blocks
% in the input. When a block is not
% recognized, one line as removed and
% added into accumulator. These accumulated
% lines are added as paragraph blocks.
% This matches better the sematics of
% http://daringfireball.net/projects/markdown/dingus

blocks(Ctx, Acc, Result) -->
    empty_lines,
    block(Ctx, Block), !,
    {
        (   Acc = []
        ->  Result = [Block|Blocks]
        ;   acc_block(Acc, AccBlock),
            Result = [AccBlock,Block|Blocks])
    },
    blocks(Ctx, [], Blocks).

blocks(Ctx, Acc, Blocks) -->
    non_empty_line(Line), !,
    blocks(Ctx, [Line|Acc], Blocks).

blocks(_, Acc, Result) -->
    empty_lines,
    eos, !,
    {
        (   Acc = []
        ->  Result = []
        ;   Result = [Block],
            acc_block(Acc, Block))
    }.

blocks(Ctx, Acc, Result) -->
    empty_line,
    {
        (   Acc = []
        ->  Result = Blocks
        ;   Result = [Block|Blocks],
            acc_block(Acc, Block))
    },
    blocks(Ctx, [], Blocks).

% Converts lines into a <p>
% element and applies span-level
% parsing.

acc_block(Acc, p(Span)):-
    reverse(Acc, AccLines),
    merge_lines(AccLines, Block),
    md_span_codes(Block, Span).

% Recognizes a single block.
% Tries to parse in the following
% order: headers, horisontal ruler,
% lists, blockquote, html.

block(_, Block) -->
    md_header(Block), !.

block(_, Block) -->
    code(Block), !.

block(top, hr([])) -->
    md_hr, !.

block(_, Block) -->
    list(Block), !.

block(top, Block) -->
    blockquote(Block), !.

block(_, Block) -->
    html(Block), !.

block(_, Block) -->
    fenced_code(Block).

code(pre(code(String))) -->
    indented_lines(Codes), !,
    {
        trim_right(Codes, Trimmed),
        string_codes(String, Trimmed)
    }.

% Recognizes fenced code blocks.
% The language is put into the
% `data-language` attribute of the
% `code` tag.

fenced_code(Block) -->
    "```", inline_string(LangCodes), ln,
    string(Codes),
    ln, "```", whites, ln_or_eos, !,
    {
        trim(LangCodes, Trimmed),
        atom_codes(Lang, Trimmed),
        string_codes(Code, Codes),
        (   Lang = ''
        ->  Block = pre(code(Code))
        ;   Block = pre(code(['data-language'=Lang], Code)))
    }.

% Optimizes generated HTML structure.
% Applied after parsing different blocks.
% Mostly deals with excessive <p> elements
% removal.

optimize(blockquote([p(Block)]), blockquote(Block)):- !.

optimize(li([p(Block)]), li(Block)):- !.

optimize(li([p(Block1), ul(Block2)]), li(Block)):- !,
    append(Block1, [ul(Block2)], Block).

optimize(li([p(Block1), ol(Block2)]), li(Block)):- !,
    append(Block1, [ol(Block2)], Block).

optimize(Block, Block).

% Recognizes a sequence of one or more
% indented lines. Gives back codes of
% whole sequence.

indented_lines(Codes) -->
    indented_lines_collect(Lines),
    {
        Lines \= [],
        merge_lines(Lines, Codes)
    }.

% Recognizes a sequence of indented lines.
% There might be empty lines between
% indented lines.

indented_lines_collect([Line|Lines]) -->
    indented_line(Line), !,
    indented_lines_collect(Lines).

indented_lines_collect([]) -->
    eos, !.

indented_lines_collect([[]|Lines]) -->
    empty_line, !,
    indented_lines_collect(Lines).

indented_lines_collect([]) --> "".

indented_line(Line) -->
    indent, inline_string(Line), ln_or_eos.

% Recognizes block-level HTML.
% No Markdown inside it is processed.
% Gives term that write_html's html//1
% does not escape.

html(\[String]) -->
    [0'<, Code], { code_type(Code, alpha) }, !,
    non_empty_lines(Html),
    { string_codes(String, [0'<,Code|Html]) }.

% Recognizes either ordered list
% or bulleted list.

list(List) -->
    bullet_list(List), !.

list(List) -->
    ordered_list(List).

% Recognizes ordered list.
% Gives term like ol(Term)
% where Items is non-empty list.

ordered_list(ol(Items)) -->
    ordered_list_collect(Items, _), !,
    { Items \= [] }.

ordered_list_collect([Item|Items], Mode) -->
    ordered_list_item(Item, Mode), !,
    empty_lines,
    ordered_list_collect(Items, Mode).

ordered_list_collect([], _) --> "".

% Recognizes a single ordered list item.

ordered_list_item(Item, ListMode) -->
    md_ordered_list_item(Codes, ItemMode),
    { postproc_list_item(Codes, ItemMode, ListMode, Item) }.

% Recognizes bulleted list.
% Gives a term like ul(Items)
% where Items is non-empty list.

bullet_list(ul(Items)) -->
    bullet_list_collect(Items, _), !,
    { Items \= [] }.

bullet_list_collect([Item|Items], Mode) -->
    bullet_list_item(Item, Mode), !,
    empty_lines,
    bullet_list_collect(Items, Mode).

bullet_list_collect([], _) --> "".

% Recognizes a single bulleted list item.

bullet_list_item(Item, ListMode) -->
    md_bullet_list_item(Codes, ItemMode),
    { postproc_list_item(Codes, ItemMode, ListMode, Item) }.

% Postprocesses a list item.
% In paragraph mode, no optimizations are
% applied (preserves `<p>` in `<li>`).
% The actual list mode is set by the first
% list item.

postproc_list_item(Codes, ItemMode, ListMode, Item):-
    phrase(md_blocks(list, Blocks), Codes),
    list_mode(ListMode, ItemMode, Mode),
    (   Mode = normal
    ->  optimize(li(Blocks), Item)
    ;   Item = li(Blocks)).

% List mode setup. When ListMode
% is set, its value is used. Otherwise
% ListMode is set to ItemMode.

list_mode(ListMode, ItemMode, Mode):-
    (   var(ListMode)
    ->  ListMode = ItemMode
    ;   true),
    Mode = ListMode.

% Recognizes a blockquote.
% Strips > from line beginnings.
% Output is a term like blockquote(Blocks).

blockquote(Opt) -->
    ">", string(Codes),
    empty_line,
    empty_line, !,
    {
        trim_left(Codes, Trimmed),
        phrase(bq_strip(Stripped), Trimmed), !,
        phrase(md_blocks(top, Blocks), Stripped),
        optimize(blockquote(Blocks), Opt)
    }.

% Strips > from blockquote line
% beginnings.

bq_strip([0'\n|Codes]) -->
    ln, "> ", !, bq_strip(Codes).

bq_strip([0'\n|Codes]) -->
    ln, ">", !, bq_strip(Codes).

bq_strip([Code|Codes]) -->
    [Code], !, bq_strip(Codes).

bq_strip([]) -->
    eos.

% List of consequtive non-empty lines.
% Consumes as many non-empty lines
% as possible. Gives flattened list
% of codes.

non_empty_lines(Codes) -->
    non_empty_lines_collect(Lines),
    { merge_lines(Lines, Codes) }, !.

non_empty_lines_collect([Line|Lines]) -->
    non_empty_line(Line), !,
    non_empty_lines_collect(Lines).

non_empty_lines_collect([]) --> "".
