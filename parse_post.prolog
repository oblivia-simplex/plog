:- module(parse_post, [parse_post/2,
                       parse_meta/2,
                       parse_post_with_meta/3,
                       assemble_toc/2]).

:- use_module(lib/yaml/parser).
:- use_module(lib/md/md_parse).
% This is a bit of a hack.

% First read the file as lines, then pick out the initial yaml block,
% then parse that. Return the remainder either raw or parsed as markdown.

lines(Stream, []) :-
    at_end_of_stream(Stream).

lines(Stream, [H|T]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, S),
    atom_string(H, S),
    lines(Stream, T).

lines_from_file(Filename, Lines) :-
    open(Filename, read, Stream),
    lines(Stream, Lines),
    close(Stream),
    !.


metadata_block([], [], _, []).

metadata_block(['---'|T], Meta, false, Remainder) :-
    metadata_block(T, Meta, true, Remainder),
    !.

% closing
metadata_block(['---'|Remainder], [], true, Remainder) :- !.

metadata_block([H|T], [H|Meta], true, Remainder) :-
    H \= '---',
    metadata_block(T, Meta, true, Remainder),
    !.


raw_yaml_and_markdown(Filename, Meta, Remainder) :-
    lines_from_file(Filename, Lines),
    metadata_block(Lines, MetaLines, false, RemainderLines),
    atomic_list_concat(MetaLines, '\n', Meta),
    atomic_list_concat(RemainderLines, '\n', Remainder),
    !.


entry_from_meta(Path, Meta, [file(Basename),
                           title(Meta.title),
                           author(Meta.author),
                           abstract(Meta.abstract),
                           date(TimeStamp),
                           tags(Meta.tags)]) :-
    date_time_stamp(Meta.date, TimeStamp),
    file_base_name(Path, Basename).


% (+Filename, -Html, -Entry)
parse_post_with_meta(Filename, Html, Entry) :-
    raw_yaml_and_markdown(Filename, RawYaml, RawMarkdown),
    parser:parse(RawYaml, Meta),
    entry_from_meta(Filename, Meta, Entry),
    atom_codes(RawMarkdown, MarkdownCodes),
    md_parse_codes(MarkdownCodes, Html),
    !.


% (+Filename, -Entry)
parse_meta(Filename, Entry) :-
    raw_yaml_and_markdown(Filename, RawYaml, _RawMarkdown),
    parser:parse(RawYaml, Meta),
    entry_from_meta(Filename, Meta, Entry).
    

% (+Filename, -Html)
parse_post(Filename, Html) :-
    (raw_yaml_and_markdown(Filename, _RawYaml, RawMarkdown),
     atom_codes(RawMarkdown, MarkdownCodes),
     md_parse_codes(MarkdownCodes, Html)); % maybe there's no header. fallback
    format(user_error, 'No header found for ~s, falling back to legacy mode\n', Filename),
    md_parse_file(Filename, Html).


parse_meta_list([], []).

parse_meta_list([File|Files], Entries) :-
    parse_meta_list(Files, RemainingEntries),
    (parse_meta(File, Entry)
    -> Entries = [Entry | RemainingEntries]
    ; Entries = RemainingEntries).

is_md(Filename) :- wildcard_match('*.md', Filename).


fullpaths(_, [], []) :- !.

fullpaths(Dir, [File|Fs], [Path|Ps]) :-
    atomic_list_concat([Dir, '/', File], Path),
    fullpaths(Dir, Fs, Ps),
    !.

assemble_toc(Dir, Entries) :-
    directory_files(Dir, Files),
    include(is_md, Files, MarkdownFiles),
    fullpaths(Dir, MarkdownFiles, Paths),
    parse_meta_list(Paths, Entries),
    !.
