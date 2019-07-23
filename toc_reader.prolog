:- module(toc_reader, [dissect_entry/8,
                       read_toc/2,
                       make_toc/3,
                       make_tag/2,
                       filter_toc_by_tag/3,
                       filter_toc/3,
                       filter_toc/2,
                       extract_tags_from_toc/2,
                       get_file_entry/3,
                       lookup_file/2,
                       filter_toc_no_drafts/2]).

:- use_module('content/tag_order').

:- use_module(timestamp).

resolve_author(me, Me) :- content:about:admin(Me).
resolve_author(X,X).

dissect_entry(Entry,
              Filename,
              Title,
              Author,
              Abstract,
              Tags,
              WordCount,
              Date) :-
    memberchk(file(Filename), Entry),
    memberchk(title(Title), Entry),
    ((memberchk(author(UnresolvedAuthor), Entry),
      resolve_author(UnresolvedAuthor, Author));
     % if no author field, default to admin
     content:about:admin(Author)),
    (memberchk(tags(Tags), Entry);
     % if no tags provided, assume empty list of tags
     Tags = []),
    (memberchk(abstract(Abstract), Entry);
     % if no abstract provided, assume no abstract
     Abstract = ''),
    post_file_path(Filename, PostPath),
    count_words(PostPath, WordCount),
    date_of_entry(Entry, Date),
    !.

date_of_entry(Entry, Date) :-
    memberchk(date(Date), Entry).

date_of_entry(Entry, Date) :-
    memberchk(file(Filename), Entry),
    last_build_date_of_file(posts, Filename, Date).

date_of_entry(Entry, Date) :-
    memberchk(file(Filename), Entry),
    post_file_path(Filename, Path),
    file_mod_date(Path, Date).

date_of_entry(_Entry, '1970-01-01').

post_uri(Filename, URI) :-
    http:location(posts, PostDir, []),
    atomic_list_concat([PostDir, '/', Filename], URI).

post_file_path(Filename, PostPath) :-
    user:file_search_path(posts, PostDir),
    atomic_list_concat([PostDir, '/', Filename], PostPath).

toc_entry_to_html(Entry,
                  [
                      div(class=toc_entry, [
                              li(class(toc_title),
                                 span([a(href=HREF, Title), Date])),
                              div(class(toc_author), Byline),
                              p(class(toc_abstract), Abstract),
                              div(span(class(toc_tag_line), [TagPrefix|TagLine]))
                          ]),
                      hr(class=toc_hr)
                  ]) :-
    dissect_entry(Entry, Filename, Title, Author, Abstract, Tags, WordCount, IsoDate),
    toc_date(IsoDate, PrettyDate),
    post_uri(Filename, HREF),
    resolve_author(Author, ResolvedAuthor),
    format(atom(Date), '  (~s)', PrettyDate),
    format(atom(Byline), 'by ~s (~d words)', [ResolvedAuthor, WordCount]),
    TagPrefix = 'TAGS: ',
    maplist(make_tag, Tags, TagLine),
    !.

make_tag(Tag, span([a([href=HREF, class=toc_tag], UppercaseTag), ' '])) :-
    atom_concat('/tags/', Tag, HREF),
    upcase_atom(Tag, UppercaseTag),
    !.

% Cache the ToC.
% this means that the server will need to be restarted to refresh it.
read_toc(_Path, Entries) :-
    nb_current(toc, Entries),
    !.

read_toc(Path, Entries) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    close(Stream),
    nb_setval(toc, Entries),
    !.

make_toc(Path, Blocks, Tag) :-
    read_toc(Path, Entries),
    prepare_toc(Entries, Tag, Blocks),
    !.

filter_toc(Entries, Filtered) :-
    filter_toc(Entries, everything, Filtered).

filter_toc(Entries, Tag, Filtered) :-
    get_time(Now),
    filter_toc_by_tag(Entries, Tag, F1),
    (
        Tag == draft -> F3 = F1;
        filter_toc_no_drafts(F1, F2),
        (
            Tag == the_future -> F3 = F2;
            filter_toc_no_future(F2, Now, F3)
        )
    ),
    sort_toc(F3, Filtered).

prepare_toc(Entries, Tag, [hr(class=toc_hr) | Blocks]) :-
    filter_toc(Entries, Tag, FilteredEntries),
    maplist(toc_entry_to_html, FilteredEntries, BlockLists),
    flatten(BlockLists, Blocks).

compare_entries_by_date(Delta, E1, E2) :-
    dissect_entry(E1,_,_,_,_,_,_,Date1),
    dissect_entry(E2,_,_,_,_,_,_,Date2),
    parse_time(Date1, T1),
    parse_time(Date2, T2),
    time_compare(Delta, T2, T1).

time_compare(Delta, T1, T2) :-
    T1 =:= T2 -> Delta = '<';
    compare(Delta, T1, T2).

sort_toc(Entries, SortedEntries) :-
    predsort(compare_entries_by_date, Entries, SortedEntries).

%%
% Tags form a join-semilattice
%%

% transitivity
supertag(Tag1, Tag2) :-
    content:tag_order:super(T, Tag2),
    supertag(Tag1, T).

% reflexivity
supertag(Tag, Tag).

filter_toc_by_tag([], _, []).

filter_toc_by_tag(Entries, everything, Entries).
filter_toc_by_tag(Entries, the_future, Entries).

filter_toc_by_tag([E|Entries], Tag, [E|FilteredEntries]) :-
    memberchk(tags(T), E),
    supertag(Tag, Subtag),
    memberchk(Subtag, T),
    filter_toc_by_tag(Entries, Tag, FilteredEntries).

filter_toc_by_tag([_|Entries], Tag, FilteredEntries) :-
    filter_toc_by_tag(Entries, Tag, FilteredEntries).

filter_toc_no_drafts([],[]).

filter_toc_no_drafts([E|Entries], FilteredEntries) :-
    memberchk(tags(T), E),
    memberchk(draft, T),
    filter_toc_no_drafts(Entries, FilteredEntries).

filter_toc_no_drafts([E|Entries], [E|FilteredEntries]) :-
    filter_toc_no_drafts(Entries, FilteredEntries).

filter_toc_no_future([],_,[]).

filter_toc_no_future([E|Entries], Now, FilteredEntries) :-
    % date is > Now
    memberchk(date(Date), E),
    parse_time(Date, Timestamp),
    Timestamp > Now,
    filter_toc_no_future(Entries, Now, FilteredEntries).

filter_toc_no_future([E|Entries], Now, [E|FilteredEntries]) :-
    filter_toc_no_future(Entries, Now, FilteredEntries).

uniq(Data, Uniques) :- sort(Data, Uniques).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create the Tag Index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cache the tag index
extract_tags_from_toc(_, ul(TagBlocks)) :-
    nb_current(tag_index, TagBlocks).

extract_tags_from_toc(Path, ul(TagBlocks)) :-
    read_toc(Path, Entries),
    filter_toc_no_drafts(Entries, NoDrafts),
    extract_tags(NoDrafts, Tags),
    filter_suprema(Tags, TopTags),
    maplist(tag_item, TopTags, TagBlocks),
    nb_setval(tag_index, TagBlocks).

extract_tag_lists([], []).

extract_tag_lists([E|Entries], [Ts|Tags]) :-
    memberchk(tags(Ts), E),
    extract_tag_lists(Entries, Tags).

extract_tag_lists([_|Entries], Tags) :-
    extract_tag_lists(Entries, Tags).

filter_has_at_least_two_subs([], []).

filter_has_at_least_two_subs([T|Tags], [T|Supertags]) :-
    super(T, X),
    super(T, Y),
    X \= Y,
    filter_has_at_least_two_subs(Tags, Supertags).

filter_has_at_least_two_subs([_|Tags], Supertags) :-
    filter_has_at_least_two_subs(Tags, Supertags).

extract_tags(Entries, Tags) :-
    extract_tag_lists(Entries, Taglists),
    flatten(Taglists, TagsWithDupes),
    findall(T, super(T, _), Supertags),
    filter_has_at_least_two_subs(Supertags, Supertags2),
    append(TagsWithDupes, Supertags2, TagsWithDupes2),
    sort(TagsWithDupes2, SortedTags),
    uniq(SortedTags, Tags).

filter_suprema(Tags, TopTags) :-
    filter_suprema_(Tags, Tags, TopTags).

filter_suprema_([],_,[]).

filter_suprema_([T|Tags], TagsInUse, TopTags) :-
    content:tag_order:super(S, T),
    memberchk(S, TagsInUse),
    filter_suprema_(Tags, TagsInUse, TopTags).

filter_suprema_([T|Tags], TagsInUse, [T|TopTags]) :-
    filter_suprema_(Tags, TagsInUse, TopTags).

get_strict_subtags(Tag, Subtags) :-
    findall(T, content:tag_order:super(Tag, T), Subtags).

get_subtags(Tag, Subtags) :-
    findall(T, supertag(Tag, T), Subtags).

tag_item(Tag, li(span(class=taglist_item,
                      [
                          TagSpan
                          | ul(SubtagItems)
                      ]))) :-
    make_tag(Tag, TagSpan),
    get_strict_subtags(Tag, Subtags),
    sort(Subtags, SortedSubtags),
    maplist(tag_item, SortedSubtags, SubtagItems).

% note: this will include subtags that refer to hidden drafts


%%%%%%%%%%%%%%%%%%%

%count_words(Path, Num) :-
%    nb_current(Path, Num). % check to see if we've already cached the result

count_words(Path, Num) :-
    open(Path, read, Stream),
    read_string(Stream, _, Text),
    split_string(Text, " \n\t#", " \n\t#", Words),
    length(Words, Num),
    close(Stream),
    !.
%    nb_setval(Path, Num).


get_file_entry(F, [E|_], E) :-
    memberchk(file(F), E).

get_file_entry(F, [_|Entries], X) :-
    get_file_entry(F, Entries, X).

lookup_file(Basename, Entry) :-
    read_toc('content/toc.data', Entries),
    get_file_entry(Basename, Entries, Entry),
    !.

%%

reset_toc_globals :-
    nb_delete(toc),
    nb_delete(tag_index).
