:- module(toc_reader, [dissect_entry/7,
                       make_toc/3,
                       filter_toc_by_tag/3,
                       extract_tags_from_toc/2]).


:- use_module(timestamp).

resolve_author(me, Me) :- content:about:admin(Me).
resolve_author(X,X).
/*
dissect_entry(entry(file(Filename),
                    title(Title),
                    author(Author),
                    abstract(Abstract),
                    tags(_Tags)),
              Filename,
              Title,
              Author,
              Abstract,
              IsoDate) :-
    (post_path(Filename, MdPath),
     file_mod_date(MdPath, IsoDate));
    IsoDate = 'date unknown'.

dissect_entry(entry(file(Filename),
                    title(Title),
                    author(Author),
                    abstract(Abstract),
                    tags(_Tags),
                    date(Date)),
              Filename,
              Title,
              Author,
              Abstract,
              Date).
*/

dissect_entry(Entry,
              Filename,
              Title,
              Author,
              Abstract,
              Tags,
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
    (memberchk(date(Date), Entry);
     % if no date provided, check the modification date on the file
     (post_path(Filename, PostPath),
      file_mod_date(PostPath, Date));
     % failing that, put the beginning of the epoch, to tip the author off
     Date = '1970-01-01').





post_path(Filename, PostPath) :-
    user:file_search_path(posts, PostDir),
    atomic_list_concat([PostDir, '/', Filename], PostPath).

toc_entry_to_html(Entry,
                  [li(class(toc_title), a(href=MdPath, Title)),
                   div(class(toc_author),
                       ['by ', ResolvedAuthor, ' (', PrettyDate, ')']),
                   p(class(toc_abstract), Abstract),
                   p(span(class(toc_tag_line), ['TAGS: ' |TagLine]))]) :-
    dissect_entry(Entry, Filename, Title, Author, Abstract, Tags, IsoDate),
    toc_date(IsoDate, PrettyDate),
    post_path(Filename, MdPath),
    resolve_author(Author, ResolvedAuthor),
    maplist(make_tag, Tags, TagLine).

make_tag(Tag, span([a([href=HREF, class=toc_tag], UppercaseTag), ' '])) :-
    atom_concat('/tags/', Tag, HREF),
    upcase_atom(Tag, UppercaseTag).


make_toc(Path, Blocks, Tag) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    prepare_toc(Entries, Tag, Blocks).

prepare_toc(Entries, Tag, Blocks) :-
    filter_toc_by_tag(Entries, Tag, FilteredEntries), 
    %FilteredEntries = Entries,
    print_term(FilteredEntries, [output(user_error)]),
    sort_toc(FilteredEntries, SortedEntries),
    maplist(toc_entry_to_html, SortedEntries, BlockLists),
    flatten(BlockLists, Blocks).

compare_entries_by_date(Delta, E1, E2) :-
    dissect_entry(E1,_,_,_,_,_,Date1),
    dissect_entry(E2,_,_,_,_,_,Date2),
    parse_time(Date1, T1),
    parse_time(Date2, T2),
    compare(Delta, T2, T1).

sort_toc(Entries, SortedEntries) :-
    predsort(compare_entries_by_date, Entries, SortedEntries).

filter_toc_by_tag([], _, []).

filter_toc_by_tag(Entries, everything, Entries).

filter_toc_by_tag([E|Entries], Tag, [E|FilteredEntries]) :-
    memberchk(tags(T), E),
    memberchk(Tag, T),
    filter_toc_by_tag(Entries, Tag, FilteredEntries).

filter_toc_by_tag([_|Entries], Tag, FilteredEntries) :-
    filter_toc_by_tag(Entries, Tag, FilteredEntries).


uniq(Data, Uniques) :- sort(Data, Uniques).

extract_tag_lists([], []).
extract_tag_lists([E|Entries], [Ts|Tags]) :-
    memberchk(tags(Ts), E),
    extract_tag_lists(Entries, Tags).

extract_tag_lists([_|Entries], Tags) :-
    extract_tag_lists(Entries, Tags).

extract_tags(Entries, Tags) :-
    extract_tag_lists(Entries, Taglists),
    flatten(Taglists, TagsWithDupes),
    uniq(TagsWithDupes, Tags).

tag_item(TagSpan, li(TagSpan)).

extract_tags_from_toc(Path, TagBlocks) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    extract_tags(Entries, Tags),
    maplist(make_tag, Tags, TagSpans),
    maplist(tag_item, TagSpans, TagBlocks).


