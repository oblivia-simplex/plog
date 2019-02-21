:- module(toc_reader, [dissect_entry/6,
                       make_toc/2]).

:- use_module(timestamp).


dissect_entry(entry(file(Filename),
                    title(Title),
                    author(Author),
                    abstract(Abstract)),
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
                    date(Date)),
              Filename,
              Title,
              Author,
              Abstract,
              Date).

post_path(Filename, PostPath) :-
    user:file_search_path(posts, PostDir),
    atomic_list_concat([PostDir, '/', Filename], PostPath).

toc_entry_to_html(Entry,
                  [li(class(toc_title), a(href=MdPath, Title)),
                   div(class(toc_author),
                       ['by ', ResolvedAuthor, ' (', PrettyDate, ')']),
                   p(class(toc_abstract), Abstract)]) :-
    dissect_entry(Entry, Filename, Title, Author, Abstract, IsoDate),
    toc_date(IsoDate, PrettyDate),
    post_path(Filename, MdPath),
    resolve_author(Author, ResolvedAuthor).

make_toc(Path, Blocks) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    sort_toc(Entries, SortedEntries),
    maplist(toc_entry_to_html, SortedEntries, BlockLists),
    flatten(BlockLists, Blocks).

compare_entries_by_date(Delta, E1, E2) :-
    dissect_entry(E1,_,_,_,_,Date1),
    dissect_entry(E2,_,_,_,_,Date2),
    parse_time(Date1, T1),
    parse_time(Date2, T2),
    compare(Delta, T2, T1).

sort_toc(Entries, SortedEntries) :-
    predsort(compare_entries_by_date, Entries, SortedEntries).
