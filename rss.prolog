:- module(rss, [make_rss/1]).

:- use_module('content/about').
:- use_module(git).

escape_char([], []).
% & --> &amp;
escape_char([0x26 | Tail], [[0x26, 0x61, 0x6d, 0x70, 0x3b] | Esc]) :-
    escape_char(Tail, Esc).
% < --> &lt;
escape_char([0x3c | Tail], [[0x26, 0x6c, 0x74, 0x3b] | Esc]) :-
    escape_char(Tail, Esc).
% > --> &gt;
escape_char([0x3e | Tail], [[0x26, 0x67, 0x74, 0x3b] | Esc]) :-
    escape_char(Tail, Esc).
% " --> &quot;
escape_char([0x22 | Tail], [[0x26, 0x71, 0x75, 0x6f, 0x74, 0x3b] | Esc]) :-
    escape_char(Tail, Esc).
% ' --> &apos;
escape_char([0x27 | Tail], [[0x26, 0x61, 0x70, 0x6f, 0x73, 0x3b] | Esc]) :-
    escape_char(Tail, Esc).
% and the rest is unchanged
escape_char([A | Tail], [A | Esc]) :- escape_char(Tail, Esc).

escape_xml_atom(Atom, Escaped) :-
    name(Atom, Code),
    escape_char(Code, Chunks),
    flatten(Chunks, EscapedCode),
    name(Escaped, EscapedCode).

rfc2822_date(IsoDate, Rfc2822Date) :-
    parse_time(IsoDate, Timestamp),
    format_time(atom(Rfc2822Date), '%a, %b %e, %Y', Timestamp).

commit_date_or_today(File, Date) :-
    (last_build_date_of_file(File, IsoDate),
     rfc2822_date(IsoDate, Date));
    (get_time(Timestamp),
     format_time(atom(Date), '%a, %b %e, %Y', Timestamp)).

build_date(Date) :-
    last_build_date_of_repo(IsoDate),
    rfc2822_date(IsoDate, Date).

rss_xml_header([
                      '<?xml version="1.0" encoding="utf-8"?>',
                      '<rss version="2.0">',
                      '<channel>',
                      '<title>', Title, '</title>',
                      '<link>', Link, '</link>',
                      '<description>', Description, '</description>',
                      '<lastBuildDate>', LastBuildDate, '</lastBuildDate>'
                  ]) :-
    about(title(UnescTitle),
          admin(_),
          domain(UnescDomain),
          abstract(UnescDesc)),
    escape_xml_atom(UnescTitle, Title),
    escape_xml_atom(UnescDomain, Domain),
    escape_xml_atom(UnescDesc, Description),
    atomic_list_concat(['//', Domain, '/'], Link),
    build_date(LastBuildDate).


rss_xml_footer(['</channel>', '</rss>']).


rss_item(entry(file(UnescBasename),
               title(UnescTitle),
               author(_),
               abstract(UnescDesc)),
         [
             '<item>',
             '<title>', Title, '</title>',
             '<link>', Link, '</link>',
             '<guid>', Guid, '</guid>',
             '<description>', Description, '</description>',
             '<pubDate>', PubDate, '</pubDate>',
             '</item>'
         ]) :-
    escape_xml_atom(UnescBasename, Basename),
    escape_xml_atom(UnescTitle, Title),
    escape_xml_atom(UnescDesc, Description),
    about(_,_,domain(Domain),_),
    atomic_list_concat(['http://', Domain, '/content/posts/', Basename], Link),
    Guid = Link,
    atom_concat('./content/posts/', Basename, Filepath),
    commit_date_or_today(Filepath, PubDate).

make_rss(RSS) :-
    rss_xml_header(Header),
    open('content/toc.data', read, Stream),
    read(Stream, ToC),
    maplist(rss_item, ToC, ItemList),
    flatten(ItemList, Items),
    rss_xml_footer(Footer),
    append(Header, Items, R),
    append(R, Footer, RssList),
    atomic_list_concat(RssList, RSS).

