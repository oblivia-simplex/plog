:- module(rss, [make_rss/1]).

:- use_module('content/about').
:- use_module(git).


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
    about(title(Title),
          admin(_),
          domain(Domain),
          abstract(Description)),
    atomic_list_concat(['//', Domain, '/'], Link),
    build_date(LastBuildDate).


rss_xml_footer(['</channel>', '</rss>']).


rss_item(entry(file(Basename),
               title(Title),
               author(_),
               abstract(Description)),
         [
             '<item>',
             '<title>', Title, '</title>',
             '<link>', Link, '</link>',
             '<guid>', Guid, '</guid>',
             '<description>', Description, '</description>',
             '<pubDate>', PubDate, '</pubDate>',
             '</item>'
         ]) :-
    about(_,_,domain(Domain),_),
    atomic_list_concat(['http://', Domain, '/posts/', Basename], Link),
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

