:- module(rss, [make_rss/1]).

:- use_module('content/about').
:- use_module(timestamp).
:- use_module(toc_reader).

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

rss_xml_header([
                      '<?xml version="1.0" encoding="utf-8"?>',
                      '<rss version="2.0">',
                      '<channel>',
                      '<title>', Title, '</title>',
                      '<link>', Link, '</link>',
                      '<description>', Description, '</description>',
                      '<lastBuildDate>', LastBuildDate, '</lastBuildDate>'
                  ]) :-
    content:about:title(UnescTitle),
    content:about:domain(UnescDomain),
    content:about:abstract(UnescDesc),
    escape_xml_atom(UnescTitle, Title),
    escape_xml_atom(UnescDomain, Domain),
    escape_xml_atom(UnescDesc, Description),
    atomic_list_concat(['//', Domain, '/'], Link),
    rfc2822_build_date(LastBuildDate).


rss_xml_footer(['</channel>', '</rss>']).


rss_item(Entry,
         [
             '<item>',
             '<title>', Title, '</title>',
             '<link>', Link, '</link>',
             '<guid>', Guid, '</guid>',
             '<description>', Description, '</description>',
             '<pubDate>', PubDate, '</pubDate>',
             '</item>'
         ]) :-
    dissect_entry(Entry, UnescBasename, UnescTitle, _Author, UnescDesc, _Tags, _WordCount, IsoDate),
    rfc2822_date(IsoDate, PubDate),
    escape_xml_atom(UnescBasename, Basename),
    escape_xml_atom(UnescTitle, Title),
    escape_xml_atom(UnescDesc, Description),
    content:about:domain(Domain),
    atomic_list_concat(['http://', Domain, '/content/posts/', Basename], Link),
    Guid = Link.

% Drafts are excluded from the RSS feed.
make_rss(RSS) :-
    rss_xml_header(Header),
    open('content/toc.data', read, Stream),
    read(Stream, Entries),
    filter_toc_no_drafts(Entries, ToC),
    maplist(rss_item, ToC, ItemList),
    flatten(ItemList, Items),
    rss_xml_footer(Footer),
    append(Header, Items, R),
    append(R, Footer, RssList),
    atomic_list_concat(RssList, RSS).

