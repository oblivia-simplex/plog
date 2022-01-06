:- module(rss, [make_rss/1, make_sitemap/1]).

:- use_module('../about').
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
    atomic_list_concat(['http://', Domain, '/posts/', Basename], Link),
    Guid = Link.

% Drafts are excluded from the RSS feed.
%% make_rss(RSS) :-
%%     nb_current(rss, RSS). % cache

make_rss(RSS) :-
    rss_xml_header(Header),
    assemble_toc('./../posts/', Entries),
    %write(user_error, Entries),
    filter_toc(Entries, ToC),
    maplist(rss_item, ToC, ItemList),
    flatten(ItemList, Items),
    rss_xml_footer(Footer),
    append(Header, Items, R),
    append(R, Footer, RssList),
    atomic_list_concat(RssList, RSS),
    nb_setval(rss_feed, RSS).

% The sitemap for a blog will be pretty similar to its RSS feed, so let's handle
% that here, too.

get_url(Dir, Basename, Url) :-
    content:about:domain(Domain),
    atomic_list_concat(['http://', Domain, '/', Dir, '/', Basename], Url).

sitemap_info(Basename,
             [
                 '<url>',
                 '<loc>', Location, '</loc>',
                 '<lastmod>', LastMod, '</lastmod>',
                 '<changefreq>', 'weekly', '</changefreq>',
                 '<priority>', '0.4', '</priority>',
                 '</url>'
             ]) :-
    get_url(info, Basename, Location),
    (
        last_build_date_of_file(info, Basename, BuildIsoDate);
        atom_concat('./../info/', Basename, Path),
        file_mod_date(Path, BuildIsoDate)
    ),
    sitemap_date(BuildIsoDate, LastMod).

sitemap_home([
                    '<url>',
                    '<loc>', HomeUrl, '</loc>',
                    '<lastmod>', TocLastMod, '</lastmod>',
                    '<changefreq>', 'daily', '</changefreq>',
                    '<priority>', '0.6', '</priority>',
                    '</url>'
                ]) :-
    content:about:domain(Domain),
    atom_concat('http://', Domain, HomeUrl),
    (
        last_build_date_of_file('.', 'toc.data', BuildIsoDate);
        file_mod_date('../toc.data', BuildIsoDate)
    ),
    sitemap_date(BuildIsoDate, TocLastMod).

sitemap_item(Entry,
             [
                 '<url>',
                 '<loc>', Location, '</loc>',
                 '<lastmod>', LastMod, '</lastmod>',
                 '<changefreq>', 'weekly', '</changefreq>',
                 '<priority>', '1.0', '</priority>',
                 '</url>'
             ]) :-
    dissect_entry(Entry, UnescBasename, _UnescTitle,
                  _Author, _UnescDesc, _Tags, _WordCount, _IsoDate),
    (
        last_build_date_of_file(posts, UnescBasename, BuildIsoDate);
        atom_concat('./../posts/', UnescBasename, Path),
        file_mod_date(Path, BuildIsoDate)
    ),
    sitemap_date(BuildIsoDate, LastMod),
    get_url(posts, UnescBasename, UnescLocation),
    escape_xml_atom(UnescLocation, Location).

make_sitemap(Sitemap) :-
    nb_current(sitemap, Sitemap).

make_sitemap(Sitemap) :-
    sitemap_xml_header(Header),
    assemble_toc('./../posts/', Entries),
    filter_toc(Entries, ToC),
    maplist(sitemap_item, ToC, ItemList),
    sitemap_home(Home),
    sitemap_info('about.md', About),
    sitemap_info('links.md', Links),
    sitemap_info('license.md', License),
    WithInfo = [Home, About, Links, License | ItemList],
    flatten(WithInfo, Items),
    sitemap_xml_footer(Footer),
    append(Header, Items, S),
    append(S, Footer, SitemapList),
    atomic_list_concat(SitemapList, Sitemap),
    nb_setval(sitemap, Sitemap).

sitemap_xml_header(
    [
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">'
    ]).

sitemap_xml_footer(['</urlset>']).
