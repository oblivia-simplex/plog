:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).

% http_reply_from_files is here
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dirindex)).
% for the DCG stuff, I think
:- use_module(library(http/html_write)).
% html_resource
:- use_module(library(http/html_head)). 
:- use_module(library(md/md_parse)).
:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(content/about).
:- use_module(git).
:- use_module(rss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile http:location/3.
:- dynamic   http:location/3.
:- multifile user:body//2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This predicate starts the server in the background, and returns
% to the toplevel, so that you can reload and debug the code, etc.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

http:location(css, '/content/css', []).
http:location(posts, '/content/posts', []).
http:location(info, '/content/info', []).
http:location(img, '/content/img', []).
http:location(data, '/content/data', []).

user:file_search_path(css, './content/css').
user:file_search_path(posts, './content/posts').
user:file_search_path(info, './content/info').
user:file_search_path(img, './content/img').
user:file_search_path(data, './content/data').

:- html_resource(css('stylesheet.css'), []).
:- html_resource(root('favicon.ico'), []).

:- http_handler(css(.), http_reply_from_files('./content/css', []), [prefix]).

:- http_handler(img(.), http_reply_from_files('./content/img', []), [prefix]).

:- http_handler(data(.), http_reply_from_files('./content/data', []), [prefix]).

:- http_handler(posts(.), serve_post_markdown, [prefix]).

:- http_handler(info(.), serve_info_markdown, [prefix]).

:- http_handler(root(.), display_toc, []).

:- http_handler('/favicon.ico',
                http_reply_file('./content/img/favicon.ico', []),
                []).

:- http_handler('/robots.txt',
                http_reply_file('./content/info/robots.txt', []),
                []).

:- http_handler('/feed', serve_rss, [prefix]).

serve_rss(_Request) :-
    make_rss(RSS),
    format('Content-Type: application/xml~n~n~s~n', RSS).


validate_file(Path) :-
    exists_file(Path),
    http_safe_file(Path, []).

path_of_request([path_info(PathInfo) | _], PathInfo).
path_of_request([_ | Tail], PathInfo) :- path_of_request(Tail, PathInfo).

serve_post_markdown(Request) :- serve_markdown(Request, posts).
serve_info_markdown(Request) :- serve_markdown(Request, info).

% Let's try to handle some markdown.
serve_markdown(Request, Location) :-
    path_of_request(Request, Basename),
    user:file_search_path(Location, PostDir),
    atomic_list_concat([PostDir, '/', Basename], Path),
    validate_file(Path),
    md_parse_file(Path, Blocks),
    reply_html_page(my_style, [title(Basename)], Blocks).

serve_markdown(Request) :-
    user:file_search_path(posts, PostDir),
    http_reply_from_files(PostDir, [], Request).

serve_markdown(Request) :-
    http_404([], Request).

resolve_author(me, Me) :- about(_,admin(Me),_,_).
resolve_author(X,X).

pretty_date(IsoDate, PrettyDate) :-
    parse_time(IsoDate, Timestamp),
    format_time(atom(PrettyDate), "%F", Timestamp).

% Table of Contents
toc_entry_to_html(entry(file(Filename),
                        title(Title),
                        author(Author),
                        abstract(Abstract)),
                  [li(class(toc_title), a(href=MdPath, Title)),
                   div(class(toc_author),
                       ['by ', ResolvedAuthor, ' (', PrettyDate, ')']),
                   p(class(toc_abstract), Abstract)]) :-
    resolve_author(Author, ResolvedAuthor),
    user:file_search_path(posts, PostDir),
    atomic_list_concat([PostDir, '/', Filename], MdPath),
    %% Try to parse the date, but fail gracefully if unable to, and
    %% just return the original date atom unaltered.
    (last_build_date_of_file(MdPath, IsoDate),
     format(user_error, "toc_entry_to_html> IsoDate = ~s~n", IsoDate),
     pretty_date(IsoDate, PrettyDate);
     PrettyDate = uncommitted).

make_toc(Path, Blocks) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    maplist(toc_entry_to_html, Entries, BlockLists),
    flatten(BlockLists, Blocks).


% the reply_html_page predicate takes care of a lot of this for us.
display_toc(_Request) :-
    make_toc('content/toc.data', ToC),
    about(title(Title),_,_,_),
    reply_html_page(
        my_style,
        [title(Title)],
        [\toc_page_content(ToC)]).

toc_page_content(ToC) -->
    html([ul(class(toc), ToC)]).

user:body(my_style, Body) -->
    {about(title(Title),admin(_),domain(_),abstract(Abstract))},
    html(body([div(class(container),
                   [div(id(main),
                        [h1(Title),
                         hr(_),
                         p(class(abstract), Abstract),
                         \nav_bar,
                         hr(_),
                         br(_),
                         Body])])])).

user:head(my_style, Head) -->
    {about(title(Title),_,_,_)},
    html(head([title(Title),
               \html_requires(css('stylesheet.css')),
               Head])).



nav_bar -->
 	  {
 	      findall(Name, nav(Name, _), ButtonNames),
 	      maplist(as_top_nav, ButtonNames, TopButtons)
 	  },
 	  html(TopButtons).



nav('Home', /).
nav('About', '/content/info/about.md').
nav('Links', '/content/info/links.md').
nav('Storage', '/content/data/').
nav('RSS', '/feed').
nav('P\'log', 'https://github.com/oblivia-simplex/plog').
nav('License', '/content/info/gpl.md').

as_top_nav(Name, span([a([href=HREF, class=topnav], Name), ' '])) :-
    nav(Name, HREF).


%%%
% Logging
%%%

