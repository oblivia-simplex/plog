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

:- use_module(about).
:- use_module(git).

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

http:location(static, '/static', []).
http:location(css, root(css), []).
http:location(posts, root(posts), []).
http:location(data, './data', []).
http:location(img, root(img), []).
:- html_resource(css('stylesheet.css'), []).

:- http_handler(css(.), http_reply_from_files(css, []), [prefix]).
:- http_handler(img(.), http_reply_from_files(img, []), [prefix]).
:- http_handler(posts(.), serve_markdown, [prefix]).
:- http_handler(root(.), display_toc, []).
:- http_handler('/favicon.ico',
                http_reply_from_files(root, []),
                [prefix]).

validate_file(Path) :-
    exists_file(Path).

path_of_request([path_info(PathInfo) | _], PathInfo).
path_of_request([_ | Tail], PathInfo) :- path_of_request(Tail, PathInfo).

% Let's try to handle some markdown.
serve_markdown(Request) :-
    path_of_request(Request, Basename),
    atom_concat('./posts/', Basename, Path),
    validate_file(Path),
    md_parse_file(Path, Blocks),
    reply_html_page(my_style, [title(Basename)], Blocks).

serve_markdown(Request) :-
    http_reply_from_files(posts, [], Request).

serve_markdown(Request) :-
    http_404([], Request).

resolve_author(me, Me) :- about(_,admin(Me),_,_).
resolve_author(X,X).

% Table of Contents
toc_entry_to_html(entry(file(Filename),
                        title(Title),
                        author(Author),
                        abstract(Abstract)),
                  [li(class(toc_title), a(href=MdPath, Title)),
                   div(class(toc_author),
                       ['by ', ResolvedAuthor, ', ', PrettyDate]),
                   p(class(toc_abstract), Abstract)]) :-
    resolve_author(Author, ResolvedAuthor),
    atom_concat('./posts/', Filename, MdPath),
    %% Try to parse the date, but fail gracefully if unable to, and
    %% just return the original date atom unaltered.
    (last_build_date_of_file(MdPath, PrettyDate);
     PrettyDate = uncommitted).

make_toc(Path, Blocks) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    maplist(toc_entry_to_html, Entries, BlockLists),
    flatten(BlockLists, Blocks).


% the reply_html_page predicate takes care of a lot of this for us.
display_toc(_Request) :-
    make_toc('toc.data', ToC),
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
nav('About', '/posts/about.md').

as_top_nav(Name, span([a([href=HREF, class=topnav], Name), ' '])) :-
    nav(Name, HREF).


