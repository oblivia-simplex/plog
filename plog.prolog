:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
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
%% Some easily tuned facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

the_title('Campaign Logs').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile http:location/3.
:- dynamic   http:location/3.
:- multifile user:body//2.

http:location(static, '/static', []).
http:location(css, root(css), []).
http:location(md, root(md), []).
http:location(data, './data', []).

:- http_handler(css(.), serve_css, [prefix]).

serve_css(Request) :-
    http_reply_from_files(css, [], Request).

serve_css(Request) :-
    http_404([], Request).

:- http_handler(md(.), serve_markdown, [prefix]).

validate_file(Path) :-
    exists_file(Path).

path_of_request([path_info(PathInfo) | _], PathInfo).
path_of_request([_ | Tail], PathInfo) :- path_of_request(Tail, PathInfo).

% Let's try to handle some markdown.
serve_markdown(Request) :-
    path_of_request(Request, Basename),
    atom_concat('./md/', Basename, Path),
    validate_file(Path),
    md_parse_file(Path, Blocks),
    reply_html_page(my_style, [title(Basename)], Blocks).

serve_markdown(Request) :-
    http_reply_from_files(md, [], Request).

serve_markdown(Request) :-
    http_404([], Request).


% Table of Contents
toc_entry_to_html([Filename, Title, Author, Abstract],
                  [li(class(toc_title), a(href=MdPath, Title)),
                   div(class(toc_author),
                       ['by ', Author]),
                   p(class(toc_abstract), Abstract)]) :-
    atom_concat('./md/', Filename, MdPath).

make_toc(Path, Blocks) :-
    open(Path, read, Stream),
    read(Stream, Entries),
    maplist(toc_entry_to_html, Entries, BlockLists),
    flatten(BlockLists, Blocks).



:- html_resource(css('stylesheet.css'), []).

% This predicate starts the server in the background, and returns
% to the toplevel, so that you can reload and debug the code, etc.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.), display_toc, []).

% the reply_html_page predicate takes care of a lot of this for us.
display_toc(_Request) :-
    make_toc('toc.data', ToC),
    the_title(Title),
    reply_html_page(
        my_style,
        [title(Title)],
        [h1(Title),
         \toc_page_content(ToC)]).

toc_page_content(ToC) -->
    html(
        [
            ul(ToC)
        ]
    ).

user:body(my_style, Body) -->
    html(body([
                     div(class(container),
                         div(id(main),
                            Body))
                 ])).

user:head(my_style, Head) -->
    html(head([title('Fancy Ass Title'),
               \html_requires(css('stylesheet.css')),
               Head])).



%% nav_bar -->
%% 	  {
%% 	      findall(Name, nav(Name, _), ButtonNames),
%% 	      maplist(as_top_nav, ButtonNames, TopButtons),
%% 	      maplist(as_bottom_nav, ButtonNames, BottomButtons)
%% 	  },
%% 	  html([\html_post(bottom_nav, BottomButtons) | TopButtons]).


%% nav('Home', '/home').
%% nav('About', '/about').
%% nav('MD', '/md').

%% as_top_nav(Name, a([href=HREF, class=topnav], Name)) :-
%% 	  nav(Name, HREF).

%% as_bottom_nav(Name, a([href=HREF, class=bottomnav], Name)) :-
%% 	  nav(Name, HREF).

