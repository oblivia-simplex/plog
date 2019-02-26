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

% :- use_module(content/about).
:- use_module(timestamp).
:- use_module(rss).
:- use_module(toc_reader).

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

http:location(css, '/css', []).
http:location(posts, '/posts', []).
http:location(info, '/info', []).
http:location(img, '/img', []).
http:location(data, '/data', []).
http:location(tags, '/tags', []).
http:location(static, '/static', []).

user:file_search_path(css, './content/css').
user:file_search_path(posts, './content/posts').
user:file_search_path(info, './content/info').
user:file_search_path(img, './content/img').
user:file_search_path(data, './content/data').
user:file_search_path(static, './content/static').

:- html_resource(css('stylesheet.css'), []).
:- html_resource(root('favicon.ico'), []).

%%%
% Handlers
%%
:- http_handler(css(.), http_reply_from_files('./content/css', []), [prefix]).
:- http_handler(img(.), http_reply_from_files('./content/img', []), [prefix]).
:- http_handler(data(.), http_reply_from_files('./content/data', []), [prefix]).
:- http_handler(static(.), http_reply_from_files('./content/static', []), [prefix]).
:- http_handler(posts(.), serve_post_markdown, [prefix]).
:- http_handler(info(.), serve_info_markdown, [prefix]).
:- http_handler(tags(.), display_tags, []).
:- http_handler(tags(_), display_toc, [prefix]).
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

check_toc_for_file(File, Entry) :-
    open('./content/toc.data', read, Stream),
    read(Stream, Entries),
    close(Stream),
    get_file_entry(File, Entries, Entry).

draftchk(File) :-
    check_toc_for_file(File, Entry),
    memberchk(tags(Tags), Entry),
    memberchk(draft, Tags).

draftchk(File) :-
    check_toc_for_file(File, _);
    true. % if no entry yet, assume draft

parse_markdown(File, _, Blocks) :-
    nb_current(File, Blocks), % check the cache
    format(user_error, "Retrieved parsed blocks from cache for ~s~n", File).

parse_markdown(File, Location, Blocks) :-
    user:file_search_path(Location, PostDir),
    atomic_list_concat([PostDir, '/', File], Path),
    validate_file(Path),
    md_parse_file(Path, Blocks),
    (
        % Store the result only if the file is not marked as a draft
        draftchk(File);
        nb_setval(File, Blocks)
    ).

serve_markdown(Request, Location) :-
    path_of_request(Request, Basename),
    parse_markdown(Basename, Location, Blocks),
    reply_html_page(my_style, [title(Basename)], Blocks).

serve_markdown(Request, Location) :-
    user:file_search_path(Location, PostDir),
    http_reply_from_files(PostDir, [], Request).

serve_markdown(Request, _) :-
    http_404([], Request).

display_toc(Request) :-
    memberchk(request_uri(Uri), Request),
    (
        atom_concat('/tags/', Tag, Uri);
        Tag = everything
    ),
    %toc_reader:get_subtags(Tag, Subtags),
    make_toc('content/toc.data', ToC, Tag),
    content:about:title(Title),
    reply_html_page(
        my_style,
        [title(Title)],
        [
            /*
            h2('Table of Contents'),
            p([
                     'Tag: ~s' - [Tag],
                     'Subtags: ',
                     span(Subtags)
              ]),
            */
            \toc_page_content(ToC)
        ]).

display_tags(_Request) :-
    content:about:title(Title),
    extract_tags_from_toc('content/toc.data', Tags),
    reply_html_page(
        my_style,
        [title(Title)],
        [\tag_page_content(Tags)]
    ).

toc_page_content(ToC) -->
    html([div(class(toc), ul(class(toc), ToC))]).

tag_page_content(Tags) -->
    html([h1('Tags'), div(class(tag_list), Tags)]).

user:body(my_style, Body) -->
    {
        content:about:title(Title),
        content:about:abstract(Abstract)
    },
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
    {
        content:about:title(Title)
    },
    html(head([title(Title),
               meta([name=viewport,
                     content='width=device-width, initial-scale=1.0']),
               \html_requires(css('stylesheet.css')),
               Head])).



nav_bar -->
 	  {
 	      findall(Name, nav(Name, _), ButtonNames),
 	      maplist(as_top_nav, ButtonNames, TopButtons)
 	  },
 	  html(TopButtons).



nav('Home', /).
nav('About', '/info/about.md').
nav('Tags', '/tags').
nav('Links', '/info/links.md').
nav('Storage', '/data/').
nav('RSS', '/feed').
nav('P\'log', 'https://github.com/oblivia-simplex/plog').
nav('License', '/info/license.md').
%nav('Contact', '/content/info/contact.md').

as_top_nav(Name, span([a([href=HREF, class=topnav], Name), ' '])) :-
    nav(Name, HREF).


%%%%%%%%%%%%%%%%%%%%%%
%%  some scratch code for testing
%:- use_module(comments).
%:- http_handler(root(testform) , test_form_page_handler, [id(testform)]).
%%
% Consider replacing this with a javascript obfuscator
% one that ROT13s the email address, e.g.
%%
%% gently_obfuscate_codes([],[]).
%% gently_obfuscate_codes([C|CTail], [Ob|ObTail]) :-
%%     number_codes(C,Ncodes),
%%     Ob_ = [0x26,0x23|Ncodes],
%%     append(Ob_, [0x3b], Ob),
%%     gently_obfuscate_codes(CTail, ObTail).

%% gently_obfuscate(Clear, Obf) :-
%%     name(Clear, Codes),
%%     gently_obfuscate_codes(Codes, ObCodes),
%%     flatten(ObCodes, FlatObCodes),
%%     append([0x22], FlatObCodes, Q1),
%%     append(Q1, [0x22], Q2),
%%     name(Obf, Q2).

 

start :-
    about:port(Port),
    server(Port).
