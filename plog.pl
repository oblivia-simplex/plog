% vim: set expandtab syntax=prolog :

:- use_module(library(error)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pio)).

% http_reply_from_files is here
:- use_module(library(http/http_files)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_dirindex)).
% for the DCG stuff, I think
:- use_module(library(http/html_write)).
% html_resource
:- use_module(library(http/html_head)).
:- use_module(library(lists)).

:- use_module(parse_post).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- use_module(content/about).
:- use_module(fileinfo).
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
http:location(feed, '/feed', []).
http:location(sitemap, '/sitemap.xml', []).
http:location(notfound, '/', [priority(-9)]).
http:location(bing, '/BingSiteAuth.xml', []).
http:location(scripts, '/scripts', []).

user:file_search_path(css, './content/css').
user:file_search_path(posts, './content/posts').
user:file_search_path(info, './content/info').
user:file_search_path(img, './content/img').
user:file_search_path(data, './content/data').
user:file_search_path(static, './content/static').
user:file_search_path(scripts, './scripts'). % not in content/

%%%
% Handlers
%%

%:- http_handler(root(.), serve_404, [prefix]).
:- http_handler(css(.), file_reply(css(/), []), [prefix]).
:- http_handler(img(.), file_reply(img(/), []), [prefix]).
:- http_handler(data(.), file_reply(data(/), []), [prefix]).
:- http_handler(static(.), file_reply(static(/), []), [prefix]).
:- http_handler(scripts(.), file_reply(scripts(/), []), [prefix]).
:- http_handler(posts(.), serve_post_markdown, [prefix]).
:- http_handler(info(.), serve_info_markdown, [prefix]).
:- http_handler(tags(.), display_tags, []).
:- http_handler(tags(.), display_toc, [prefix]).
:- http_handler('/favicon.ico',
                http_reply_file(img('favicon.ico'), []),
                []).
:- http_handler('/robots.txt',
                http_reply_file(info('robots.txt'), []),
                []).
:- http_handler(feed(.), serve_rss, []).
:- http_handler(sitemap(.), serve_sitemap, []).
:- http_handler(bing(.), http_reply_file(info('BingSiteAuth.xml'), []), []).
%:- http_handler(notfound(.), serve_404, [prefix]).
:- http_handler(root(.), display_toc, []).





file_reply(Dir, Opt, Request) :-
    http_reply_from_files(Dir, Opt, Request).

file_reply(_, _, Request) :-
    serve_404(Request).

serve_rss(_Request) :-
    make_rss(RSS),
    format('Content-Type: application/xml~n~n~s~n', RSS).

serve_sitemap(_Request) :-
    make_sitemap(Sitemap),
    format('Content-Type: application/xml~n~n~s~n', Sitemap).

validate_file(Path) :-
    exists_file(Path),
    http_safe_file(Path, []).

path_of_request([path_info(PathInfo) | _], PathInfo).
path_of_request([_ | Tail], PathInfo) :- path_of_request(Tail, PathInfo).

serve_post_markdown(Request) :- serve_markdown(Request, posts).
serve_info_markdown(Request) :- serve_markdown(Request, info).



% (+File, +Location, -Blocks, -Commit, -Entry)
get_content(File, Location, Blocks, Commit, Entry) :-
    user:file_search_path(Location, PostDir),
    atomic_list_concat([PostDir, '/', File], Path),
    validate_file(Path),
    git_hash_of_file(Location, File, Commit),
    parse_post_with_meta(Path, Blocks, Entry),
    !.

% (+File, +Location, -Blocks, -Commit)
get_content(File, Location, Blocks, Commit) :-
    user:file_search_path(Location, PostDir),
    atomic_list_concat([PostDir, '/', File], Path),
    validate_file(Path),
    git_hash_of_file(Location, File, Commit),
    %format(user_error, 'Commit for ~s/~s: ~s\n', [Location, File, Commit]),
    %format(user_error, 'Path: ~s\n', Path),
    parse_post(Path, Blocks),
    !.


make_footer(uncommitted,
            [hr(class=footer_hr),
             'Last Commit: uncommitted',
             hr(class=footer_hr)]).

make_footer(Commit, [Bar, FooterDiv, Bar]) :-
    about:repo(Repo),
    Bar = hr(class=footer_hr),
    format(atom(CommitUrl), '~s/commit/~s', [Repo, Commit]),
    FooterDiv = div(class=footer, ['Last Commit: ', a(href=CommitUrl, Commit)]),
    !.


make_header(Basename, Entry, Header) :-
    dissect_entry(Entry,_Filename,Title,Author,_Abstract,Tags,WordCount,Date),
    prettify_date(Date, PrettyDate),
    format(atom(DateLine), 'Posted: ~s', [PrettyDate]),
    (memberchk(draft, Tags)
    -> TitleLine = ['DRAFT: ', Title]
    ; TitleLine = [Title]),
    last_build_date_of_file(posts, Basename, RevisedDate),
    prettify_date(RevisedDate, PrettyRevisedDate),
    memberchk(editedby(Editors), Entry),
    format(atom(RevisedDateLine), 'Edited: ~s (~s)', [PrettyRevisedDate, Editors]),
    format(atom(WordLine), '~d words', WordCount),
    maplist(make_tag, Tags, TagLine),
    Header = [
        h1(TitleLine),
        div(class=post_frontmatter,
            [
                hr(class=title_hr),
                div(class=byline, ['Author: ', Author]),
                div(class=dateline, DateLine),
                div(class=dateline, RevisedDateLine),
                div(class=header_wordcount, ['Length: ', WordLine]),
                div(class=tagline, ['Tags: ' | TagLine]),
                hr(class=title_hr)
            ])
    ],
    !.


serve_markdown(Request, posts) :-
    memberchk(path(Path), Request), %path_of_request(Request, Basename),
    safe_base_and_parent_name(Path, Basename, posts),
    %format(user_error, "[serve_markdown] Path = ~s Basename = ~s~n", [Path, Basename]),
    file_name_extension(_, md, Basename),
    get_content(Basename, posts, PostBlocks, Commit, Entry),
    make_header(Basename, Entry, Header),
    memberchk(abstract(Abstract), Entry),
    memberchk(title(Title), Entry),
    format(atom(Description), 'name="description", content="~s"', Abstract),
    make_footer(Commit, Footer),
    reply_html_page(my_style,
                    [title(Title),
                     meta(Description)
                    ],
                    [div(class=header, Header),
                     div(class=post, PostBlocks),
                     div(class=footer, Footer)]).

serve_markdown(Request, Location) :-
    Location \= posts,
    path_of_request(Request, Basename),
    file_name_extension(_, md, Basename),
    get_content(Basename, Location, PostBlocks, Commit),
    make_footer(Commit, Footer),
    format(atom(Title), '~s', [Basename]),
    format(atom(Description), 'name="description", content="~s"', [Basename]),
    reply_html_page(my_style,
                    [title(Title),
                     meta(Description)],
                    [div(class=post, PostBlocks),
                     div(class=footer, Footer)]).

serve_markdown(Request, Location) :-
    user:file_search_path(Location, PostDir),
    file_reply(PostDir, [], Request).


serve_404(Request) :-
  memberchk(path(Path), Request),
  format('Status: 404~n'),
  Message = div(['The requested URL ', code(Path), ' could not be found on this server.']),
  reply_html_page(my_style,
                  [title('Not Found'),
                   meta('name="description", content="resource not found"')],
                  [h1(class=error, 'Not Found'),
                   div(id=post, Message)]).

%%
% The "secret" parameter that's required in order to display the
% drafts is a pretty low-security secret. It's passed in the GET
% request, and can easily be snooped. But the stakes are low, so
% I'm letting it be. Don't put anything in your drafts that you
% are afraid of the world seeing.
%%
get_secret(Secret) :-
  getenv('PLOG_SECRET', Secret),
  !.

get_secret(opensesame).

get_tag_from_request(Request, draft) :-
    memberchk(path(Path), Request),
    atom_concat('/tags/', draft, Path),
    get_secret(Secret),
    catch(http:http_parameters(Request, [secret(Secret, [] )]),
        error(existence_error(http_parameter, secret), _),
        false).

get_tag_from_request(Request, Tag) :-
    memberchk(path(Path), Request),
    atom_concat('/tags/', Tag, Path),
    Tag \= draft.

get_tag_from_request(_, everything).

% if the Tag is 'draft', then check for the magic word
% in the parameter.
display_toc(Request) :-
    make, %% KLUDGE - rebuild the page when reloading %%
    get_tag_from_request(Request, Tag),
    user:file_search_path(posts, PostDir),
    make_toc(PostDir, ToC, Tag),
    content:about:title(Title),
    reply_html_page(
        my_style,
        [title(Title)],
        [
            \toc_page_content(ToC)
        ]).

display_tags(_Request) :-
    content:about:title(Title),
    user:file_search_path(posts, PostDir),
    extract_tags_from_toc(PostDir, Tags),
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
    html(head([meta([name=viewport,
                     content='width=device-width, initial-scale=1.0']),
               \html_requires(scripts('es5/tex-mml-chtml.js')),
               \html_requires(scripts('MathJaxConfig.js')),
               \html_requires(css('stylesheet.css')),
               Head])).


nav('Home', /).
nav('About', '/info/about.md').
nav('Tags', '/tags').
nav('Links', '/info/links.md').
nav('Storage', '/data/').
nav('RSS', '/feed').
nav('P\'log', 'https://github.com/oblivia-simplex/plog').

nav_bar -->
	  {
	      findall(Name, nav(Name, _), ButtonNames),
	      maplist(as_top_nav, ButtonNames, TopButtons)
	  },
	  html(TopButtons).


as_top_nav(Name, span([a([href=HREF, class=topnav], Name), &(nbsp), ' '])) :-
    nav(Name, HREF).



%%%%% Entry point %%%%%

start :-
    about:bind(Address),
    about:port(Port),
    server(Address:Port).
