% vim: set expandtab syntax=prolog :
:- module(fileinfo, [last_build_date_of_file/3,
                      git_hash_of_file/3,
                      last_build_date_of_repo/1,
                      file_mod_date/2,
                      safe_directory_name/2,
                      safe_base_name/2,
                      safe_parent_name/2,
                      safe_base_and_parent_name/3,
                      rfc2822_date/2,
                      mod_date_or_today/2,
                      sitemap_date/2,
                      rfc2822_build_date/1,
                      prettify_date/2]).

use_module(library(git)).
use_module(library(pcre)).
% Add a predicate to adjust the timezone
% and have the local timezone set in either about.prolog, or consult
% the OS.
%
% Stopgap: 4 hours is 14400 seconds.

exists_path(P) :- exists_file(P).
exists_path(P) :- exists_directory(P).


% better functions for dir name and base name to avoid // bugs
safe_directory_name(Path, Dir) :-
  collapse_multiple_slashes(Path, CleanPath),
  file_directory_name(CleanPath, Dir),
  !.

safe_base_name(Path, Base) :-
  collapse_multiple_slashes(Path, CleanPath),
  file_base_name(CleanPath, Base),
  !.

safe_parent_name(Path, Parent) :-
  collapse_multiple_slashes(Path, CleanPath),
  file_directory_name(CleanPath, Dir),
  file_base_name(Dir, Parent),
  !.

safe_base_and_parent_name(Path, Base, Parent) :-
  collapse_multiple_slashes(Path, CleanPath),
  file_directory_name(CleanPath, Dir),
  file_base_name(Dir, Parent),
  file_base_name(CleanPath, Base),
  !.

collapse_multiple_slashes(Path, CleanPath) :-
  atom_string(Path, PathS),
  re_replace("//*", "/", PathS, CleanPath),
  format(user_error, '[collapse_multiple_slashes] "~s" --> "~s"~n', [PathS, CleanPath]),
  !.


%% cheat, and just take the file modification date
file_mod_date(File, Timestamp) :-
    time_file(File, Timestamp).
    %format_time(atom(IsoDate), '%FT%T', Timestamp).
    
last_build_date_of_file(Dir, File, TimeStamp) :-
  atomic_list_concat(['content/', Dir], WorkingDir),
  git([log, '-n1', '--pretty=format:%cI', File], [directory(WorkingDir), output(DateCodes)]),
  atom_codes(IsoDate, DateCodes),
  parse_time(IsoDate, TimeStamp),
  !.

last_build_date_of_file(Dir, File, TimeStamp) :-
  atomic_list_concat(['content/', Dir, '/', File], Path),
  file_mod_date(Path, TimeStamp).

git_hash_of_file(Dir, File, Hash) :-
    atomic_list_concat(['content/', Dir], WorkingDir),
    git([log, '-n1', '--pretty=format:%H', File], [directory(WorkingDir), output(HashCodes)]),
    atom_codes(Hash, HashCodes),
    !.

git_hash_of_file(_,_,uncommitted).


last_build_date_of_repo(Date) :-
    file_mod_date('.', Date).

rfc2822_date(TimeStamp, Rfc2822Date) :-
    content:about:timezone(TZ),
    LocalTimeStamp is TimeStamp + TZ,
    format_time(atom(Rfc2822Date), '%a, %b %e, %Y', LocalTimeStamp).

mod_date_or_today(File, Date) :-
    (file_mod_date(File, IsoDate),
     rfc2822_date(IsoDate, Date));
    (get_time(Timestamp),
     format_time(atom(Date), '%a, %b %e, %Y', Timestamp)).

rfc2822_build_date(Date) :-
    last_build_date_of_repo(IsoDate),
    rfc2822_date(IsoDate, Date).



prettify_date(TimeStamp, PrettyDate) :-
    %content:about:timezone(TZ),
    %LocalTimeStamp is TimeStamp + TZ,
    format_time(atom(PrettyDate), "%F", TimeStamp).

sitemap_date(IsoDate, SitemapDate) :- prettify_date(IsoDate, SitemapDate).




%% Get all of the authors who have edited a piece
git_authors(Dir, File, Authors) :-
  atomic_list_concat(['content/', Dir], WorkingDir),
  git([log, '--pretty=format:%aN', File], [directory(WorkingDir), output(NameCodes)]),
  string_codes(NameString, NameCodes),
  split_string(NameString, "\n", "", Names),
  list_to_set(Names, UniqueNames),
  %  maplist(atom_string, UniqueAuthors, UniqueNames),
  atomics_to_string(UniqueNames, ', ', UniqueNamesConcat),
  atom_string(UniqueNamesConcat, Authors),
  !.

git_authors(_,_,uncommitted).

