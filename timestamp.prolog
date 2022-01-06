:- module(timestamp, [last_build_date_of_file/3,
                      git_hash_of_file/3,
                      content_git_command/4,
                      last_build_date_of_repo/1,
                      file_mod_date/2,
                      rfc2822_date/2,
                      mod_date_or_today/2,
                      sitemap_date/2,
                      rfc2822_build_date/1,
                      prettify_date/2]).

% Add a predicate to adjust the timezone
% and have the local timezone set in either about.prolog, or consult
% the OS.
%
% Stopgap: 4 hours is 14400 seconds.

exists_path(P) :- exists_file(P).
exists_path(P) :- exists_directory(P).

find_git_dir(ContentDir, GitDir) :-
    atomic_list_concat([ContentDir, '/', '.git'], D),
    absolute_file_name(D, AbsDir),
    ((exists_path(AbsDir), GitDir = AbsDir, !); 
      atomic_list_concat([ContentDir, '/', '..'], UpDir),
      find_git_dir(UpDir, GitDir)).

find_git_dir('/', '/.git') :-
    exists_directory('/.git').

content_git_command(GitArgs, Dir, File, Output) :-
    working_directory(CWD, CWD),
    atomic_list_concat([Dir, '/', File], Path),
    atomic_list_concat([CWD, '/content/'], ContentDir),
    find_git_dir(ContentDir, GitDir),
    %format(user_error, "Git dir is ~s~n", [GitDir]),
    Args1 = [
        '--git-dir', GitDir,
        '-C', 'content/'
        | GitArgs
    ],
    append(Args1, [Path], Args),
    process_create(path(git), Args, [stdout(pipe(Out)), process(PID)]),
    working_directory(_, CWD),
    read_line_to_codes(Out, Codes),
    close(Out),
    process_wait(PID, exit(ExitCode)),
    ExitCode == 0, % Fail otherwise
    Codes \= end_of_file,
    name(Output, Codes).


%% cheat, and just take the file modification date
file_mod_date(File, Timestamp) :-
    time_file(File, Timestamp).
    %format_time(atom(IsoDate), '%FT%T', Timestamp).
    
last_build_date_of_file(Dir, File, TimeStamp) :-
    content_git_command(['log', '-n1', '--pretty=format:%cI'],
                        Dir,
                        File,
                        IsoDate),
    parse_time(IsoDate, TimeStamp).

last_build_date_of_file(Dir, File, TimeStamp) :-
  working_directory(CWD, CWD),
  atomic_list_concat([CWD, '/content/', Dir, '/', File], Path),
  file_mod_date(Path, TimeStamp),
  working_directory(_, CWD).

git_hash_of_file(Dir, File, Hash) :-
    content_git_command(['log', '-n1', '--pretty=format:%H'],
                        Dir,
                        File,
                        Hash).

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

