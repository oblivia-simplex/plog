:- module(timestamp, [last_build_date_of_file/3,
                      git_hash_of_file/3,
                      content_git_command/4,
                      last_build_date_of_repo/1,
                      file_mod_date/2,
                      rfc2822_date/2,
                      mod_date_or_today/2,
                      rfc2822_build_date/1,
                      toc_date/2]).

% Add a predicate to adjust the timezone
% and have the local timezone set in either about.prolog, or consult
% the OS.
%
% Stopgap: 4 hours is 14400 seconds.

content_git_command(GitArgs, Dir, File, Output) :-
    working_directory(CWD, CWD),
    atomic_list_concat([Dir, '/', File], Path),
    atomic_list_concat([CWD, '/', content, '/'], ContentDir),
    atomic_list_concat([ContentDir, '.git'], GitDir),
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


last_build_date_of_file(Dir, File, IsoDate) :-
    content_git_command(['log', '-n1', '--pretty=format:%cI'],
                        Dir,
                        File,
                        IsoDate).

git_hash_of_file(Dir, File, Hash) :-
    content_git_command(['log', '-n1', '--pretty=format:%H'],
                        Dir,
                        File,
                        Hash).

git_hash_of_file(_,_,uncommitted).

%% cheat, and just take the file modification date
file_mod_date(File, IsoDate) :-
    time_file(File, Timestamp),
    format_time(atom(IsoDate), '%FT%T', Timestamp).

last_build_date_of_repo(Date) :-
    file_mod_date('.', Date).

rfc2822_date(IsoDate, Rfc2822Date) :-
    parse_time(IsoDate, Timestamp),
    content:about:timezone(TZ),
    LocalTimestamp is Timestamp + TZ,
    format_time(atom(Rfc2822Date), '%a, %b %e, %Y', Timestamp).

mod_date_or_today(File, Date) :-
    (file_mod_date(File, IsoDate),
     rfc2822_date(IsoDate, Date));
    (get_time(Timestamp),
     format_time(atom(Date), '%a, %b %e, %Y', Timestamp)).

rfc2822_build_date(Date) :-
    last_build_date_of_repo(IsoDate),
    rfc2822_date(IsoDate, Date).

toc_date(IsoDate, PrettyDate) :-
    parse_time(IsoDate, Timestamp),
    % Stopgap
    content:about:timezone(TZ),
    LocalTimestamp is Timestamp + TZ,
    format_time(atom(PrettyDate), "%F", LocalTimestamp).


