:- module(git, [last_build_date_of_file/2,
                last_build_date_of_repo/1,
                file_mod_date/2,
                rfc2822_date/2,
                mod_date_or_today/2,
                rfc2822_build_date/1]).


last_build_date_of_file(File, IsoDate) :-
    working_directory(CWD, CWD),
    working_directory(_, 'content'),
    atom_concat('.', File, Path), % to prefix with ..
    format(user_error, "Path = ~s~n", Path),
    process_create(path(git),
                   [
                    %   '--git-dir', './content/.git',
                       'log', '-n1', '--pretty=format:%cI',
                       Path
                   ],
                   [stdout(pipe(Out))]),
    working_directory(_, CWD),
    read_line_to_codes(Out, Codes),
    Codes \= end_of_file,
    name(IsoDate, Codes),
    format(user_error, 'IsoDate = ~s~n', IsoDate),
    close(Out).

%% cheat, and just take the file modification date
file_mod_date(File, IsoDate) :-
    time_file(File, Timestamp),
    format_time(atom(IsoDate), '%FT%T', Timestamp).

last_build_date_of_repo(Date) :-
    file_mod_date('.', Date).

rfc2822_date(IsoDate, Rfc2822Date) :-
    parse_time(IsoDate, Timestamp),
    format_time(atom(Rfc2822Date), '%a, %b %e, %Y', Timestamp).

mod_date_or_today(File, Date) :-
    (file_mod_date(File, IsoDate),
     rfc2822_date(IsoDate, Date));
    (get_time(Timestamp),
     format_time(atom(Date), '%a, %b %e, %Y', Timestamp)).

rfc2822_build_date(Date) :-
    last_build_date_of_repo(IsoDate),
    rfc2822_date(IsoDate, Date).

