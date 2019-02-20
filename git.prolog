:- module(git, [last_build_date_of_file/2, last_build_date_of_repo/1]).


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
last_build_date_of_file(File, IsoDate) :-
    time_file(File, Timestamp),
    format_time(atom(IsoDate), '%FT%T', Timestamp).

last_build_date_of_repo(Date) :-
    last_build_date_of_file('.', Date).
