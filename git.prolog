:- module(git, [last_build_date_of_file/2, last_build_date_of_repo/1]).

make_cmd(File, Cmd) :-
    format(atom(Cmd), '\\"cd content; git log -n1 --pretty=format:%cI ~s\\"', File).

last_build_date_of_file(File, IsoDate) :-
    format(user_error, "File = ~s~n", File),
    process_create(path(git),
                   [
                       '--git-dir', './content/.git',
                       'log', '-n1', '--pretty=format:%cI',
                       File
                   ],
                   [stdout(pipe(Out))]),
    read_line_to_codes(Out, Codes),
    Codes \= end_of_file,
    name(IsoDate, Codes),
    close(Out).


last_build_date_of_repo(Date) :-
    last_build_date_of_file('.', Date).
