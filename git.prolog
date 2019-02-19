:- module(git, [last_build_date_of_file/2, last_build_date_of_repo/1]).

last_build_date_of_file(File, IsoDate) :-
    process_create(path(git), [log,
                               '-n1',
                               '--pretty=format:%cI',
                               File],
                   [stdout(pipe(Out))]),
    read_line_to_codes(Out, Codes),
    Codes \= end_of_file,
    atom_codes(IsoDate, Codes).


last_build_date_of_repo(Date) :-
    last_build_date_of_file('.', Date).
