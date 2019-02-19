:- module(git, [last_build_date_of_file/2, last_build_date_of_repo/1]).

last_build_date_of_file(File, Date) :-
    process_create(path(git), [log,
                               '-n1',
                               '--pretty=format:%cI',
                               File],
                   [stdout(pipe(Out))]),
    read_line_to_codes(Out, Codes),
    atom_codes(IsoDate, Codes),
    parse_time(IsoDate, Timestamp),
    format_time(atom(Date), '%a, %b %e, %Y', Timestamp).


last_build_date_of_repo(Date) :-
    last_build_date_of_file('.', Date).
