:- module(md_trim, [
    trim_left/2,  % +Codes, -Result
    trim_right/2, % +Codes, -Result
    trim/2        % +Codes, -Result
]).

/** <module> Code list whitespace trimming

Helper module to trim whitespaces from
lists of codes.
*/

%! trim_left(+Codes, -Result) is det.
%
% Trims whitespaces from the beginning of
% the list of codes.

trim_left([Code|Codes], Result):-
    code_type(Code, space), !,
    trim_left(Codes, Result).

trim_left(Codes, Codes).

%! trim_right(+Codes, -Result) is det.
%
% Trims whitespace from the end of the
% list of codes.

trim_right(Codes, Result):-
    reverse(Codes, CodesR),
    trim_left(CodesR, ResultR),
    reverse(ResultR, Result).

%! trim(+Codes, -Result) is det.
%
% Trims whitespace from both sides of the
% list of codes.

trim(Codes, Result):-
    trim_left(Codes, Tmp1),
    reverse(Tmp1, Tmp2),
    trim_left(Tmp2, Tmp3),
    reverse(Tmp3, Result).
