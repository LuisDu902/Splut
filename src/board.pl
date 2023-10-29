:- dynamic position/2.

% -----------------------------------------

create_list(0, [], _).

create_list(Size, [Elem|Rest], Elem) :-
    Size > 0,
    Size1 is Size - 1,
    create_list(Size1, Rest, Elem).

% -----------------------------------------

create_row(1, Size, Row):-
    Padding is (Size - 1 ) // 2,
    create_list(Padding, A, e-e),
    append([A, [r-1], A], Row), !.

create_row(Size, Size, Row):-
    Padding is (Size - 1 ) // 2,
    create_list(Padding, A, e-e),
    append([A, [r-4], A], Row), !.

create_row(2, Size, Row):-
    Padding is (Size - 3) // 2,
    create_list(Padding, A, e-e),
    append([A, [t-p2, d-p2, s-p2], A], Row),
    asserta(position(t-p2, 2-4)),
    asserta(position(d-p2, 2-5)),
    asserta(position(s-p2, 2-6)), !.

create_row(Index, Size, Row):-
    Index is Size-1,
    Padding is (Size - 3) // 2,
    create_list(Padding, A, e-e),
    append([A, [s-p1, d-p1, t-p1], A], Row), 
    asserta(position(s-p1, Index-4)),
    asserta(position(d-p1, Index-5)),
    asserta(position(t-p1, Index-6)), !.

create_row(Index, Size, Row):-
    Index is (Size+1) // 2,
    Empty is Size - 2,
    create_list(Empty, A, x-x),
    append([[r-2], A, [r-3]], Row), !.

create_row(Index, Size, Row):-
    Padding is abs((Size + 1) // 2 - Index),
    Rest is Size - 2 * Padding,
    create_list(Padding, A, e-e),
    create_list(Rest, B, x-x),
    append([A, B, A], Row), !.

% -----------------------------------------

create_board(Size, Board):-
    create_board_aux(1, Size, [], Board).

create_board_aux(Size, Size, AuxBoard, Board):-
    create_row(Size, Size, Row),
    append(AuxBoard, [Row], Board), !.

create_board_aux(Index, Size, AuxBoard, Board):-
    Index < Size,
    create_row(Index, Size, Row),
    NextIndex is Index + 1,
    append(AuxBoard, [Row], UpdatedBoard),
    create_board_aux(NextIndex, Size, UpdatedBoard, Board), !.

% -----------------------------------------

display_board(Board) :-
    length(Board, Size),
    display_col_index(1, Size),
    display_rows(Board, Size, 1).

% -----------------------------------------

display_col_index(1, Size):-
    write('  1'),
    display_col_index(2, Size).

display_col_index(Size, Size) :-
   (Size >= 10 ->  format('  ~d\n', [Size]) ; format('   ~d\n', [Size])).

display_col_index(N, Size) :-
    N < Size,
   (N >= 10 -> format('  ~d', [N]) ; format('   ~d', [N])),
    N1 is N + 1,
    display_col_index(N1, Size).

% -----------------------------------------
    
display_sep_line(0, _) :- write('|'), nl.

display_sep_line(Size, Index) :-
    Middle is (Size + 1) / 2, 
    Index =< Middle, 
    Dashes is 2 * Index - 1,
    Padding is (Size - Dashes) // 2,
    display_padding(Padding),
    display_dashes(Dashes, Dashes),
    nl, !.

display_sep_line(Size, Index) :-
    Middle is (Size + 1) // 2, 
    Dashes is Size - 2 * (Index - Middle - 1),
    Padding is (Size - Dashes) // 2,
    display_padding(Padding),
    display_dashes(Dashes, Dashes),
    nl, !.


% -----------------------------------------
    
display_padding(0).
display_padding(N) :-
    N > 0,
    write('    '),
    NewN is N - 1,
    display_padding(NewN).

% -----------------------------------------
    
display_dashes(0, _).
display_dashes(N, N) :- 
    write(' ---'),
    NewN is N - 1,
    display_dashes(NewN, N).
display_dashes(N, Dashes) :-
    N > 0,
    write('|---'),
    NewN is N - 1,
    display_dashes(NewN, Dashes).

% -----------------------------------------

display_rows([], Size, _) :- display_sep_line(Size, 1).
display_rows([Row|Rest], Size, Index) :-
    display_sep_line(Size, Index),
    display_row(Row),
    format(' ~d\n', [Index]),
    NewIndex is Index + 1,
    display_rows(Rest, Size, NewIndex).

% -----------------------------------------


last_elem(e-e, _).

last_elem(_, []):- write('|').

last_elem(_, [H | _]):-
    H = e-e,
    write('|').

last_elem(_, [H | _]):-
    H \= e-e.


display_row([]). 

display_row([Element|Rest]) :-
    display_element(Element), 
    last_elem(Element, Rest),
    display_row(Rest).

% -----------------------------------------


color(r-_, 33) :- !.
color(_-p1, 34) :- !.
color(_-p2, 31) :- !.
color(_-_, 0) :- !.

display_element(x-x) :- write('|   '), !.

display_element(X-Y) :- 
    color(X-Y, ColorCode),
    (ColorCode \= 0 -> format('| \e[~dm~w\e[0m ', [ColorCode, X]); write('    ')).
    