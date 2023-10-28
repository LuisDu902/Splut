


:- use_module(library(lists)).

create_list(0, [], _).

create_list(Size, [Elem|Rest], Elem) :-
    Size > 0,
    Size1 is Size - 1,
    create_list(Size1, Rest, Elem).

% -----------------------------------------

create_row(1, Size, Row):-
    Padding is (Size - 1 ) // 2,
    create_list(Padding, A, '-'),
    append([A, ['r1'], A], Row), !.

create_row(Size, Size, Row):-
    Padding is (Size - 1 ) // 2,
    create_list(Padding, A, '-'),
    append([A, ['r4'], A], Row), !.

create_row(2, Size, Row):-
    Padding is (Size - 3) // 2,
    create_list(Padding, A, '-'),
    append([A, ['t1', 'd1', 's1'], A], Row), !.

create_row(Index, Size, Row):-
    Index is Size-1,
    Padding is (Size - 3) // 2,
    create_list(Padding, A, '-'),
    append([A, ['t2', 'd2', 's2'], A], Row), !.

create_row(Index, Size, Row):-
    Index is (Size+1) // 2,
    Empty is Size - 2,
    create_list(Empty, A, 'x'),
    append([['r2'], A, ['r3']], Row), !.

create_row(Index, Size, Row):-
    Padding is abs((Size + 1) // 2 - Index),
    Rest is Size - 2 * Padding,
    create_list(Padding, A, '-'),
    create_list(Rest, B, 'x'),
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


last_elem('-', _).

last_elem(_, []):- write('|').

last_elem(_, [H | _]):-
    H = '-',
    write('|').

last_elem(_, [H | _]):-
    H \= '-'.


display_row([]). 

display_row([Element|Rest]) :-
    display_element(Element), 
    last_elem(Element, Rest),
    display_row(Rest).

% -----------------------------------------

display_element(-) :- write('    ').
display_element(x) :- write('|   ').
display_element(r1) :- write('| r ').
display_element(r2) :- write('| r ').
display_element(r3) :- write('| r ').
display_element(r4) :- write('| r ').
display_element(t1) :- write('| t ').
display_element(d1) :- write('| d ').
display_element(s1) :- write('| s ').
display_element(t2) :- write('| T ').
display_element(d2) :- write('| D ').
display_element(s2) :- write('| S ').
