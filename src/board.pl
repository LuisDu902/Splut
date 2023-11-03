% -----------------------------------------
% |           Board predicates            |
% -----------------------------------------

% position(?Piece, ?Position)
% Finds the position of a piece, or the piece in a give position
:- dynamic position/2.

% create_list(+Size, +Elem, -[Elem|Rest])
% Creates a list of the specified size where all elements are unified with the given Element.
create_list(0, _, []).
create_list(Size, Elem, [Elem|Rest]) :-
    Size > 0,
    Size1 is Size - 1,
    create_list(Size1, Elem, Rest).

% -----------------------------------------

% assert_pieces(+Pieces, +Row, +Col)
% Asserts the positions of pieces on the game board.
assert_pieces([], _, _).
assert_pieces([Piece|Rest], Row, Col) :-
    NextCol is Col+1,
    asserta(position(Piece, NextCol-Row)),
    assert_pieces(Rest, Row, NextCol).

% -----------------------------------------

% create_row(+Index, +Size, -Row)
% Generates a row for the game board based on the given Index and Size.
create_row(1, Size, Row):-
    Padding is (Size - 1 ) // 2,
    create_list(Padding, e-e, A),
    B = [r-1],
    append([A, B, A], Row),   
    assert_pieces(B, 1, Padding), !.

create_row(Size, Size, Row):-
    Padding is (Size - 1 ) // 2,
    create_list(Padding, e-e, A),
    B = [r-4],
    append([A, B, A], Row), 
    assert_pieces(B, Size, Padding), !.

create_row(2, Size, Row):-
    Padding is (Size - 3) // 2,
    create_list(Padding, e-e, A),
    B = [d-p2, t-p2, s-p2],
    append([A, B, A], Row),
    assert_pieces(B, 2, Padding), !.

create_row(Index, Size, Row):-
    Index is Size-1,
    Padding is (Size - 3) // 2,
    create_list(Padding, e-e, A),
    B = [s-p1, t-p1, d-p1],
    append([A, B, A], Row), 
    assert_pieces(B, Index, Padding), !.

create_row(Index, Size, Row):-
    Index is (Size+1) // 2,
    Empty is Size - 2,
    create_list(Empty, x-x, A),
    append([[r-2], A, [r-3]], Row), 
    asserta(position(r-2, 1-Index)),
    asserta(position(r-3, Size-Index)), !.

create_row(Index, Size, Row):-
    Padding is abs((Size + 1) // 2 - Index),
    Rest is Size - 2 * Padding,
    create_list(Padding, e-e, A),
    create_list(Rest, x-x, B),
    append([A, B, A], Row), !.

% -----------------------------------------

% create_board(+Size, -Board)
% Generates a game board of the specified size.
create_board(Size, Board):-
    create_board_aux(1, Size, [], Board).

% create_board_aux(+Index, +Size, +AuxBoard, -Board)
% Auxiliary predicate for generating a game board of the specified size.
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

% display_board(+Board)
% Displays the given game board.
display_board(Board) :-
    length(Board, Size),
    display_col_index(1, Size),
    display_rows(Board, Size, 1).

% -----------------------------------------

% display_col_index(+Column, +Size)
% Displays the column indices from Column to Size.
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
    
% display_sep_line(+Index, +Size)
% Displays a separator line for the game board.
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

% display_padding(+N)
% Displays a specific number of padding spaces.
display_padding(0).

display_padding(N) :-
    N > 0,
    write('    '),
    NewN is N - 1,
    display_padding(NewN).

% -----------------------------------------
    
% display_dashes(+N, +Dashes)
% Displays a specific number of dashes for the separator line.
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

% display_rows(+Rows, +Size, +Index)
% Displays the rows of the game board along with separator lines and row indices.
display_rows([], Size, _) :- display_sep_line(Size, 1).

display_rows([Row|Rest], Size, Index) :-
    display_sep_line(Size, Index),
    display_row(Row),
    format(' ~d\n', [Index]),
    NewIndex is Index + 1,
    display_rows(Rest, Size, NewIndex).

% -----------------------------------------

% last_elem(+Element, +List)
% Determines the last element in a list that matches the specified Element.
last_elem(e-e, _).
last_elem(_, []):- write('|').
last_elem(_, [H | _]):- H = e-e, write('|').
last_elem(_, [H | _]):- H \= e-e.

% -----------------------------------------

% display_row(+Row)
% Displays a row of the game board.
display_row([]). 
display_row([Element|Rest]) :-
    display_element(Element), 
    last_elem(Element, Rest),
    display_row(Rest).

% -----------------------------------------

% color(+Element, -ColorCode)
% Associates color codes with specific game board elements.
color(r-_, 33) :- !.
color(_-p1, 34) :- !.
color(_-p2, 31) :- !.
color(_-_, 0) :- !.

% -----------------------------------------

% display_element(+Element)
% Displays an individual element in the row, with specific formatting and colors.
display_element(x-x) :- write('|   '), !.
display_element(X-Y) :- 
    color(X-Y, ColorCode),
    (ColorCode \= 0 -> format('| \e[~dm~w\e[0m ', [ColorCode, X]); write('    ')).

% -----------------------------------------

% put_piece(+Board, +Piece, +X-Y, -NewBoard)
% Places a specific piece at the given coordinates on the game board.
put_piece(Board, Piece, X-Y, NewBoard) :-
    nth1(Y, Board, Row),
    replace(X, Piece, Row, NewRow),
    replace(Y, NewRow, Board, NewBoard).

% -----------------------------------------

% swap_places(+Board, +Piece1, +Pos1, +Piece2, +Pos2, -NewBoard)
% Swaps the positions of two specified pieces on the game board.
swap_places(Board, Piece1, Pos1, Piece2, Pos2, NewBoard) :-
    put_piece(Board, Piece1, Pos2, Temp),
    put_piece(Temp, Piece2, Pos1, NewBoard).

% -----------------------------------------

% get_col(+N, +Board, -Column)
% Retrieves the N-th column from the given game board.
get_col(_, [], []).
get_col(N, [Row|Rest], [Element|Column]) :-
    nth1(N, Row, Element),
    get_col(N, Rest, Column).