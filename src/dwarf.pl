% -----------------------------------------
% |           Dwarf predicates            |
% -----------------------------------------

% valid_dwarf_move(+Board, +Pos, +Direction)
% Determines if the intended move is valid given that the dwarf is the one performing the movement
valid_dwarf_move(Board, Pos, Direction):-
    new_pos(Pos, Direction, NewPos),
    (\+ position(_-_, NewPos) -> true ; (can_push(Board, Pos, Direction))).

% -----------------------------------------

% can_push(+Board, +X-Y, +Direction) 
% Determines if there is room for the dwarf to push things in the direction of the movement
can_push(Board, X-Y, Direction) :-
    length(Board, Size),
    ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
      nth1(Y, Board, List), get_remaining(X, List, Size, Rest, Direction)),
    memberchk(x-x, Rest).

% -----------------------------------------

% Moves the dwarf taking into consideration the fact that he has to push the other pieces with him
dwarf_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]) :-
    position(d-Player, Pos),
    new_pos(Pos, Direction, NewPos),
    (position(_-_, NewPos) -> 
        push(Board, Turn, Pos, Direction, Temp),
        general_move(Temp, d-Player, Pos, NewPos, NewBoard)
    ; general_move(Board, d-Player, Pos, NewPos, NewBoard)).

% -----------------------------------------

% push(+Board, +X-Y, +Direction, -NewBoard)
% Pushes all pieces next to the dwarf in the direction of the movement one space
push(Board, Turn, X-Y, Direction, NewBoard):-
   length(Board, Size),
   ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
   nth1(Y, Board, List), get_remaining(X, List, Size, Rest, Direction)),
   get_push_pieces(Rest, Direction, Pieces),
   push_pieces(Board, Turn, Pieces, Direction, NewBoard).

% -----------------------------------------

% push_pieces(+Board, +[Piece|Rest], +Direction, -NewBoard)
% Push the first piece in the specified direction and continue with the rest of the pieces.
push_pieces(Board, _, [], _, Board).
push_pieces(Board, Turn, [Piece|Rest], Direction, NewBoard) :-
    (Piece = r-_, asserta(moved_rocks(Piece, Turn)); true),
    position(Piece, Pos),
    new_pos(Pos, Direction, NewPos), 
    update_piece_pos(Piece, NewPos),
    put_piece(Board, Piece, NewPos, TempBoard),
    push_pieces(TempBoard, Turn, Rest, Direction, NewBoard).

% -----------------------------------------

% get_push_pieces(+List, +Direction, -Pieces)
% Determines the list of pieces in the specified direction that need to be pushed by the dwarf
get_push_pieces(List, Direction, Pieces):-
    ((Direction =:= 2; Direction =:= 4) -> find_elem(List, [x-x], Len), sublist(List, Pieces, 0, Len);
    find_reverse_elem(List, [x-x], Len), sublist(List, Pieces, _, Len, 0)).
  
print_list([]). 
print_list([H|T]) :-
    write(H), 
    write(' '), 
    print_list(T). 

