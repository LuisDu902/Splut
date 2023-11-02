% -----------------------------------------
% |           Dwarf predicates           |
% -----------------------------------------

% valid_dwarf_move(+Board, +X-Y, +Direction)
% determines if the intended move is valid given that
% the dwarf is the one performing the movement
valid_dwarf_move(Board, X-Y, Direction):-
    new_pos(X-Y, Direction, NewPos),
    (\+ position(_-_, NewPos) -> true ; (can_push(Board, X-Y, Direction))).


% can_push(+Board, +X-Y, +Direction)
% determines if there is room for the dwarf
% to push things in the direction of the movement
can_push(Board, X-Y, Direction) :-
    length(Board, Size),
    ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
      nth1(Y, Board, List), get_remaining(X, List, Size, Rest, Direction)),
    memberchk(x-x, Rest).
    
% -----------------------------------------

% dwarf_move(+Board, +Dwarf, +Pos, +Direction, -NewBoard)
% moves the dwarf taking into consideration the fact
% that he has to push the other pieces with him
dwarf_move(Board, Dwarf, X-Y, Direction, NewBoard) :-
    new_pos(X-Y, Direction, NewPos),
    write('dwarf moving\n'),
    (position(_-_, NewPos) -> 
        write('pushing pieces\n'),
        push(Board, X-Y, Direction, Temp),
        general_move(Temp, Dwarf, X-Y, NewPos, NewBoard)
    ; general_move(Board, Dwarf, X-Y, NewPos, NewBoard)).


push(Board, X-Y, Direction, NewBoard):-
   length(Board, Size),
   ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
   nth1(Y, Board, List), write('getting remaing list\n'), get_remaining(X, List, Size, Rest, Direction)),
   write('Getting list : '),
   print_list(Rest),
   delete(Rest, x-x, Temp),
   delete(Temp, e-e, Pieces),
   write('Getting updated list : '),
   print_list(Pieces),
   push_pieces(Board, Pieces, Direction, NewBoard).

push_pieces(Board, [], _, Board).
push_pieces(Board, [Piece|Rest], Direction, NewBoard) :-
    position(Piece, Pos),
    new_pos(Pos, Direction, NewPos), 
    update_piece_pos(Piece, NewPos),
    put_piece(Board, Piece, NewPos, TempBoard),
    push_pieces(TempBoard, Rest, Direction, NewBoard).


print_list([]). 
print_list([H|T]) :-
    write(H), 
    write(' '), 
    print_list(T). 