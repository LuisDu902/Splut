% -----------------------------------------
% |           Dwarf predicates           |
% -----------------------------------------

% valid_dwarf_move(+Board, +X-Y, +Direction)
% determines if the intended move is valid given that
% the dwarf is the one performing the movement
valid_dwarf_move(Board, X-Y, Direction):-
    new_pos(X-Y, Direction, NewPos),
    write('here\n'),
    (\+ position(_-_, NewPos) -> true ; (can_push(Board, X-Y, Direction))).


% can_push(+Board, +X-Y, +Direction)
% determines if there is room for the dwarf
% to push things in the direction of the movement
can_push(Board, X-Y, Direction) :-
    length(Board, Size),
    write('fuck\n'),
    ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
      nth1(Y, Board, List), get_remaining(X, List, Size, Rest, Direction)),
    write('nigga\n'),
    memberchk(x-x, Rest).
    
% -----------------------------------------

% dwarf_move(+Board, +Dwarf, +Pos, +Direction, -NewBoard)
% moves the dwarf taking into consideration the fact
% that he has to push the other pieces with him
dwarf_move(Board, Dwarf, X-Y, Direction, NewBoard) :-
    new_pos(X-Y, Direction, NewPos),
    write('balls\n'),
    (position(_-_, NewPos) -> length(Board, Size),
    write('this far\n'),
    ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
      nth1(Y, Board, List), get_remaining(X, List, Size, Rest, Direction)),
    write('this further\n'),
    push(Board, Rest, Direction, Temp),
    general_move(Temp, Dwarf, X-Y, NewPos, NewBoard)
    ; general_move(Board, Dwarf, X-Y, NewPos, NewBoard)).


% push(+Board, +[Elem | Rest], +Direction, -NewBoard) 
% recursively updates the position of pieces in the way of the dwarf, 
% considering the direction that he is pushing him to
push(_, [], _, _).

push(Board, [Elem | Rest], Direction, NewBoard) :-
    ((Elem \= x-x , Elem \= e-e) -> position(Elem, Pos),
    new_pos(Pos, Direction, NewPos),
    general_move(Board, Elem, Pos, NewPos, Temp), 
    push(Temp, Rest, Direction, NewBoard)
    ; push(Board, Rest, Direction, NewBoard)).

