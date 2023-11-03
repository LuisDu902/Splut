% ----------------------------------------
% |         Stonetoll predicates         |
% ----------------------------------------

% valid_troll_move(+Pos, +Direction)
% Checks if a troll's move to a new position in the specified direction is valid.
valid_troll_move(Pos, Direction):-
    new_pos(Pos, Direction, NewPos),
    ((position(r-_, NewPos); \+ position(_-_, NewPos)) -> true ; false).

% ----------------------------------------

% valid_throw_direction(+Board, +Player, +Position, +Direction)
% Checks if a throw direction from a specified position is valid.
valid_throw_direction(Board, Player, Position, Direction) :-
    length(Board, Size),
    new_pos(Position, Direction, NewPos),
    inside_board(NewPos, Size),
    next_player(Player, P),
    \+ position(r-_, NewPos),
    \+ position(t-P, NewPos).

% ----------------------------------------

% pull_rock_option(-Option)
% Displays the pull rock option to the player and reads the player choice.
pull_rock_option(Option):-
    write('\nDo you want to pull the rock right behind the Stonetroll?\n\n'),
    write('[1] Yes\n'),
    write('[2] No\n\n'),
    select_option(1, 2, Option).   

% ----------------------------------------

% pull_rock(+Board, +Position, +Troll, +Direction, -NewBoard)
% Handles the process of pulling a rock in a specified direction.
pull_rock(Board, Position, Troll, Direction, NewBoard) :-
    opposite_direction(Direction, NewD), new_pos(Position, NewD, A-B), position(Rock, A-B),
    new_pos(Position, Direction, NewPos),
    update_piece_pos(Troll, NewPos),
    update_piece_pos(Rock, Position),
    swap_places(Board, Troll, Position, x-x, NewPos, Temp),
    swap_places(Temp, Rock, A-B, x-x, Position, NewBoard).

% ----------------------------------------

% throw_rock_menu(-Direction)
% Displays the throw rock menu to the player and reads the player chosen direction.
throw_rock_menu(Direction):-
    write('\nYou have found a rock! Which direction do you want to throw it?\n\n'),
    write('[1] Up\n'),
    write('[2] Down\n'),
    write('[3] Left\n'),
    write('[4] Right\n\n'),
    select_option(1, 4, Direction).

% ----------------------------------------

% throw_rock_option(+Board, +Player, +Position, -Direction)
% Handles the process of allowing the player to choose the direction to throw a rock.
throw_rock_option(Board, _-Player, Position, Direction):-
    repeat,
    throw_rock_menu(Direction),
    (valid_throw_direction(Board, Player, Position, Direction) -> true;
        write('You cannot throw in this direction! Choose a valid direction: '),
        fail
    ).

% ----------------------------------------

% throw_rock(+Board, +Pos, +Troll, +Dir, +ThrowDir, -NewBoard)
% Handles the process of throwing a rock in the specified direction.
throw_rock(Board, Pos, Troll, Dir, ThrowDir, NewBoard) :-
    new_pos(Pos, Dir, NewPos),
    position(Rock, NewPos),
    update_piece_pos(Troll, NewPos),
    swap_places(Board, Troll, Pos, x-x, NewPos, Tmp),
    move_rock(Tmp, Rock, ThrowDir, NewBoard).

% ----------------------------------------

% troll_move(+Board, +Troll, +Pos, +Direction, -NewBoard)
% Handles the movement of the stonetroll piece.
troll_move(Board, Troll, Pos, Direction, NewBoard) :-
    new_pos(Pos, Direction, NewPos),
    (position(r-_, NewPos) ->
        throw_rock_option(Board, Troll, NewPos, ThrowDir),
        throw_rock(Board, Pos, Troll, Direction, ThrowDir, NewBoard)
    ;
    (opposite_direction(Direction, NewD), new_pos(Pos, NewD, A-B), position(r-_, A-B) ->
        pull_rock_option(Option),
        (Option == 1 ->
            pull_rock(Board, Pos, Troll, Direction, NewBoard) ;
            general_move(Board, Troll, Pos, NewPos, NewBoard)
        );
        general_move(Board, Troll, Pos, NewPos, NewBoard)
    )).

% ----------------------------------------

% move_rock(+Board, +Rock, +Direction, -NewBoard)
% Moves a rock piece in the specified direction on the game board.
move_rock(Board, Rock, Direction, NewBoard):-
    position(Rock, X-Y),
    length(Board, Size),
    ((Direction =:= 1; Direction =:= 2) -> get_col(X, Board, List), get_remaining(Y, List, Size, Rest, Direction);   
      nth1(Y, Board, List), get_remaining(X, List, Size, Rest, Direction)),
    new_rock_pos(Rest, X-Y, Direction, NewX-NewY),
    format('THE ROCK SHOULD GO FROM (~d,~d) TO (~d,~d)\n', [X, Y, NewX, NewY]),
    (position(Piece, NewX-NewY) -> retract(position(Piece, NewX-NewY)); true),
    update_piece_pos(Rock, NewX-NewY), !,
    put_piece(Board, Rock, NewX-NewY, NewBoard).

% ----------------------------------------

% new_rock_pos(+List, +Pos, +Direction, -NewPos)
% Calculates the new position for a rock based on the specified direction and obstacles in its path.
new_rock_pos([], Pos, _, Pos).

% move up
new_rock_pos(List, X-Y, 1, X-NewY):-
    find_reverse_elem(List, [r-_, t-_, e-e, s-_], A),
    TmpY is Y - A - 1,
    (position(s-_, X-TmpY) -> NewY is TmpY; NewY is Y - A).

% move down
new_rock_pos(List, X-Y, 2, X-NewY):-
    find_elem(List, [r-_, t-_, e-e, s-_], A),
    TmpY is Y + A + 1,
    (position(s-_, X-TmpY) -> NewY is TmpY; NewY is Y + A).

% move left
new_rock_pos(List, X-Y, 3, NewX-Y):-
    find_reverse_elem(List, [r-_, t-_, e-e, s-_], A),
    TmpX is X - A - 1,
    (position(s-_, TmpX-Y) -> NewX is TmpX; NewX is X - A).

% move right
new_rock_pos(List, X-Y, 4, NewX-Y):-
    find_elem(List, [r-_, t-_, e-e], A),
    TmpX is X + A + 1,
    (position(s-_, TmpX-Y) -> NewX is TmpX; NewX is X + A).
