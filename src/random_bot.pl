% -----------------------------------------
% |     Computer Level 1 predicates       |
% -----------------------------------------

choose_random_piece(Piece) :-
    List = [t, d, s], 
    random_member(Piece, List).

% -----------------------------------------

choose_random_direction(D) :-
    random(1, 4, D).

% -----------------------------------------

choose_random_move(Board, Player, 1, Piece-D) :-
    length(Board, Size),
    repeat,
    choose_random_piece(Piece),
    position(Piece-Player, X-Y),
    choose_random_direction(D),
    (valid_move(Board, Piece-Player, X-Y, Size, D) -> format('I chose to move ~a-~a from (~d,~d) -> ~d\n', [Piece, Player, X, Y, D]) ; fail).

% -----------------------------------------

random_troll_move(Board, Troll, Pos, Direction, NewBoard) :-
    new_pos(Pos, Direction, NewPos),
    (position(r-_, NewPos) ->
        random_throw_rock(Board, Troll, NewPos, ThrowDir),
        format('I am throwing a rock in ~d direction\n', [ThrowDir]),
        throw_rock(Board, Pos, Troll, Direction, ThrowDir, NewBoard)
    ;
    (opposite_direction(Direction, NewD), new_pos(Pos, NewD, A-B), position(r-_, A-B) ->
        random(1, 2, Option),
        (Option == 1 ->
            pull_rock(Board, Pos, Troll, Direction, NewBoard) ;
            general_move(Board, Troll, Pos, NewPos, NewBoard)
        );
        general_move(Board, Troll, Pos, NewPos, NewBoard)
    )).

% -----------------------------------------

random_throw_rock(Board, _-Player, Position, Direction):-
    repeat,
    choose_random_direction(Direction),
    (valid_throw_direction(Board, Player, Position, Direction) -> true; fail).

% -----------------------------------------
