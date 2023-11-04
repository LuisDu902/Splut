% -----------------------------------------
% |     Computer Level 1 predicates       |
% -----------------------------------------

% choose_random_piece(-Piece)
% Randomly selects a piece from the list of game pieces [t, d, s] and unifies it with the variable Piece.
choose_random_piece(Piece) :-
    List = [t, d, s], 
    random_member(Piece, List).

% -----------------------------------------

% choose_random_direction(-D)
% Randomly selects a direction (1 to 4) and unifies it with the variable D.
choose_random_direction(D) :-
    random(1, 4, D).

% -----------------------------------------


% -----------------------------------------

% Determines a random move for the troll on the game board and updates the board accordingly.
random_troll_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, NewMove, Turn]) :-
    position(t-Player, Pos),
    new_pos(Pos, Direction, NewPos),
    (position(r-_, NewPos) ->
        random_throw_rock(Board, t-Player, NewPos, ThrowDir),
        format('I am throwing a rock in ~d direction\n', [ThrowDir]),
        throw_rock(Board, Turn, Pos, t-Player, Direction, ThrowDir, NewBoard),
        NewMove is 3
    ;
    (opposite_direction(Direction, NewD), new_pos(Pos, NewD, RockPos), position(r-_, RockPos) ->
        random(1, 2, Option),
        (Option == 1 ->
            format('I am pulling a rock\n', []),
            pull_rock(Board, Turn, Pos, t-Player, Direction, NewBoard), NewMove is Move ;
            general_move(Board, t-Player, Pos, NewPos, NewBoard), NewMove is Move
        );
        general_move(Board, t-Player, Pos, NewPos, NewBoard), NewMove is Move
    )).


% -----------------------------------------

% random_throw_rock(+Board, +Player, +Position, -Direction)
% Randomly selects a direction to throw a rock from the specified position on the game board.
random_throw_rock(Board, _-Player, Position, Direction):-
    repeat,
    choose_random_direction(Direction),
    (valid_throw_direction(Board, Player, Position, Direction) -> true; fail).

% -----------------------------------------


random_sorcerer_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]):-
    position(s-Player, Pos),    
    new_pos(Pos, Direction, NewPos),
    movable_rocks(Turn, Move, Board, Direction, Rocks),
    length(Rocks, Size),
    format('Movable rocks : ~d\n', [Size]),
       
    ( Size = 0 -> general_move(Board, s-Player, Pos, NewPos, NewBoard);   
    
    Size = 1 -> random(1, 2, Option),
    ( Option = 1 ->
        write('i chose to levitate\n'),
        (chosen_rock(_, Turn, Move) ->
            continuous_levitation(Board, Move, Turn, s-Player, Pos, NewPos, Direction, NewBoard);
            random_first_levitation(Board, Move, Turn, s-Player, Pos, NewPos, Direction, Rocks, NewBoard)
        );   
         write('i chose to not levitate\n'),
        general_move(Board, s-Player, Pos, NewPos, NewBoard)
    );   
    
    random(1, 2, Option),
    ( Option = 1 ->
         write('i chose to levitate\n'),
        random_first_levitation(Board, Move, Turn, s-Player, Pos, NewPos, Direction, Rocks, NewBoard);   
         write('i chose to not levitate\n'),
        general_move(Board, s-Player, Pos, NewPos, NewBoard)
    )).


random_first_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, Rocks, NewBoard):-
    random_member(Rock, Rocks),
    position(Rock, RockPos),
    new_pos(RockPos, Direction, NewRockPos),
    general_move(Board, Rock, RockPos, NewRockPos, Temp),
    general_move(Temp, Sorcerer, Pos, NewPos, NewBoard),  
    N is Move + 1,
    asserta(chosen_rock(Rock, Turn, N)), 
    asserta(moved_rocks(Rock, Turn)).