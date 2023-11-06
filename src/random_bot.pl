% -----------------------------------------
% |     Computer Level 1 predicates       |
% -----------------------------------------

% random_troll_move(+GameState, +Direction, -NewGameState)
% Determines a random move for the troll on the game board and updates the board accordingly.
random_troll_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, NewMove, Turn]) :-
    position(t-Player, X-Y),
    new_pos(X-Y, Direction, NewX-NewY),
    (position(r-_, NewX-NewY) ->
        random_throw_rock(Board, t-Player, NewX-NewY, ThrowDir),
        throw_rock(Board, Turn, X-Y, t-Player, Direction, ThrowDir, NewBoard),
        format('I moved my stonetroll to (~d, ~d) and I threw the rock', [NewX, NewY]),
        NewMove is 3
    ;
    (opposite_direction(Direction, NewD), new_pos(X-Y, NewD, RockPos), position(r-_, RockPos) ->
        random(1, 2, Option),
        (Option == 1 ->
            format('I moved my stonetroll to (~d, ~d) and I pulled the rock', [NewX, NewY]),
            pull_rock(Board, Turn, X-Y, t-Player, Direction, NewBoard), NewMove is Move ;
            general_move(Board, t-Player, X-Y, NewX-NewY, NewBoard), NewMove is Move,
            format('I moved my stonetroll to (~d, ~d)', [NewX, NewY])
        );
        general_move(Board, t-Player, X-Y, NewX-NewY, NewBoard), NewMove is Move,
        format('I moved my stonetroll to (~d, ~d)', [NewX, NewY])
    )).


% -----------------------------------------

% random_throw_rock(+Board, +Player, +Position, -Direction)
% Randomly selects a direction to throw a rock from the specified position on the game board.
random_throw_rock(Board, _-Player, Position, Direction):-
    Directions = [1,2,3,4],
    findall(Dir, (member(Dir, Directions), valid_throw_direction(Board, Player, Position, Dir)), ValidDirs),
    random_member(Direction, ValidDirs).

% -----------------------------------------

% random_sorcerer_move(+GameState, +Direction, -NewGameState)
% Determines a random move for the sorcerer on the game board and updates the board accordingly.
random_sorcerer_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]):-
    position(s-Player, Pos),    
    new_pos(Pos, Direction, NewX-NewY),
    movable_rocks(Turn, Move, Board, Direction, Rocks),
    length(Rocks, Size),
       
    ( Size = 0 -> 
        general_move(Board, s-Player, Pos, NewX-NewY, NewBoard), 
        format('I moved my sorcerer to (~d, ~d)', [NewX, NewY]);   
    
    Size = 1 -> random(1, 2, Option),
    ( Option = 1 ->
       
        (chosen_rock(_, Turn, Move) ->
            continuous_levitation(Board, Move, Turn, s-Player, Pos, NewX-NewY, Direction, NewBoard),
            format('I moved my sorcerer to (~d, ~d) and I levitated the same rock', [NewX, NewY]);
            random_first_levitation(Board, Move, Turn, s-Player, Pos, NewX-NewY, Direction, Rocks, NewBoard)
        );   
        general_move(Board, s-Player, Pos, NewX-NewY, NewBoard),
        format('I moved my sorcerer to (~d, ~d)', [NewX, NewY]) 
    );   
    
    random(1, 2, Option),
    ( Option = 1 ->
        random_first_levitation(Board, Move, Turn, s-Player, Pos, NewX-NewY, Direction, Rocks, NewBoard);   
        general_move(Board, s-Player, Pos, NewX-NewY, NewBoard),
        format('I moved my sorcerer to (~d, ~d)', [NewX, NewY])
    )).

% -----------------------------------------

% random_first_levitation(+Board, +Move, +Turn, +Sorcerer, +Pos, -NewX-Newy, +Direction, +Rocks, -NewBoard)
% Randomly selects a rock to start levitating
random_first_levitation(Board, Move, Turn, Sorcerer, Pos, NewX-NewY, Direction, Rocks, NewBoard):-
    random_member(Rock, Rocks),
    position(Rock, RockX-RockY),
    new_pos(RockX-RockY, Direction, NewRockPos),
    general_move(Board, Rock, RockX-RockY, NewRockPos, Temp),
    general_move(Temp, Sorcerer, Pos, NewX-NewY, NewBoard),  
    N is Move + 1,
    asserta(chosen_rock(Rock, Turn, N)), 
    asserta(moved_rocks(Rock, Turn)),
    format('I moved my sorcerer to (~d, ~d) and I levitated the rock in (~d,~d)', [NewX, NewY, RockX, RockY]).