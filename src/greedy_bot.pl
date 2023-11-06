% -----------------------------------------
% |     Computer Level 2 predicates       |
% -----------------------------------------

% greedy_move(-Moves, +NrMove, +Turn)
% Finds the greedy move to be done in move "NrMove" and turn "Turn"
:- dynamic greedy_move/3.

% first_moves
% Asserts initial greedy moves for the first two rounds
first_moves :-
    asserta(greedy_move(d-1, 1, 1)),
    asserta(greedy_move(d-2, 1, 2)),
    asserta(greedy_move(t-4, 2, 2)).

% -----------------------------------------

% same_axis(+Piece1, +Piece2, -Axis)
% Determines if two pieces are on the same x-axis (horizontal) or y-axis (vertical).
same_axis(Piece1, Piece2, x) :- 
    position(Piece1, X1-_),
    position(Piece2, X2-_),
    (X1 =:= X2), !.

same_axis(Piece1, Piece2, y) :-
    position(Piece1, _-Y1),
    position(Piece2, _-Y2),
    (Y1 =:= Y2), !.

% -----------------------------------------

% clear_shot(+Board, +Player, +Rock, +Axis)
% Determines if it is possible to kill Player using the specified Rock in the Axis
clear_shot(Board, Player, Rock, Axis) :-
    position(Rock, X-Y),
    position(s-Player, SX-SY),
    length(Board, Size),
    (Axis == x -> get_col(X, Board, List), 
        ((SY < Y) -> 
            get_remaining(Y, List, Size, Rest, 1), new_rock_pos(Rest, X-Y, 1, NewPos);
            get_remaining(Y, List, Size, Rest, 2), new_rock_pos(Rest, X-Y, 2, NewPos)
        );   
        nth1(Y, Board, List), 
        ((SX < X) -> 
            get_remaining(X, List, Size, Rest, 3),  new_rock_pos(Rest, X-Y, 3, NewPos);
            get_remaining(X, List, Size, Rest, 4),  new_rock_pos(Rest, X-Y, 4, NewPos))),   
    position(s-Player, NewPos).

% -----------------------------------------

% dangerous_rocks(+Board, +Player, -Rock)
% Determines all the dangerous rocks that can attack the Player
dangerous_rocks(Board, Player, Rock) :-
    Rocks = [r-1, r-2, r-3, r-4],
    setof(R, (member(R, Rocks), same_axis(s-Player, R, Axis), clear_shot(Board, Player, R, Axis)), Rock).

% -----------------------------------------

% reachable_rocks(+Board, +Player, -ReachableRocks, +Rocks)
% Determines all the rocks that Player can reach within their turn
reachable_rocks(Board, Player, ReachableRocks, Rocks) :-
    position(t-Player, X-Y),
    troll_paths(Board, X-Y, Paths),
    findall(Rock, (
        member(Rock, Rocks),
        position(Rock, RX-RY),
        findall(Path, (member(Path, Paths), last(Path, LX-LY), LX=RX, LY=RY), ChosenPaths),
        length(ChosenPaths, L), !, 
        L \= 0
    ), ReachableRocks).

% -----------------------------------------

% can_attack(+Board, +Player)
% Determines whether a Player can attack their opponent
can_attack(Board, Player) :-
    next_player(Player, Opponent),
    dangerous_rocks(Board, Opponent, Rocks),
    reachable_rocks(Board, Player, ThrowableRocks, Rocks),
    length(ThrowableRocks, Tlen), !,
    Tlen \= 0 .

% -----------------------------------------~

% attack(+GameState, -Move) :-
% Handles an attack to the opponent's sorcerer
attack([Board, Player, _, Turn], Move) :-
    next_player(Player, Opponent),
    dangerous_rocks(Board, Opponent, Rocks),
    reachable_rocks(Board, Player, ThrowableRocks, Rocks),
    random_member(TheRock, ThrowableRocks),
    position(TheRock, TheRockPos),
    position(t-Player, Pos),
    troll_paths(Board, Pos, Paths),
    findall(Path, (member(Path, Paths), last(Path, LastPos), LastPos = TheRockPos), ChosenPaths),
    random_member(ThePath, ChosenPaths),
    greedy_piece(t, ThePath, Turn),
    greedy_move(Move, 1, Turn).

% ----------------------------------------- 

% need_protection(+Board, +Player) 
% Determines whether a Player needs to protect their sorcerer
need_protection(Board, Player) :-
    next_player(Player, Opponent),
    can_attack(Board, Opponent).

% -----------------------------------------

% is_safe(+X-Y, +Player)
% Determines if the position (X,Y) is safe from opponent's attack 
is_safe(X-Y, Player) :-
    next_player(Player, Opponent),
    AttackingPieces = [r-1, r-2, r-3, r-4, t-Opponent],
    findall(Piece, (member(Piece, AttackingPieces),
        position(Piece, PX-PY),
        (X =:= PX ; Y =:= PY)), Pieces),
    length(Pieces, L),
    L = 0.

% -----------------------------------------

% protect_sorcerer(+GameState, -Move) :-
% Protects players sorcerer, moving it to a safer position
protect_sorcerer([Board, Player, 1, Turn], Move) :-
    position(s-Player, Pos),
    possible_paths(Board, Pos, Paths),
    findall(Path, (member(Path, Paths), last(Path, NewPos), is_safe(NewPos, Player)), SafePaths),
    random_member(ChosenPath, SafePaths),
    greedy_piece(s, ChosenPath, Turn),
    greedy_move(Move, 1, Turn).

% -----------------------------------------

% reach_pos(+Size, +Path, -PossiblePaths) :-
% Determines all reachable positions within the playing board
reach_pos(_, [], []).
reach_pos(Size, [List|R], [PossiblePos|Rest]) :-
    Directions = [1, 2, 3, 4],
    last(List, Pos),
    findall(ListWithNewPos, (
        member(Direction, Directions),
        new_pos(Pos, Direction, NewPos),
        inside_board(NewPos, Size),
        \+ position(_, NewPos),
        append(List, [NewPos], ListWithNewPos)
    ), PossiblePos),
    reach_pos(Size, R, Rest).
    
% -----------------------------------------

% troll_reach(+Size, +Path, -PossiblePaths) :-
% Determines all reachable positions by a stonetroll within the playing board
troll_reach(_, [], []).
troll_reach(Size, [List|R], [PossiblePos|Rest]) :-
    Directions = [1, 2, 3, 4],
    last(List, Pos),
    findall(ListWithNewPos, (
        member(Direction, Directions),
        new_pos(Pos, Direction, NewPos),
        inside_board(NewPos, Size),
        (\+ position(_, NewPos); position(r-_, NewPos)),
        append(List, [NewPos], ListWithNewPos)
    ), PossiblePos),
    troll_reach(Size, R, Rest).

% -----------------------------------------

% troll_paths(+Board, +Pos, -Positions) :-
% Determines all possible paths that a stonetroll can take
troll_paths(Board, Pos, Positions) :-
    length(Board, Size),
    troll_reach(Size, [[Pos]], A), append(A, First),
    troll_reach(Size, First, B), append(B, Second),
    troll_reach(Size, Second, C), append(C, Third),
    append([First, Second , Third], Posi),
    remove_dups(Posi, Positions).

% -----------------------------------------

% general_paths(+Board, +Pos, -Positions, +N) :-
% Determines all possible paths that a general piece can take within N moves
general_paths(Board, Pos, Positions, N) :-
    length(Board, Size),
    reach_pos(Size, [[Pos]], A), append(A, First),
    reach_pos(Size, First, B), append(B, Second),
    reach_pos(Size, Second, C), append(C, Third),
    nth1(N, [First, Second, Third], Positions).

% -----------------------------------------

% possible_paths(+Board, +Pos, -Positions) :-
% Determines all possible paths that a sorcerer can take in 3 moves
possible_paths(Board, Pos, Positions) :-
    length(Board, Size),
    reach_pos(Size, [[Pos]], F), append(F, First),
    reach_pos(Size, First, Second), append(Second, NonReachable),
    reach_pos(Size, NonReachable, Final), append(Final, Posi),
    remove_dups(Posi, Positions).

% -----------------------------------------

% greedy_piece(+Piece, +Path, +Turn)
% Asserts in greedy_move/3 the list of moves to be done in a Turn
greedy_piece(_, [], _).
greedy_piece(Piece, [H|T], Turn) :-
    greedy_piece_helper(Piece, [H|T], 1, Turn).

greedy_piece_helper(_, [], _, _).
greedy_piece_helper(_, List, _, _) :-
    length(List, Size),
    Size = 1, !.

greedy_piece_helper(Piece, [A|Rest], Index, Turn) :-
    NewIndex is Index + 1,
    nth1(1, Rest, NextElement),
    new_pos(A, D, NextElement),
    asserta(greedy_move(Piece-D, Index, Turn)),
    greedy_piece_helper(Piece, Rest, NewIndex, Turn).

% -----------------------------------------

% greedy_sorcerer_move(+GameState, +Direction, -NewGameState)
% Handles a greedy sorcerer move
greedy_sorcerer_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]) :-
    position(s-Player, Pos),
    new_pos(Pos, Direction, NewX-NewY),
    general_move(Board, s-Player, Pos, NewX-NewY, NewBoard),
    format('I moved my sorcerer to (~d, ~d)', [NewX, NewY]).

% -----------------------------------------

% can_protect(+GameState)
% Determines if it is possible to protect the sorcerer, moving it to a safer position
can_protect([Board, Player, _, _]) :-
    position(s-Player, Pos),
    possible_paths(Board, Pos, Paths),
    findall(Path, (member(Path, Paths), last(Path, NewPos), is_safe(NewPos, Player)), SafePaths),
    length(SafePaths, Size),
    Size > 0.

% -----------------------------------------

% get_dist(+Rock, +Player, -Dist)
% Determines the smallest distance between a Player and a Rock
get_dist(Rock, Player, Dist) :-
    Pieces = [s, t, d],
    findall(D, (
        member(Piece, Pieces),
        position(Piece-Player, X-Y),
        position(Rock, RX-RY),
        D is abs(X - RX) + abs(Y - RY)
    ), Dists),
    min_member(Dist, Dists).

% -----------------------------------------

% get_dist(+Rock, +Player, -Dist)
% Determines the closest Rock to a Player
closest_rock(Player, Rock, Dist) :-
    Rocks = [r-1, r-2, r-3, r-4],
    findall(D-Rock, (
        member(Rock, Rocks),
        get_dist(Rock, Player, D)
    ), Distances),
    min_member(Dist-Rock, Distances).

% -----------------------------------------

% greedy(+Piece, +GameState, -Move)
% Chooses one greedy list of moves to be done in the turn
greedy(Piece, [Board, Player, 1, Turn], Move) :-
    position(Piece-Player, Pos),
    possible_paths(Board, Pos, Paths),
    random_member(ChosenPath, Paths),
    greedy_piece(d, ChosenPath, Turn),
    greedy_move(Move, 1, Turn).

% -----------------------------------------

% move_closer(+GameState, -Move)
% Moves greedyly the pieces closer to the rocks
move_closer([Board, Player, 1, Turn], Move) :-
    closest_rock(Player, _, Dist),
    (Dist < 3-> 
        greedy(d, [Board, Player, 1, Turn], Move); 
        greedy(t, [Board, Player, 1, Turn], Move)
    ).

% -----------------------------------------


% greedy_troll_move(+GameState, +Direction, -NewGameState)
% Handles a greedy stronetroll move
greedy_troll_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, NewMove, Turn]) :-
    position(t-Player, Pos),    
    new_pos(Pos, Direction, NewX-NewY), 
    (position(r-_, NewX-NewY) ->
        next_player(Player, Opponent),
        position(s-Opponent, SorcX-SorcY),
        (
            (NewX < SorcX, SorcY = NewY) -> ThrowDir is 4; 
            (SorcX < NewX, SorcY = NewY) -> ThrowDir is 3; 
            (SorcY < NewY, SorcX = NewX) -> ThrowDir is 1; 
            (NewY < SorcY, SorcX = NewX) -> ThrowDir is 2
        ),
        throw_rock(Board, Turn, Pos, t-Player, Direction, ThrowDir, NewBoard),
        NewMove is 3,
        format('I moved my stonetroll to (~d, ~d) and I threw the rock', [NewX, NewY])
    ;
    (opposite_direction(Direction, NewD), new_pos(Pos, NewD, A-B), position(r-_, A-B) ->
        pull_rock(Board, Turn, Pos, t-Player, Direction, NewBoard), NewMove is Move,
        format('I moved my stonetroll to (~d, ~d) and I pulled the rock', [NewX, NewY]); 
        general_move(Board, t-Player, Pos, NewX-NewY, NewBoard), NewMove is Move,
        format('I moved my stonetroll to (~d, ~d)', [NewX, NewY])
    )).
