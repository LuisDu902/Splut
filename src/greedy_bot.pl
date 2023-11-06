
% greedy_move(Piece-Dir, NrMove, Turn)
:- dynamic greedy_move/3.

first_moves :-
    asserta(greedy_move(d-1, 1, 1)),
    asserta(greedy_move(d-2, 1, 2)),
    asserta(greedy_move(t-4, 2, 2)).

% checked
most_threatening_rock(Player, Rock) :-
    next_player(Player, Opponent),
    distance(t-Opponent, r-1, D1),
    distance(t-Opponent, r-2, D2),
    distance(t-Opponent, r-3, D3),
    distance(t-Opponent, r-4, D4),
    min4(D1-1, D2-2, D3-3, D4-4, Rock).

% -----------------------------------------

% checked
same_axis(Piece1, Piece2, x) :- 
    position(Piece1, X1-Y1),
    position(Piece2, X2-Y2),
    (X1 =:= X2), !.

same_axis(Piece1, Piece2, y) :-
    position(Piece1, X1-Y1),
    position(Piece2, X2-Y2),
    (Y1 =:= Y2), !.

% -----------------------------------------

% checked
clear_shot(Board, Player, Rock, Axis) :-
    position(Rock, X-Y),
    length(Board, Size),
    (Axis == x -> get_col(X, Board, List), 
        get_remaining(Y, List, Size, Rest1, 1), 
        get_remaining(Y, List, Size, Rest2, 2);   
    nth1(Y, Board, List), 
        get_remaining(X, List, Size, Rest1, 3),
        get_remaining(X, List, Size, Rest2, 4)),
    new_rock_pos(Rest1, X-Y, Direction, NewPos1),
    new_rock_pos(Rest2, X-Y, Direction, NewPos2),
    (position(s-Player, NewPos1) ;
    position(s-Player, NewPos2)).


% -----------------------------------------

% checked
dangerous_rocks(Board, Player, Rock) :-
    Rocks = [r-1, r-2, r-3, r-4],
    setof(R, (member(R, Rocks), same_axis(s-Player, R, Axis), clear_shot(Board, Player, R, Axis)), Rock).

% -----------------------------------------

% checked
reachable_rocks(Board, Player, ReachableRocks) :-
    Rocks = [r-1, r-2, r-3, r-4],
    position(t-Player, Pos),
    troll_paths(Board, Pos, Paths),
    findall(Rock, (
        member(Rock, Rocks),
        position(Rock, RockPos),
        findall(Path, (member(Path, Paths), last(Path, Lastpos), Lastpos =:= RockPos), ChosenPaths),
        length(ChosenPaths, L), !, 
        L \= 0
    ), ReachableRocks).

% -----------------------------------------

% checked
can_attack(Board, Player, Turn) :-
    next_player(Player, Opponent),
    dangerous_rocks(Board, Opponent, Rocks),
    reachable_rocks(Board, Player, ReachableRocks),
    intersect(Rocks, ReachableRocks, ThrowableRocks),
    length(ThrowableRocks, Tlen), !,
    Tlen \= 0 .

attack([Board, Player, NrMove, Turn], Move) :-
    next_player(Player, Opponent),
    dangerous_rocks(Board, Opponent, Rocks),
    reachable_rocks(Board, Player, ReachableRocks),
    intersect(Rocks, ReachableRocks, ThrowableRocks),
    random_member(TheRock, ThrowableRocks),
    position(TheRock, TheRockPos),
    position(t-Player, Pos),
    troll_paths(Board, Pos, Paths),
    findall(Path, (member(Path, Paths), last(Path, LastPos), LastPos = TheRockPos), ChosenPaths),
    random_member(ThePath, ChosenPaths),
    greedy_piece(t, ThePath, Turn),
    greedy_move(Move, 1, Turn).

% ----------------------------------------- 

need_protection(Board, Player) :-
    write('protection\n'),
    next_player(Player, Opponent),
    can_attack(Board, Opponent).

% -----------------------------------------

is_safe(X-Y, Player) :-
    next_player(Player, Opponent),
    AttackingPieces = [r-1, r-2, r-3, r-4, t-Opponent],
    findall(Piece, (member(Piece, AttackingPieces),
        position(Piece, PX-PY),
        (X =:= PX ; Y =:= PY)), Pieces),
    length(Pieces, L),
    L = 0.

% -----------------------------------------

protect_sorcerer([Board, Player, 1, Turn], Move) :-
    position(s-Player, Pos),
    possible_paths(Board, Pos, Paths),
    findall(Path, (member(Path, Paths), last(Path, NewPos), is_safe(NewPos, Player)), SafePaths),
    random_member(ChosenPath, SafePaths),
    greedy_piece(s, ChosenPath, Turn),
    greedy_move(Move, 1, Turn).


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

troll_paths(Board, Pos, Positions):-
    length(Board, Size),
    troll_reach(Size, [[Pos]], A), append(A, First),
    troll_reach(Size, First, B), append(B, Second),
    troll_reach(Size, Second, C), append(C, Third),
    append([First, Second , Third], Posi),
    remove_dups(Posi, Positions).

possible_paths(Board, Pos, Positions):-
    length(Board, Size),
    reach_pos(Size, [[Pos]], F), append(F, First),
    reach_pos(Size, First, Second), append(Second, NonReachable),
    reach_pos(Size, NonReachable, Final), append(Final, Posi),
    remove_dups(Posi, Positions).

% checked
greedy_piece(_, [], _).
greedy_piece(Piece, [H|T], Turn) :-
    greedy_piece_helper(Piece, [H|T], 1, Turn).

greedy_piece_helper(_, [], _, _).
greedy_piece_helper(_, List, _, _):-
    length(List, Size),
    Size = 1, !.

greedy_piece_helper(Piece, [A|Rest], Index, Turn) :-
    NewIndex is Index + 1,
    nth1(1, Rest, NextElement),
    new_pos(A, D, NextElement),
    asserta(greedy_move(Piece-D, Index, Turn)),
    greedy_piece_helper(Piece, [Rest], NewIndex, Turn).


% greedy_piece(Piece, [A, B, C, D], Turn):-
%    new_pos(A, D1, B), asserta(greedy_move(Piece-D1, 1, Turn)),
%    new_pos(B, D2, C), asserta(greedy_move(Piece-D2, 2, Turn)),
%    new_pos(C, D3, D), asserta(greedy_move(Piece-D3, 3, Turn)).

greedy_piece_list([Piece1, Piece2, Piece3], [A, B, C, D, E, F], Turn):-
    new_pos(A, D1, B), asserta(greedy_move(Piece1-D1, 1, Turn)),
    new_pos(C, D2, D), asserta(greedy_move(Piece2-D2, 2, Turn)),
    new_pos(E, D3, F), asserta(greedy_move(Piece3-D3, 3, Turn)).

greedy_sorcerer_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]):-
    position(s-Player, Pos),
    new_pos(Pos, Direction, NewX-NewY),
    general_move(Board, s-Player, Pos, NewX-NewY, NewBoard),
    format('I moved my sorcerer to (~d, ~d)', [NewX, NewY]).


can_protect([Board, Player, _, _]):-
    position(s-Player, Pos),
    possible_paths(Board, Pos, Paths),
    findall(Path, (member(Path, Paths), last(Path, NewPos), is_safe(NewPos, Player)), SafePaths),
    length(SafePaths, Size),
    Size > 0.


get_dist(Rock, Player, Dist) :-
    Pieces = [s, t, d],
    findall(D, (
        member(Piece, Pieces),
        position(Piece-Player, X-Y),
        position(Rock, RX-RY),
        D is abs(X - RX) + abs(Y - RY)
    ), Dists),
    min_member(Dist, Dists).

closest_rock(Player, Rock, Dist):-
    Rocks = [r-1, r-2, r-3, r-4],
    findall(D-Rock, (
        member(Rock, Rocks),
        get_dist(Rock, Player, D)
    ), Distances),
    min_member(Dist-Rock, Distances).


greedy_dwarf([Board, Player, 1, Turn], Move):-
    position(d-Player, Pos),
    possible_paths(Board, Pos, Paths),
    random_member(ChosenPath, Paths),
    greedy_piece(d, ChosenPath, Turn),
    greedy_move(Move, 1, Turn).

move_closer([Board, Player, 1, Turn], Move):-
    closest_rock(Player, Rock, Dist),
    
    (Dist == 1-> greedy_dwarf([Board, Player, 1, Turn], Move); true).



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
        NewMove is 3
    ;
        general_move(Board, t-Player, Pos, NewX-NewY, NewBoard), NewMove is Move
    ).