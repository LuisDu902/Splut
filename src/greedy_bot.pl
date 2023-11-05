
% greedy_move(Piece-Dir, NrMove, Turn)
:- dynamic greedy_move/3.


most_threatening_rock(Player, Rock) :-
    next_player(Player, Opponent),
    D1 is distance(t-Opponent, r-1),
    D2 is distance(t-Opponent, r-2),
    D3 is distance(t-Opponent, r-3),
    D4 is distance(t-Opponent, r-4),
    min4(D1-1, D2-2, D3-3, D4-4, Rock).

% -----------------------------------------

same_axis(Piece1, Piece2) :-
    position(Piece1, X1-Y1),
    position(Piece2, X2-Y2),
    ((X1 =:= X2) ; (Y1 =:= Y2)).

% -----------------------------------------

danger_sorcerer(Player) :-
    same_axis(s-Player, r-1),
    same_axis(s-Player, r-2),
    same_axis(s-Player, r-3),
    same_axis(s-Player, r-4).

% -----------------------------------------


need_protection(Board, Player) :-
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

protect_sorcerer([Board, Player, NrMove, Turn], Move) :-
    (\+greedy_move(Move, NrMove, Turn) ->
        position(s-Player, Pos),
        possible_paths(Board, Pos, Paths),
        findall(Path, (member(Path, Paths), last(Path, NewPos), is_safe(NewPos, Player)), SafePaths),
        random_member(ChosenPath, SafePaths),
        greedy_sorcerer(ChosenPath, Turn),
        greedy_move(Move, 1, Turn);
    greedy_move(Move, NrMove, Turn)).



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
    

possible_paths(Board, Pos, Positions):-
    length(Board, Size),
    reach_pos(Size, [[Pos]], F),
    append(F, First),
    reach_pos(Size, First, Second),
    append(Second, NonReachable),
    reach_pos(Size, NonReachable, Final),
    append(Final, Posi),
    remove_dups(Posi, Positions).

greedy_sorcerer([A, B, C, D], Turn):-
    new_pos(A, D1, B), asserta(greedy_move(s-D1, 1, Turn)),
    new_pos(B, D2, C), asserta(greedy_move(s-D2, 2, Turn)),
    new_pos(C, D3, D), asserta(greedy_move(s-D3, 3, Turn)).

greedy_sorcerer_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]):-
    position(s-Player, Pos),
    new_pos(Pos, Direction, NewPos),
    general_move(Board, s-Player, Pos, NewPos, NewBoard).
