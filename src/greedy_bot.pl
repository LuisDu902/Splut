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

can_step_aside(Board, X-Y, Player) :-
    length(Board, Size),
    inside_board(X-Y, Size),
    \+position(_, X-Y),
    next_player(Player, Opponent),
    AttackingPieces = [r-1, r-2, r-3, r-4, t-Opponent],
    findall(Piece, (member(Piece, AttackingPieces),
        position(Piece, PX-PY),
        (X =:= PX ; Y =:= PY)), Pieces),
    length(Pieces, L),
    L = 0.

% -----------------------------------------

protect_sorcerer(Board, Player, s-Dir) :-
    position(s-Player, Pos),
    reachable_pos(Board, Pos, Positions),
    print_list(Positions).



reach_pos([], []).

reach_pos([Pos|R], [PossiblePos | Rest]) :-
    Directions = [1, 2, 3, 4],
    findall(NewPos, 
        (member(Direction, Directions), 
        new_pos(Pos, Direction, NewPos),
        inside_board(NewPos, 9)), 
        PossiblePos),
    reach_pos(R, Rest).
    

reachable_pos(Board, Pos, Positions):-
    length(Board, Size),
    reach_pos([Pos], F),
    append(F, First),
    reach_pos(First, Second),
    append(Second, NonReachable),
    reach_pos(NonReachable, Final),
    append(Final, Posi),
    remove_dups(Posi, Positions).