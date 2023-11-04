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