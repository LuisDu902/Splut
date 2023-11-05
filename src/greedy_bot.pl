% -----------------------------------------
% |     Computer Level 2 predicates       |
% -----------------------------------------

most_threatening_rock(Player, Rock) :-
    distance(t-Opponent, r-1, Dist1),
    distance(t-Opponent, r-2, Dist2),
    distance(t-Opponent, r-3, Dist3),
    distance(t-Opponent, r-4, Dist4),
    min4(Dist1-1, Dist2-2, Dist3-3, Dist4-4, Rock).

% -----------------------------------------

same_axis(Piece1, Piece2) :-
    position(Piece1, X1-Y1),
    position(Piece2, X2-Y2),
    ((X1 =:= X2) ; (Y1 =:= Y2)).

% -----------------------------------------

danger_sorcerer(Player, Rock) :-
    (same_axis(s-Player, r-1) -> Rock is r-1),
    (same_axis(s-Player, r-2) -> Rock is r-2),
    (same_axis(s-Player, r-3) -> Rock is r-3),
    (same_axis(s-Player, r-4) -> Rock is r-4).

% -----------------------------------------

choose_greedy(Player, MovesLeft, Move) :-
    danger_sorcerer(Player, Rock),
    (distance(t-Player, Rock) =< MovesLeft ->
        position(t-Player, X1-Y1),
        position(Rock, X2-Y2),
        ((abs(X2-X1) > abs(Y2-Y1)) -> ((X2-X1) > 0 -> Move is 4 ; Move is 3) ; ((Y2-Y1) > 0 -> Move is 2 ; Move is 1)),
         ;
    ),
