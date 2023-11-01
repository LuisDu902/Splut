% ----------------------------------------
% |         Stonetoll predicates         |
% ----------------------------------------

valid_troll_move(Pos, Direction):-
    new_pos(Pos, Direction, NewPos),
    ((position(r-_, NewPos); \+ position(_-_, NewPos)) -> true ; false).

% ----------------------------------------

pull_rock_option(Option):-
    write('Do you want to pull the rock right behind the Stonetroll?\n\n'),
    write('[1] Yes\n'),
    write('[2] No\n\n'),
    select_option(1, 2, Option).   

pull_rock(Board, Position, Troll, Direction, NewBoard) :-
    opposite_direction(Direction, NewD), new_pos(Position, NewD, A-B), position(Rock, A-B),
    new_pos(Position, Direction, NewPos),
    update_piece_pos(Troll, Position, NewPos),
    update_piece_pos(Rock, A-B, Position),
    swap_places(Board, Troll, Position, x-x, NewPos, Temp),
    swap_places(Temp, Rock, A-B, x-x, Position, NewBoard).

% ----------------------------------------

throw_rock_option(Direction):-
    write('Looks like you have found a rock! Which direction do you want to throw it?\n\n'),
    write('[1] Up\n'),
    write('[2] Down\n'),
    write('[3] Left\n'),
    write('[4] Right\n\n'),
    select_option(1, 4, Direction).   

throw_rock(Board, Pos, Troll, Direction, NewBoard) :-
    new_pos(Pos, Direction, NewPos),
    position(Rock, NewPos),
    update_piece_pos(Troll, Pos, NewPos),
    update_piece_pos(Rock, NewPos, Pos),
    swap_places(Board, Troll, Pos, Rock, NewPos, Tmp),
    move_rock(Tmp, Rock, 1, NewBoard).

% ----------------------------------------

troll_move(Board, Troll, Pos, Direction, NewBoard) :-
    new_pos(Pos, Direction, NewPos),
    (position(r-_, NewPos) ->
        throw_rock_option(Dir),
        throw_rock(Board, Pos, Troll, Direction, NewBoard)
    ;
    (opposite_direction(Direction, NewD), new_pos(Pos, NewD, A-B), position(r-_, A-B) ->
        pull_rock_option(Option),
        (Option == 1 ->
            pull_rock(Board, Pos, Troll, Direction, NewBoard)
            ;
           
            general_move(Board, Troll, Pos, NewPos, NewBoard)
        )
        ;
        general_move(Board, Troll, Pos, NewPos, NewBoard)
    )).

move_rock(NewBoard, Rock, Direction, NewBoard):-
    position(Rock, X-Y),
    length(NewBoard, Size),
    (   (Direction =:= 1; Direction =:= 2) ->
        get_col(X, NewBoard, List),
        get_remaining(Y, List, Size, Rest, Direction)
    ;   nth1(Y, NewBoard, List),
        R is Size - X, 
        get_remaining(Y, List, Size, Rest, Direction)
    ),
    new_rock_pos(Rest, X-Y, Direction, NewPos),
    write('MOVING ROCK...\n').


new_rock_pos([], Pos, Direction, Pos).

new_rock_pos(List, X-Y, Direction, NewX-NewY):-
   
    (   Direction =:= 2 ->
        find_elem(List, [r-_, t-_], A),
        NewX is X,
        NewY is Y + A
    ;   Direction =:= 4 ->
        find_elem(List, [r-_, t-_], A),
        NewX is X + A,
        NewY is Y
    ;   Direction =:= 1 -> 
        find_reverse_elem(List, [r-_, t-_], A),
        NewX is X,
        NewY is Y - A
    ;   Direction =:= 3 -> 
        find_reverse_elem(List, [r-_, t-_], A),
        NewX is X - A,
        NewY is Y
    ),
    format('THE ROCK SHOULD GO FROM (~d,~d) TO (~d,~d)\n', [X, Y, NewX, NewY])
    .


get_remaining(N, List, Size, Rest, Direction):-
    format('DIRECTION IS ~d\n', [Direction]),
    (   (Direction =:= 2; Direction =:= 4) ->
        Len is Size - N,
        format('It is starting in ~d with length ~d\n', [N, Len]),
        sublist(List, Rest, N, Len, _)
    ;   Len is N - 1,
        format('It is starting at 0 with length ~d\n', [Len]),
        sublist(List, Rest, 0, Len, _)
    ).


print_list([]).
print_list([Head|Tail]) :-
    write(Head),
    write(' '),
    print_list(Tail).
