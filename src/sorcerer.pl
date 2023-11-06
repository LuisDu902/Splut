% ----------------------------------------
% |         Sorcerer predicates          |
% ----------------------------------------

% moved_rocks(-Rock, +Turn)
:- dynamic moved_rocks/2.

% chosen_rock(-Rock, +Turn, +Move)
:- dynamic chosen_rock/3.

% sorcerer_move(+GameState, +Direction, -NewGameState)
% Moves the sorcerer
sorcerer_move([Board, Player, Move, Turn], Direction, [NewBoard, Player, Move, Turn]):-
    position(s-Player, Pos), new_pos(Pos, Direction, NewPos),
    movable_rocks(Turn, Move, Board, Direction, Rocks),
    length(Rocks, Size),   
    ( Size = 0 -> general_move(Board, s-Player, Pos, NewPos, NewBoard);   
    Size = 1 -> levitate_option(Option),
        ( Option = 1 ->
        (chosen_rock(_, Turn, Move) ->
            continuous_levitation(Board, Move, Turn, s-Player, Pos, NewPos, Direction, NewBoard);
            first_levitation(Board, Move, Turn, s-Player, Pos, NewPos, Direction, Rocks, NewBoard));   
        general_move(Board, s-Player, Pos, NewPos, NewBoard));  
    levitate_option(Option),
    (Option = 1 ->
        first_levitation(Board, Move, Turn, s-Player, Pos, NewPos, Direction, Rocks, NewBoard);   
        general_move(Board, s-Player, Pos, NewPos, NewBoard)
    )).

% -----------------------------------------

% continuous_levitation(+Board, +Move, +Turn, +Sorcerer, +Pos, -NewPos, +Direction, -NewBoard)
% Continues levitating a rock that has already been levitated in a previous turn.
continuous_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, NewBoard):-
    chosen_rock(Rock, Turn, Move),
    position(Rock, RockPos),
    new_pos(RockPos, Direction, NewRockPos),
    general_move(Board, Rock, RockPos, NewRockPos, Temp),
    general_move(Temp, Sorcerer, Pos, NewPos, NewBoard), 
    N is Move + 1,
    asserta(chosen_rock(Rock, Turn, N)).

% -----------------------------------------
      
% first_levitation(+Board, +Move, +Turn, +Sorcerer, +Pos, -NewPos, +Direction, +Rocks, -NewBoard)
% Levitates a rock if no previous rock has been levitated in the same turn
first_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, Rocks, NewBoard):-
    choose_rock_option(Rocks, I),
    position(r-I, RockPos),
    new_pos(RockPos, Direction, NewRockPos),
    general_move(Board, r-I, RockPos, NewRockPos, Temp),
    format('New rock- ~d pos: ~d\n', [I, NewRockPos]),
    general_move(Temp, Sorcerer, Pos, NewPos, NewBoard),  
    N is Move + 1,
    asserta(chosen_rock(r-I, Turn, N)), 
    asserta(moved_rocks(r-I, Turn)).

% -----------------------------------------

% levitate_option(-Option)
% Asks whether the player wants to levitate a rock or not
levitate_option(Option):-
    write('\nDo you want to levitate a rock?\n\n'),
    write('[1] Yes\n'),
    write('[2] No\n\n'),
    select_option(1, 2, Option).   

% -----------------------------------------

% movable_rocks(+Turn, +Move, +Board, +Direction, -Rocks)
% Gets the list of movable rocks
movable_rocks(Turn, Move, Board, Direction, Rocks) :-
    length(Board, Size),
    (   
        (chosen_rock(_, Turn, Move)) ->
            chosen_rock(Rock, Turn, Move),
            position(Rock, Pos),
            new_pos(Pos, Direction, NewPos),
            (inside_board(NewPos, Size), \+ position(_, NewPos) -> 
                Rocks = [Rock] ; 
                Rocks = [] 
            );   
        (chosen_rock(_, Turn, _)) -> 
            Rocks = [];   
        findall(Rock,
            (member(Rock, [r-1, r-2, r-3, r-4]),
            position(Rock, Pos),
            new_pos(Pos, Direction, NewPos),
            inside_board(NewPos, Size),
            \+ position(_, NewPos)),
            List1),
        last_turn_rocks(Turn, List2),
        subtract_lists(List1, List2, Rocks)
    ).
 
% -----------------------------------------
    
% choose_rock_option(+Rocks, -Option)
% Asks the player which of the rocks they want to levitate
choose_rock_option(Rocks, Option):-
    write('\nWhich rock do you want to levitate?\n\n'),
    print_rocks(Rocks),
    write('\nRock : '),
    repeat,
    read_number_input(Option),
    (memberchk(r-Option, Rocks) -> true ; 
    write('Invalid input. Please enter a valid number: '), fail).

% -----------------------------------------

% print_rocks(+Rocks)
% Prints the rocks that can be levitated
print_rocks([]).
print_rocks([r-I|Rest]) :-
    position(r-I, X-Y),
    format('[~d] Rock at position (~d, ~d)\n', [I, X, Y]),
    print_rocks(Rest).

% -----------------------------------------

% last_turn_rocks(+Turn,-Rocks)
% Gets the rocks that were moved in the last turn
last_turn_rocks(Turn, Rocks):-
    LastTurn is Turn-1,
    findall(Rock, moved_rocks(Rock, LastTurn), Rocks).

