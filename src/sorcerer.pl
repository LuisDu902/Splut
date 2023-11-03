% ----------------------------------------
% |         Sorcerer predicates          |
% ----------------------------------------

% chosen_rock(Rock, Turn, Move)
:- dynamic chosen_rock/3.

sorcerer_move(Board, Move, Turn, Sorcerer, Pos, Direction, NewBoard) :-
    new_pos(Pos, Direction, NewPos),
    movable_rocks(Turn, Move, Board, Direction, Rocks),
    length(Rocks, Size),
    format('Movable rocks : ~d\n', [Size]),
       
    ( Size = 0 -> general_move(Board, Sorcerer, Pos, NewPos, NewBoard);   
    
    Size = 1 -> levitate_option(Option),
    ( Option = 1 ->
        (chosen_rock(_, Turn, Move) ->
            continuous_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, NewBoard);
            first_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, Rocks, NewBoard)
        );   
        general_move(Board, Sorcerer, Pos, NewPos, NewBoard)
    );   
    
    levitate_option(Option),
    ( Option = 1 ->
        first_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, Rocks, NewBoard);   
        general_move(Board, Sorcerer, Pos, NewPos, NewBoard)
    )).

  
continuous_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, NewBoard):-
    chosen_rock(Rock, Turn, Move),
    position(Rock, RockPos),
    new_pos(RockPos, Direction, NewRockPos),
    general_move(Board, Rock, RockPos, NewRockPos, Temp),
    general_move(Temp, Sorcerer, Pos, NewPos, NewBoard), 
    N is Move + 1,
    asserta(chosen_rock(Rock, Turn, N)).
            

first_levitation(Board, Move, Turn, Sorcerer, Pos, NewPos, Direction, Rocks, NewBoard):-
    choose_rock_option(Rocks, I),
    position(r-I, RockPos),
    new_pos(RockPos, Direction, NewRockPos),
    general_move(Board, r-I, RockPos, NewRockPos, Temp),
    general_move(Temp, Sorcerer, Pos, NewPos, NewBoard),  
    N is Move + 1,
    asserta(chosen_rock(r-I, Turn, N)).

levitate_option(Option):-
    write('\nDo you want to levitate a rock?\n\n'),
    write('[1] Yes\n'),
    write('[2] No\n\n'),
    select_option(1, 2, Option).   

movable_rocks(Turn, Move, Board, Direction, Rocks) :-
    length(Board, Size),
    (   (chosen_rock(_, Turn, Move)) ->
        write('THERE IS ALREADY A CHOSEN ROCK\n'),
        chosen_rock(Rock, Turn, Move),
        position(Rock, Pos),
        new_pos(Pos, Direction, NewPos),
        (   inside_board(NewPos, Size),
            \+ position(_, NewPos) ->
            Rocks = [Rock]
        ;   Rocks = []
        )
    ;   (chosen_rock(_, Turn, _)) ->
        
        write('NON CONTINUOUS MOVE\n'),
        Rocks = []
    ;   
    
        write('FIRST MOVE APPARENTLY\n'),
    findall(Rock,
            (member(Rock, [r-1, r-2, r-3, r-4]),
            position(Rock, Pos),
            new_pos(Pos, Direction, NewPos),
            inside_board(NewPos, Size),
            \+ position(_, NewPos)),
            Rocks)
    ).
 
    

choose_rock_option(Rocks, Rock):-
    write('\nWhich rock do you want to levitate?\n\n'),
    print_rocks(Rocks),
    write('\nRock : '),
    repeat,
    read_number_input(Option),
    (memberchk(r-Option, Rocks) -> true ; 
    write('Invalid input. Please enter a valid number: '), fail).

print_rocks([]).
print_rocks([r-I|Rest]) :-
    position(r-I, X-Y),
    format('[~d] Rock at position (~d, ~d)\n', [I, X, Y]),
    print_rocks(Rest).