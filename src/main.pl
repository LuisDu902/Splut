:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

:- consult(menu).
:- consult(utils).
:- consult(board).

% -----------------------------------------

game_over(_GameState, _Winner):-
    write('The game is over\n').

congratulate(_Winner):-
    write('You won the game\n').

% -----------------------------------------

display_game([Board, _, _, _]) :-
    display_board(Board).

% -----------------------------------------

choose_random_move(Board, Player, 1, Move) :-
    length(Board, Size),
    repeat,
    choose_random_piece(Piece),
    position(Piece-Player, X-Y),
    choose_random_direction(D),
    (valid_move(Piece-Player, X-Y, Size, D) -> format('I chose to move ~a-~a from (~d,~d) -> ~d\n', [Piece, Player, X, Y, D]) ; fail).

% -----------------------------------------

choose_random_piece(Piece) :-
    List = [t, d, s], 
    random_member(Piece, List).

% -----------------------------------------

choose_random_direction(D) :-
    random(1, 4, D).

% -----------------------------------------

choose_piece(Piece):-
    write('\nChoose the piece to move: \n'),
    write('[t] Stonetroll\n'),
    write('[d] Dwarf\n'),
    write('[s] Sorcerer\n\n'),
    write('Option: '),
    get_char(Piece).

% -----------------------------------------

choose_direction(D):-
    write('\nChoose the direction to move to: \n'),
    write('[1] Up\n'),
    write('[2] Down\n'),
    write('[3] Left\n'),
    write('[4] Right\n\n'),
    skip_line,
    select_option(1, 4, D).

% -----------------------------------------

choose_move([Board, Player, _, _], Piece-Direction) :-
    \+computer_level(Player, _),      
    length(Board, Size),
    repeat,
    choose_piece(Piece),
    position(Piece-Player, X-Y),
    format('Original position: (~d, ~d)\n', [X, Y]),
    choose_direction(Direction),
   
    (valid_move(Piece-Player, X-Y, Size, Direction) -> true ; 
    format('Please input a valid move: ', []), fail).

choose_move([Board,Player,_,_], Move):-
    computer_level(Player, Level),                 
    choose_random_move(Board, Player, Level, Move), !.   

% -----------------------------------------

general_valid_move(NewX-NewY) :-
    \+ position(_-_, NewX-NewY).

valid_move(Piece-Player, X-Y, Size, Direction) :-
    new_pos(X-Y, Direction, NewX-NewY),
    inside_board(NewX-NewY, Size),
    general_valid_move(NewX-NewY).

% -----------------------------------------

general_move(Board, Piece, Pos, NewPos, NewBoard):-
    update_piece_pos(Piece, Pos, NewPos),
    swap_places(Board, Piece, Pos, x-x, NewPos, NewBoard).

% -----------------------------------------

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

% -----------------------------------------

troll_move(Board, Piece-Player, X-Y, Direction, NewBoard):-
    new_pos(X-Y, Direction, NewX-NewY),
    (opposite_direction(Direction, NewD), new_pos(X-Y, NewD, A-B), position(r-_, A-B) ->
        pull_rock_option(Option),
        ( Option == 1 ->
            pull_rock(Board, X-Y, Piece-Player, Direction, NewBoard)
            ;
            general_move(Board, Piece-Player, X-Y, NewX-NewY, NewBoard)
        )
    ;
    general_move(Board, Piece-Player, X-Y, NewX-NewY, NewBoard)
).


move([Board, Player, Moves, Turns], Piece-Direction, [NewBoard, NewPlayer, NrMoves, NrTurns]) :-   
    position(Piece-Player, X-Y),
    new_pos(X-Y, Direction, NewX-NewY),
    
    (
        Piece == t -> troll_move(Board, Piece-Player, X-Y, Direction, NewBoard) ;
        Piece == s -> general_move(Board, Piece-Player, X-Y, NewX-NewY, NewBoard) ;
        Piece == d -> general_move(Board, Piece-Player, X-Y, NewX-NewY, NewBoard)
    ),

    (Turns = 1, NrTurns is 2, next_player(Player, NewPlayer), NrMoves is Moves; 
    Turns = 2, Moves = 2, NrTurns is 3, next_player(Player, NewPlayer), NrMoves is 1;   
    Moves < 3, NrMoves is Moves + 1, NewPlayer = Player, NrTurns is Turns;   
    NrTurns is Turns + 1, next_player(Player, NewPlayer), NrMoves is 1).

% -----------------------------------------


display_turn([_, Player, NrMoves, _]):-
    player_name(Player, Name),
    format('\nYour turn to play, ~a! This is your move nr ~d \n\n', [Name, NrMoves]). 

/*
game_cycle(GameState) :-
    game_over(GameState, Winner), !, 
    congratulate(Winner).
*/

game_cycle(GameState) :-
    display_turn(GameState),
    display_game(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState),
    game_cycle(NewGameState).

% -----------------------------------------

play :- 
    menu(GameState), !,
    game_cycle(GameState).
