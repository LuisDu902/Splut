:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

:- consult(utils).
:- consult(menu).
:- consult(board).
:- consult(troll).
:- consult(dwarf).
:- consult(sorcerer).
:- consult(random_bot).

% display_turn(+Player, +NewMoves)
% Displays a message indicating the players turn and the number of moves made.
display_turn(Player, NewMoves):-
    player_name(Player, Name),
    format('\nYour turn to play, ~a! This is your move nr ~d \n\n', [Name, NewMoves]). 

% -----------------------------------------

% display_game(+GameState)
% Displays the current state of the game, including the player turn and the game board.
display_game([Board, Player, NewMoves, _]) :-
    display_turn(Player, NewMoves),
    display_board(Board).

% -----------------------------------------

% choose_piece(+Player, -Piece)
% Allows the player to choose a piece (stonetroll, dwarf, or sorcerer) to move.
choose_piece(Player, Piece):-
    repeat,
    write('\nChoose the piece to move: \n'),
    write('[t] Stonetroll\n'),
    write('[d] Dwarf\n'),
    write('[s] Sorcerer\n\n'),
    write('Option: '),
    get_char(Piece),
    skip_line,
    (\+position(Piece-Player, _) -> write('\nThis piece is already dead!\n'), fail ; true).

% -----------------------------------------

% choose_direction(-D)
% Allows the player to choose a direction to move to (1 for up, 2 for down, 3 for left, 4 for right).
choose_direction(D):-
    write('\nChoose the direction to move to: \n'),
    write('[1] Up\n'),
    write('[2] Down\n'),
    write('[3] Left\n'),
    write('[4] Right\n\n'),
    select_option(1, 4, D).

% -----------------------------------------

% choose_move(+GameState, -Move)
% Allows the player to choose a move based on the current game state.
choose_move([Board, Player, _, _], Piece-Direction) :-
    \+computer_level(Player, _),      
    length(Board, Size),
    repeat,
    choose_piece(Player, Piece),
    position(Piece-Player, X-Y),
    format('Original position: (~d, ~d)\n', [X, Y]),
    choose_direction(Direction),
   
    (valid_move(Board, Piece-Player, X-Y, Size, Direction) -> true ; 
    format('Please input a valid move: ', []), fail).

% -----------------------------------------

% choose_move(+GameState, -Move)
% Allows the computer to choose a move based on the current game state.
choose_move([Board,Player,_,_], Move):-
    computer_level(Player, Level),                 
    choose_random_move(Board, Player, Level, Move), !.   

% -----------------------------------------

% general_valid_move(+NewPos)
% Checks if the specified position (NewPos) is an empty position on the game board.
general_valid_move(NewPos) :-
    \+ position(_-_, NewPos).

% -----------------------------------------

% valid_move(+Board, +Piece-Player, +Pos, +Size, +Direction)
% Checks if the specified move is valid on the game board.
valid_move(Board, Piece-_, Pos, Size, Direction) :-
    new_pos(Pos, Direction, NewPos),
    inside_board(NewPos, Size),
    (
        Piece == t -> valid_troll_move(Pos, Direction);
        Piece == s -> general_valid_move(NewPos) ;
        Piece == d -> valid_dwarf_move(Board, Pos, Direction)
    ).

% -----------------------------------------

% general_move(+Board, +Piece, +Pos, +NewPos, -NewBoard)
% Performs a general move for a piece on the game board and updates the board configuration.
general_move(Board, Piece, Pos, NewPos, NewBoard):-
    update_piece_pos(Piece, NewPos),
    swap_places(Board, Piece, Pos, x-x, NewPos, NewBoard).

% -----------------------------------------

% move(+GameState, +Move, -NewGameState)
% Performs a player move for the specified piece in the given direction and updates the game state.
move([Board, Player, Move, Turn], Piece-Direction, NewGameState) :-   
    \+computer_level(Player, _),  
      (
        Piece == t -> troll_move([Board, Player, Move, Turn], Direction, NewGameState);
        Piece == s -> sorcerer_move([Board, Player, Move, Turn], Direction, NewGameState);
        Piece == d -> dwarf_move([Board, Player, Move, Turn], Direction, NewGameState)
    ), !.
    


% -----------------------------------------

% move(+GameState, +Piece-Direction, -NewGameState)
% Performs a computer move for the specified piece in the given direction and updates the game state.
move([Board, Player, Move, Turn], Piece-Direction, NewGameState) :-   
    computer_level(Player, 1),    
    write('Computer moving...\n'),
    (
        Piece == t -> random_troll_move([Board, Player, Move, Turn], Direction, NewGameState) ;
        Piece == s -> random_sorcerer_move([Board, Player, Move, Turn], Direction, NewGameState) ;
        Piece == d -> dwarf_move([Board, Player, Move, Turn], Direction, NewGameState)
    ), !.

% -----------------------------------------

next_turn([Board, Player, Moves, Turns], [Board, NewPlayer, NewMoves, NewTurns]) :-
    (Turns = 1, NewTurns is 2, next_player(Player, NewPlayer), NewMoves is Moves; 
    Turns = 2, Moves = 2, NewTurns is 3, next_player(Player, NewPlayer), NewMoves is 1;   
    Moves < 3, NewMoves is Moves + 1, NewPlayer = Player, NewTurns is Turns;   
    NewTurns is Turns + 1, next_player(Player, NewPlayer), NewMoves is 1).


% game_over(-Winner)
% Checks if the game is over and determines the winner.
game_over(Winner) :-
    (\+ position(s-p1, _)) -> Winner = p2 ;
    (\+ position(s-p2, _)) -> Winner = p1 ;
    false.

% -----------------------------------------

% congratulate(+Winner)
% Prints a congratulatory message for the winner of the game.
congratulate(Winner):-
    player_name(Winner, Name),
    format('\nCongrats ~a! You won the game.\n', [Name]).

% -----------------------------------------

% game_cycle(+GameState)
% Controls the game flow, allowing players to take turns and managing the game state transitions.
game_cycle(_) :-
    game_over(Winner), !, 
    congratulate(Winner), !.

game_cycle(GameState) :-
    display_game(GameState), !,
    choose_move(GameState, Move),
    move(GameState, Move, Temp),
    next_turn(Temp, NewGameState),
    game_cycle(NewGameState).

% -----------------------------------------

% play
% Initiates and controls the entire gameplay, including setup, game flow, and cleanup.
play :- 
    menu(GameState), !,
    game_cycle(GameState), !, 
    clear_data.
