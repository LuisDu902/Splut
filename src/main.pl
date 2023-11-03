:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

:- consult(menu).
:- consult(utils).
:- consult(board).
:- consult(troll).
:- consult(dwarf).
:- consult(random_bot).
:- consult(greedy_bot).


% display_turn(+Player, +NrMoves)
% Displays a message indicating the player's turn and the number of moves made.
display_turn(Player, NrMoves):-
    player_name(Player, Name),
    format('\nYour turn to play, ~a! This is your move nr ~d \n\n', [Name, NrMoves]). 

% -----------------------------------------

% display_game(+GameState)
% Displays the current state of the game, including the player turn and the game board.
display_game([Board, Player, NrMoves, _]) :-
    display_turn(Player, NrMoves),
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

% move(+GameState, +Piece-Direction, -NewGameState)
% Performs a player move for the specified piece in the given direction and updates the game state.
move([Board, Player, Moves, Turns], Piece-Direction, [NewBoard, NewPlayer, NrMoves, NrTurns]) :-   
    \+computer_level(Player, _),    
    position(Piece-Player, X-Y),
    new_pos(X-Y, Direction, NewX-NewY),
    
    (
        Piece == t -> troll_move(Board, Piece-Player, X-Y, Direction, NewBoard) ;
        Piece == s -> general_move(Board, Piece-Player, X-Y, NewX-NewY, NewBoard) ;
        Piece == d -> dwarf_move(Board, Piece-Player, X-Y, Direction, NewBoard)
    ),

    (Turns = 1, NrTurns is 2, next_player(Player, NewPlayer), NrMoves is Moves; 
    Turns = 2, Moves = 2, NrTurns is 3, next_player(Player, NewPlayer), NrMoves is 1;   
    Moves < 3, NrMoves is Moves + 1, NewPlayer = Player, NrTurns is Turns;   
    NrTurns is Turns + 1, next_player(Player, NewPlayer), NrMoves is 1).

% -----------------------------------------

% move(+GameState, +Piece-Direction, -NewGameState)
% Performs a computer move for the specified piece in the given direction and updates the game state.
move([Board, Player, Moves, Turns], Piece-Direction, [NewBoard, NewPlayer, NrMoves, NrTurns]) :-   
    computer_level(Player, 1),    
    write('Computer moving...\n'),
    position(Piece-Player, X-Y),
    new_pos(X-Y, Direction, NewX-NewY),
    
    (
        Piece == t -> random_troll_move(Board, Piece-Player, X-Y, Direction, NewBoard) ;
        Piece == s -> general_move(Board, Piece-Player, X-Y, NewX-NewY, NewBoard) ;
        Piece == d -> general_move(Board, Piece-Player, X-Y, Direction, NewBoard)
    ),

    (Turns = 1, NrTurns is 2, next_player(Player, NewPlayer), NrMoves is Moves; 
    Turns = 2, Moves = 2, NrTurns is 3, next_player(Player, NewPlayer), NrMoves is 1;   
    Moves < 3, NrMoves is Moves + 1, NewPlayer = Player, NrTurns is Turns;   
    NrTurns is Turns + 1, next_player(Player, NewPlayer), NrMoves is 1).

% -----------------------------------------

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
    move(GameState, Move, NewGameState),
    game_cycle(NewGameState).

% -----------------------------------------

% play
% Initiates and controls the entire gameplay, including setup, game flow, and cleanup.
play :- 
    menu(GameState), !,
    game_cycle(GameState), !, 
    clear_data.
