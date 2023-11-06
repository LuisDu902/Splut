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
:- consult(greedy_bot).

% display_turn(+Player, +Move, +Turn)
% Displays a message indicating the players turn and the number of moves and turns made.
display_turn(Player, Move, Turn) :-
    \+computer_level(Player, _),
    player_name(Player, Name),
    format('\n\nYour turn to play, ~a! This is your move ~d of turn ~d \n\n', [Name, Move, Turn]), !. 

display_turn(Player, Move, Turn) :-
    player_name(Player, Name),
    format('\n~a move ~d of turn ~d: ', [Name, Move, Turn]), !. 

% -----------------------------------------

% display_game(+GameState)
% Displays the current state of the game, including the player turn and the game board.
display_game([Board, Player, Move, Turn]) :-
    display_turn(Player, Move, Turn),
    (\+computer_level(Player, _) -> display_board(Board); true).

% -----------------------------------------

% choose_piece(+Player, -Piece)
% Allows the player to choose a piece (stonetroll, dwarf, or sorcerer) to move.
choose_piece(Player, Piece) :-
    write('\nChoose the piece to move: \n'),
    write('[t] Stonetroll\n'),
    write('[d] Dwarf\n'),
    write('[s] Sorcerer\n\n'),
    write('Option: '),
    repeat,
    get_char(Piece),
    skip_line,
    (\+memberchk(Piece, [t, d, s]) -> write('\nInvalid input, please choose a valid piece : '), fail;
    \+position(Piece-Player, _) -> write('\nThis piece is already dead! Choose a valid piece: '), fail ;
    true).

% -----------------------------------------

% choose_direction(-Direction)
% Allows the player to choose a direction to move to (1 for up, 2 for down, 3 for left, 4 for right).
choose_direction(Direction) :-
    write('\nChoose the direction to move to: \n'),
    write('[1] Up\n'),
    write('[2] Down\n'),
    write('[3] Left\n'),
    write('[4] Right\n\n'),
    select_option(1, 4, Direction).

% -----------------------------------------

% choose_move(+GameState, -Move)
% Allows the player to choose a move based on the current game state.
choose_move([Board, Player, _, _], Piece-Direction) :-
    \+computer_level(Player, _),      
    length(Board, Size),
    repeat,
    choose_piece(Player, Piece),
    position(Piece-Player, X-Y),
    choose_direction(Direction),
    (valid_move(Board, Piece-Player, X-Y, Size, Direction) -> true ; 
    format('Please input a valid move: ', []), fail).

% -----------------------------------------

% choose_move(+GameState, -Move)
% Allows the computer to choose a move based on the current game state.
choose_move([Board, Player, NrMove, Turn], Move) :-
    computer_level(Player, Level),                 
    choose_move([Board, Player, NrMove, Turn], Level, Move), !.   

% -----------------------------------------

% choose_move(+Board, +Player, +Level, -Move)
% Chooses a random valid move from the available options on the game board.
choose_move(GameState, 1, Move) :-
    valid_moves(GameState, ListOfMoves),
    random_member(Move, ListOfMoves).

choose_move([Board, Player, NrMove, Turn], 2, Move) :-
    (\+greedy_move(_, NrMove, Turn) ->
        value([Board, Player, NrMove, Turn], Value),
        (
            Value == 1 -> attack([Board, Player, NrMove, Turn], Move);
            Value == -1 -> 
                (can_protect([Board, Player, NrMove, Turn]) ->
                    protect_sorcerer([Board, Player, 1, Turn], Move);
                    move_closer([Board, Player, 1, Turn], Move)
                );
            move_closer([Board, Player, 1, Turn], Move)
        )
    ;
    greedy_move(Move, NrMove, Turn)
    ).

% -----------------------------------------

% value(+GameState, -Value) 
% Evaluates the current game state 
value([Board, Player, _, _], 1) :- can_attack(Board, Player), !.
value([Board, Player, _, _], -1) :- need_protection(Board, Player), !.
value(_, 0) :- !.

% -----------------------------------------

% general_valid_move(+NewPos)
% Checks if the specified position (NewPos) is an empty position on the game board.
general_valid_move(NewPos) :-
    \+ position(_-_, NewPos).

% -----------------------------------------

% valid_moves(+GameState, -ListOfMoves)
% Calculates all the valid moves
valid_moves([Board, Player, _, _], ListOfMoves) :-
    length(Board, Size),
    Pieces = [t, d, s],
    Directions = [1, 2, 3, 4],
    findall(Piece-Direction, (
        member(Piece, Pieces),
        member(Direction, Directions),
        position(Piece-Player, Pos),
        valid_move(Board, Piece-Player, Pos, Size, Direction)
    ), ListOfMoves).

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
general_move(Board, Piece, Pos, NewPos, NewBoard) :-
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
% Performs a random computer move for the specified piece in the given direction and updates the game state.
move([Board, Player, NrMove, Turn], Piece-Direction, NewGameState) :-   
    computer_level(Player, 1),
    (
        Piece == t -> random_troll_move([Board, Player, NrMove, Turn], Direction, NewGameState) ;
        Piece == s -> random_sorcerer_move([Board, Player, NrMove, Turn], Direction, NewGameState) ;
        Piece == d -> dwarf_move([Board, Player, NrMove, Turn], Direction, NewGameState),  
                      position(d-Player, NewX-NewY),
                      format('I moved my dwarf to (~d, ~d)', [NewX, NewY])   
    ).

% -----------------------------------------

% move(+GameState, +Piece-Direction, -NewGameState)
% Performs a greedy computer move for the specified piece in the given direction and updates the game state.
move([Board, Player, NrMove, Turn], Piece-Direction, NewGameState) :-   
    computer_level(Player, 2),
    (
        Piece == t -> greedy_troll_move([Board, Player, NrMove, Turn], Direction, NewGameState) ;
        Piece == s -> greedy_sorcerer_move([Board, Player, NrMove, Turn], Direction, NewGameState);
        Piece == d -> dwarf_move([Board, Player, NrMove, Turn], Direction, NewGameState),  
                      position(d-Player, NewX-NewY),
                      format('I moved my dwarf to (~d, ~d)', [NewX, NewY])   
    ).
    
% -----------------------------------------

% next_turn(+GameState, -NewGameState) 
% Calculates which player will play next, updating the number of moves and turns
next_turn([Board, Player, NrMove, Turns], [Board, NewPlayer, NewMoves, NewTurns]) :-
    (Turns = 1, NewTurns is 2, next_player(Player, NewPlayer), NewMoves is 1, nl; 
    Turns = 2, NrMove \= 1, NewTurns is 3, next_player(Player, NewPlayer), NewMoves is 1, nl;   
    NrMove < 3, NewMoves is NrMove + 1, NewPlayer = Player, NewTurns is Turns;   
    NrMove = 3, NewTurns is Turns + 1, next_player(Player, NewPlayer), NewMoves is 1, nl).

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
congratulate(Winner) :-
    player_name(Winner, Name),
    write(' __  __     ______     __  __        __     __     ______     __   __    \n'), 
    write('/\\ \\_\\ \\   /\\  __ \\   /\\ \\/\\ \\      /\\ \\  _ \\ \\   /\\  __ \\   /\\ "-.\\ \\   \n'),
    write('\\ \\____ \\  \\ \\ \\/\\ \\  \\ \\ \\_\\ \\     \\ \\ \\/ ".\\ \\  \\ \\ \\/\\ \\  \\ \\ \\-.  \\  \n'),
    write(' \\/\\_____\\  \\ \\_____\\  \\ \\_____\\     \\ \\__/".~\\_\\  \\ \\_____\\  \\ \\_\\\\"\\_\\ \n'),
    write('  \\/_____/   \\/_____/   \\/_____/      \\/_/   \\/_/   \\/_____/   \\/_/ \\/_/ \n'),
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
    clear_data,
    menu(GameState), !,
    clear_console,
    game_cycle(GameState), !.
