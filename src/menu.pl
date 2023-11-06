% -----------------------------------------
% |           Menu predicates            |
% -----------------------------------------

% player_name(+Player, -Name)
% Finds the name of a player
:- dynamic player_name/2.

% computer_level(+Computer, -Level)
% Finds the level of the computer
:- dynamic computer_level/2.

% -----------------------------------------

% welcome_message
% Displays a welcome message for the Splut! game.
welcome_message :-
    write('\n\n ______     ______   __         __  __     ______ \n'), 
    write('/\\  ___\\   /\\  == \\ /\\ \\       /\\ \\/\\ \\   /\\__  _\\\n'),
    write('\\ \\___  \\  \\ \\  __/ \\ \\ \\____  \\ \\ \\_\\ \\  \\/_/\\ \\/\n'), 
    write(' \\/\\_____\\  \\ \\_\\    \\ \\_____\\  \\ \\_____\\    \\ \\\\\n'), 
    write('  \\/_____/   \\/_/     \\/_____/   \\/_____/     \\/_/\n\n'),
    write('\n+-----------------------------+\n'),
    write('|      Welcome to Splut!      |\n'),
    write('+-----------------------------+\n\n').
                                                   
% -----------------------------------------

% mode_menu
% Displays a menu for choosing the game mode in Splut!.
mode_menu :-
    write('Choose the game mode:\n'),
    write('[1] Human/Human\n'),
    write('[2] Human/Computer\n'),
    write('[3] Computer/Computer\n\n').

% -----------------------------------------

% game_mode(+Mode)
% Initializes the game based on the selected game mode.
game_mode(1) :-
    write('\nPlaying Human vs Human\n'),
    choose_name(p1),
    choose_name(p2).
game_mode(2) :-
    write('\nPlaying Human vs Computer\n'),
    choose_name(p1),
    asserta((player_name(p2, 'Computer'))),
    choose_level(p2),
    (computer_level(_, 2) -> first_moves; true).
game_mode(3) :-
    write('\nPlaying Bot vs Bot\n'),
    asserta((player_name(p1, 'Computer1'))),
    asserta((player_name(p2, 'Computer2'))),
    choose_level(p1),
    choose_level(p2),
    (computer_level(_, 2) -> first_moves; true).

% -----------------------------------------

% choose_mode
% Allows the user to choose the game mode by displaying the mode menu and processing the selected option.
choose_mode :-
    mode_menu,
    select_option(1, 3, Option),
    game_mode(Option).

% -----------------------------------------

% choose_name(+Player)
% Prompts the specified player to enter their name and stores it in the knowledge base.
choose_name(Player):-
    format('[~a] Please enter your name : ', [Player]),
    read_string_input(Name, []),
    asserta(player_name(Player, Name)).

% -----------------------------------------

% choose_level(+Computer)
% Prompts the specified computer player to choose a level of difficulty and stores it in the knowledge base.
choose_level(Computer) :-
    format('\nChoose the level of difficulty for ~a:\n', Computer),
    write('[1] Random valid move\n'),
    write('[2] Best move at the time (greedy)\n\n'),
    select_option(1, 2, Level),
    asserta((computer_level(Computer, Level))).

% -----------------------------------------

% choose_board_size(-Size)
% Prompts the user to choose the size of the game board and reads the selected size.
choose_board_size(Size):-
    write('\nChoose the board size (odd number greater than 7): '),
    select_board(Size).

% -----------------------------------------

% initial_state(+Size, -State)
% Generates the initial state of the game given the board size.
initial_state(Size, [Board, p1, 1, 1]):-
    create_board(Size, Board).

% -----------------------------------------

% menu(-GameState)
% Displays the game menu, allowing the player to select the game mode and board size,
% and initializes the game state based on the chosen options.
menu(GameState) :-
    welcome_message,
    choose_mode,
    choose_board_size(Size),
    initial_state(Size, GameState).
