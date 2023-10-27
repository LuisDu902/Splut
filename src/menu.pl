:- dynamic player_name/2.

welcome_message :-
    write('\n+-----------------------+\n'),
    write('|      Welcome to       |\n'),
    write('|         Splut!        |\n'),
    write('+-----------------------+\n\n').

% -----------------------------------------

mode_menu :-
    write('Choose the game mode:\n'),
    write('[1] Human/Human\n'),
    write('[2] Human/Computer\n'),
    write('[3] Computer/Computer\n\n').

% -----------------------------------------

game_mode(1) :-
    write('\nPlaying Human vs Human\n'),
    choose_name(player1),
    choose_name(player2).

game_mode(2) :-
    write('\nPlaying Human vs Computer\n'),
    choose_name(player1),
    asserta((player_name(player2, 'Computer'))).

game_mode(3) :-
    write('\nPlaying Bot vs Bot\n'),
    asserta((player_name(player1, 'Computer1'))),
    asserta((player_name(player2, 'Computer2'))).

% -----------------------------------------

choose_mode :-
    mode_menu,
    select_option(1, 3, Option),
    game_mode(Option).

% -----------------------------------------

choose_name(Player):-
    format('[~a] Please enter your name : ', [Player]),
    read_string_input(Name, []),
    asserta(player_name(Player, Name)).

% -----------------------------------------

select_board_size :-
    write('\nChoose the board size:\n'),
    write('[1] 9 x 9\n'),
    write('[2] 11 x 11\n'),
    write('[3] 13 x 13\n\n').

% -----------------------------------------

choose_board(Board) :-
    select_board_size,
    select_option(1, 3, Option),
    create_board(Option, Board).

% -----------------------------------------

menu([Board, Player, [], 0]) :-
    welcome_message,
    choose_mode,
    choose_board(Board).




