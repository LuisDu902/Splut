:- use_module(library(lists)).
:- use_module(library(between)).

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

choose_piece(Piece):-
    write('\nChoose the piece to move: \n'),
    write('[t] Stonetroll\n'),
    write('[d] Dwarf\n'),
    write('[s] Sorcerer\n\n'),
    write('Option: '),
    get_char(Piece).

choose_direction(D):-
    write('\nChoose the direction to move to: \n'),
    write('[1] Up\n'),
    write('[2] Down\n'),
    write('[3] Left\n'),
    write('[4] Right\n\n'),
    skip_line,
    select_option(1, 4, D).

choose_move([Board, Player, _, _], _Move) :-
    length(Board, Size),
    repeat,
    choose_piece(Piece),
    position(Piece-Player, X-Y),
    format('Original position: (~d, ~d)\n', [X, Y]),
    choose_direction(D),
   
    (valid_move(Piece-Player, X-Y, Size, D) -> true ; 
    format('Please input a valid move: ', []), fail).

valid_move(Piece-Player, X-Y, Size, D) :-
    new_pos(X-Y, D, NewX-NewY),
    inside_board(NewX-NewY, Size), 
    \+ position(_-_, NewX-NewY).

move(_GameState, _Move, _NewGameState) :-
    write('Moving pieces...\n').

update_position(Piece-Player, X-Y) :-
    retract(position(Piece-Player, _-_)),
    asserta(position(Piece-Player, X-Y)).

% -----------------------------------------

next_player(_Player, _NextPlayer) :-
    write('Passing turn\n').

% -----------------------------------------

/*
game_cycle(GameState) :-
    game_over(GameState, Winner), !, 
    congratulate(Winner).
*/

game_cycle(GameState) :-
    display_game(GameState),
    choose_move(GameState, _Move).
    /*move(GameState, Move, NewGameState).
    % next_player(Player, NextPlayer), !,
    % game_cycle(NewGameState).*/

% -----------------------------------------

play :- 
    menu(GameState), !,
    game_cycle(GameState).
