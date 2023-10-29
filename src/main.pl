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

choose_position(Size, X-Y):-
    write('\nWhere do you want to move? \n'),
    skip_line,
    select_option(1, Size, X),
    select_option(1, Size, Y).

choose_move([Board, Player, _, _], _Move) :-
    length(Board, Size),
    choose_piece(Piece),
    position(Piece-Player, X-Y),
    format('Original position: (~d, ~d)\n', [X, Y]),
    choose_position(Size, Z-W),
    format('New position: (~d, ~d)\n', [Z, W]).

move(_GameState, _Move, _NewGameState) :-
    write('Moving pieces...\n').

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
