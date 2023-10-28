game_over(GameState, Winner):-
    write('The game is over\n').

congratulate(Winner):-
    write('You won the game\n').

% -----------------------------------------

display_game([Board, _, _, _]) :-
    write('Displaying board:\n'),
    display_board(Board).

choose_move(GameState, Move) :-
    write('Choosing move\n').

move(GameState, Move, NewGameState) :-
    write('Moving pieces...\n').

next_player(Player, NextPlayer) :-
    write('Passing turn\n').

% -----------------------------------------

/*
game_cycle(GameState) :-
    game_over(GameState, Winner), !, 
    congratulate(Winner).
*/

game_cycle(GameState) :-
    display_game(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState).
    % next_player(Player, NextPlayer), !,
    % game_cycle(NewGameState).

% -----------------------------------------

play :- 
    menu(GameState), !,
    game_cycle(GameState).

