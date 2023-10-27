game_cycle(GameState) :-
    write('GAME STATE, SET UP COMPLETE!\n').

play :- 
    menu(GameState), !,
    game_cycle(GameState).

