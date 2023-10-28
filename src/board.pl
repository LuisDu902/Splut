
create_board(1,  [
    [ - , - , - , - , r , - , - , - , - ], 
    [ - , - , - , t1 , d1 , s1 , - , - , - ],
    [ - , - , x , x , x , x , x , - , - ], 
    [ - , x , x , x , x , x , x , x , - ],
    [ r , x , x , x , x , x , x , x , r ], 
    [ - , x , x , x , x , x , x , x , - ],
    [ - , - , x , x , x , x , x , - , - ], 
    [ - , - , - , s2 , d2 , t2 , - , - , - ],
    [ - , - , - , - , r , - , - , - , - ]
]).

create_board(2,  [
    [ - , - , - , - , - , r , - , - , - , - , - ], 
    [ - , - , - , - , t1 , d1 , s1 , - , - , - , - ],
    [ - , - , - , x , x , x , x , x , - , - , - ], 
    [ - , - , x , x , x , x , x , x , x , - , - ],
    [ - , x , x , x , x , x , x , x , x , x , - ],
    [ r , x , x , x , x , x , x , x , x , x , r ], 
    [ - , x , x , x , x , x , x , x , x , x , - ],
    [ - , - , x , x , x , x , x , x , x , - , - ],
    [ - , - , - , x , x , x , x , x , - , - , - ], 
    [ - , - , - , - , s2 , d2 , t2 , - , - , - , - ],
    [ - , - , - , - , - , r , - , - , - , - , - ]
]).

create_board(3,  [
    [ - , - , - , - , - , - , r , - , - , - , - , - , - ], 
    [ - , - , - , - , - , t1 , d1 , s1 , - , - , - , - , - ], 
    [ - , - , - , - , x , x , x , x , x , - , - , - , - ], 
    [ - , - , - , x , x , x , x , x , x , x , - , - , - ], 
    [ - , - , x , x , x , x , x , x , x , x , x , - , - ], 
    [ - , x , x , x , x , x , x , x , x , x , x , x , - ], 
    [ r , x , x , x , x , x , x , x , x , x , x , x , r ], 
    [ - , x , x , x , x , x , x , x , x , x , x , x , - ], 
    [ - , - , x , x , x , x , x , x , x , x , x , - , - ], 
    [ - , - , - , x , x , x , x , x , x , x , - , - , - ], 
    [ - , - , - , - , x , x , x , x , x , - , - , - , - ], 
    [ - , - , - , - , - , s2 , d2 , t2 , - , - , - , - , - ], 
    [ - , - , - , - , - , - , r , - , - , - , - , - , - ]
]).


display_board(Board) :-
    length(Board, Size),
    display_col_index(1, Size),
    display_rows(Board, Size, 1).

% -----------------------------------------

display_col_index(1, Size):-
    write('  1'),
    display_col_index(2, Size).

display_col_index(Size, Size) :-
    format('   ~d\n', [Size]).

display_col_index(N, Size) :-
    N < Size,
    format('   ~d', [N]),
    N1 is N + 1,
    display_col_index(N1, Size).

% -----------------------------------------
    
display_sep_line(0, _) :- write('|'), nl.

display_sep_line(Size, Index) :-
    Middle is (Size + 1) / 2, 
    Index =< Middle, 
    Dashes is 2 * Index - 1,
    Padding is (Size - Dashes) // 2,
    display_padding(Padding),
    display_dashes(Dashes, Dashes),
    nl, !.

display_sep_line(Size, Index) :-
    Middle is (Size + 1) // 2, 
    Dashes is Size - 2 * (Index - Middle - 1),
    Padding is (Size - Dashes) // 2,
    display_padding(Padding),
    display_dashes(Dashes, Dashes),
    nl, !.


% -----------------------------------------
    
display_padding(0).
display_padding(N) :-
    N > 0,
    write('    '),
    NewN is N - 1,
    display_padding(NewN).

% -----------------------------------------
    
display_dashes(0, _).
display_dashes(N, N) :- 
    write(' ---'),
    NewN is N - 1,
    display_dashes(NewN, N).
display_dashes(N, Dashes) :-
    N > 0,
    write('|---'),
    NewN is N - 1,
    display_dashes(NewN, Dashes).

% -----------------------------------------

display_rows([], Size, _) :- display_sep_line(Size, 1).
display_rows([Row|Rest], Size, Index) :-
    display_sep_line(Size, Index),
    display_row(Row),
    format(' ~d\n', [Index]),
    NewIndex is Index + 1,
    display_rows(Rest, Size, NewIndex).

% -----------------------------------------


handle_pipe('-', _).

handle_pipe(_, []):- write('|').

handle_pipe(_, [H | _]):-
    H = '-',
    write('|').

handle_pipe(_, [H | _]):-
    H \= '-'.


display_row([]). 

display_row([Element|Rest]) :-
    display_element(Element), 
    handle_pipe(Element, Rest),
    display_row(Rest).

% -----------------------------------------

display_element(-) :- write('    ').
display_element(x) :- write('|   ').
display_element(r) :- write('| r ').
display_element(t1) :- write('| t ').
display_element(d1) :- write('| d ').
display_element(s1) :- write('| s ').
display_element(t2) :- write('| T ').
display_element(d2) :- write('| D ').
display_element(s2) :- write('| S ').
