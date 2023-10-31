:- use_module(library(between)).

% -----------------------------------------

read_number_input(Number):-
    read_number_input_helper(Number, 0).

read_number_input_helper(Number, CurNumber) :-
    get_code(Input),
    between(48, 57, Input), !, 
    UpdatedNumber is 10*CurNumber + (Input - 48),
    read_number_input_helper(Number, UpdatedNumber).

read_number_input_helper(Number, Number).

% -----------------------------------------

read_string_input(String, CurString):-
    get_char(Char),
    Char \= '\n',
    append(CurString, [Char], UpdatedString),
    read_string_input(String, UpdatedString).

read_string_input(String, CurString) :-
    atom_chars(String, CurString).

% -----------------------------------------

select_option(Min, Max, Option) :-
    write('Option : '),
    repeat,
    read_number_input(Option),
    (between(Min, Max, Option) -> true ; 
    format('Invalid input. Please enter a number between ~d and ~d: ', [Min, Max]), fail).

select_board(Size) :-
    repeat,
    read_number_input(Size),
    ((Size > 7 , Size mod 2 =:= 1) -> true ; 
    format('Invalid input. Please enter an odd number greater than 7: ', []), fail).

% -----------------------------------------

inside_line(Line, Pos, Size) :-
    Starting is abs((Size + 1) // 2 - Line) + 1, 
    Ending is Size - Starting + 1,
    between(Starting, Ending, Pos).

inside_board(X-Y, Size) :-
    inside_line(X, Y, Size),
    inside_line(Y, X, Size).

% -----------------------------------------

new_pos(X-Y, 1, X-NewY) :- NewY is Y - 1.
new_pos(X-Y, 2, X-NewY) :- NewY is Y + 1.
new_pos(X-Y, 3, NewX-Y) :- NewX is X - 1.
new_pos(X-Y, 4, NewX-Y) :- NewX is X + 1.

% -----------------------------------------

replace(Pos, Element, List, NewList) :-
    nth1(Pos, List, _, Rest),
    nth1(Pos, NewList, Element, Rest).


update_piece_pos(Piece, OldPos, NewPos) :-
    retract(position(Piece, OldPos)),
    asserta(position(Piece, NewPos)).

% -----------------------------------------

next_player(p1, p2).
next_player(p2, p1).

opposite_direction(1, 2).
opposite_direction(2, 1).
opposite_direction(3, 4).
opposite_direction(4, 3).
