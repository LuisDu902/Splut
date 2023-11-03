% -----------------------------------------
% |           Useful predicates           |
% -----------------------------------------

% read_number_input(-Number)
% Reads a number input from the user and unifies it with the variable Number.
read_number_input(Number):-
    read_number_input_helper(Number, 0).

% read_number_input_helper(-Number, +CurrentNumber)
% Helper predicate to read a number input from the user and accumulate it.
read_number_input_helper(Number, CurNumber) :-
    get_code(Input),
    between(48, 57, Input), !, 
    UpdatedNumber is 10*CurNumber + (Input - 48),
    read_number_input_helper(Number, UpdatedNumber).
read_number_input_helper(Number, Number).

% -----------------------------------------

% read_string_input(-String, +CurString)
% Reads a string input from the user and unifies it with the variable String.
read_string_input(String, CurString):-
    get_char(Char),
    Char \= '\n',
    append(CurString, [Char], UpdatedString),
    read_string_input(String, UpdatedString).

read_string_input(String, CurString) :-
    atom_chars(String, CurString).

% -----------------------------------------

% select_option(+Min, +Max, -Option)
% Prompts the user to enter a number within a specified range and unifies it with the variable Option.
select_option(Min, Max, Option) :-
    write('Option : '),
    repeat,
    read_number_input(Option),
    (between(Min, Max, Option) -> true ; 
    format('Invalid input. Please enter a number between ~d and ~d: ', [Min, Max]), fail).

% -----------------------------------------

% select_board(-Size)
% Prompts the user to enter a board size (an odd number greater than 7) and unifies it with the variable Size.
select_board(Size) :-
    repeat,
    read_number_input(Size),
    ((Size > 7 , Size mod 2 =:= 1) -> true ; 
    write('Invalid input. Please enter an odd number greater than 7: '), fail).

% -----------------------------------------

% inside_line(+Line, +Pos, +Size)
% Checks if the given position (Pos) is inside the specified line (Line) on a game board of the given size (Size).
inside_line(Line, Pos, Size) :-
    Starting is abs((Size + 1) // 2 - Line) + 1, 
    Ending is Size - Starting + 1,
    between(Starting, Ending, Pos).

% -----------------------------------------

% inside_board(+X-Y, +Size)
% Checks if the given position (X-Y) is inside the specified game board of the given size (Size).
inside_board(X-Y, Size) :-
    inside_line(X, Y, Size),
    inside_line(Y, X, Size).

% -----------------------------------------

% replace(+Pos, +Element, +List, -NewList)
% Replaces the element at the specified position (Pos) in the input list (List) with the given element (Element)
% and unifies the resulting list with the variable NewList.
replace(Pos, Element, List, NewList) :-
    nth1(Pos, List, _, Rest),
    nth1(Pos, NewList, Element, Rest).

% -----------------------------------------

% update_piece_pos(+Piece, +NewPos)
% Updates the position of the specified piece (Piece) with the new position (NewPos) on the game board.
update_piece_pos(Piece, NewPos) :-
    retract(position(Piece, _)),
    asserta(position(Piece, NewPos)).

% -----------------------------------------

% new_pos(+X-Y, +Direction, -NewPos)
% Computes the new position (NewPos) based on the specified direction (Direction) from the given position (X-Y).
new_pos(X-Y, 1, X-NewY) :- NewY is Y - 1.
new_pos(X-Y, 2, X-NewY) :- NewY is Y + 1.
new_pos(X-Y, 3, NewX-Y) :- NewX is X - 1.
new_pos(X-Y, 4, NewX-Y) :- NewX is X + 1.

% -----------------------------------------

% next_player(+CurrentPlayer, -NextPlayer)
% Determines the next player in the game based on the current player.
next_player(p1, p2).
next_player(p2, p1).

% -----------------------------------------

% opposite_direction(+Direction, -OppositeDirection)
% Determines the opposite direction for a given direction.
opposite_direction(1, 2).
opposite_direction(2, 1).
opposite_direction(3, 4).
opposite_direction(4, 3).

% -----------------------------------------

% skip_elem(+List, +Elems, -Index)
% Finds the index of the first element in the list (List) that is not a member of the specified elements (Elems).
skip_elem([Head|_], Elems, 1) :-
    \+ member(Head, Elems).

skip_elem([_|Tail], Elems, Index) :-
    skip_elem(Tail, Elems, IndexTail),
    Index is IndexTail + 1.

% -----------------------------------------

% find_elem(+List, +Elems, -Index)
% Finds the index of the first element in the list (List) that is a member of the specified elements (Elems).
find_elem([Elem|_], Elems, 0) :- member(Elem, Elems).
find_elem([_|Tail], Elems, Index) :-
    find_elem(Tail, Elems, IndexTail),
    Index is IndexTail + 1.

% -----------------------------------------

% find_reverse_elem(+List, +Elems, -Index)
% Finds the index of the last element in the reversed list (List) that is a member of the specified elements (Elems).
find_reverse_elem(List, Elems, A) :-
    reverse(List, Reversed),
    find_elem(Reversed, Elems, A).

% -----------------------------------------

% get_remaining(+N, +List, +Size, -Rest, +Direction)
% Extracts the remaining elements from the list based on the starting index and direction of movement.
get_remaining(N, List, Size, Rest, Direction):-
    (   (Direction =:= 2; Direction =:= 4) ->
        Len is Size - N,
        format('It is starting in ~d with length ~d\n', [N, Len]),
        sublist(List, Rest, N, Len, _)
    ;   Len is N - 1,
        format('It is starting at 0 with length ~d\n', [Len]),
        sublist(List, Rest, 0, Len, _)
    ).

% -----------------------------------------

% clear_data
% Clears all stored data related to game positions, player names, and computer levels.
clear_data:-
    retractall(position(_,_)),
    retractall(player_name(_,_)),
    retractall(computer_level(_,_)),
    retractall(moved_rocks(_,_)),
    retractall(chosen_rock(_,_,_)).
