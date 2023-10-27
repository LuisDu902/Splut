
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

% -----------------------------------------
