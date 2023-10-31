
# Implementation of Splut! in Prolog

This project was developed for the course Functional and Logic Programming, with the aim of implementing a two-player board game in the Prolog language. The board game is defined by its distinctive board and pieces, the rules governing piece movement (including possible moves), and the conditions that determine the game's outcome, whether it results in defeat or victory.

## Group Information

**Class**: 3LEIC14

**Group**: Splut1

**Members**: 

| Student Number | Name    | Contribution |
| -------------- | ------- | ------------ |
| 202105385      | Luís Du | 33 %         |
| ...            | ...     |              |

## Installation and Execution

1. **Download and Extract:**
   - Download the zip file containing our project.
   - Extract the contents of the zip file to a directory of your choice.

2. **Open SICStus Prolog:**
   - Open a terminal window in the project directory.
   - Type `sicstus` and press Enter to launch SICStus Prolog.

3. **Consult Main File:**
   - Inside the SICStus Prolog console, consult the `main.pl` file by typing:
     ```prolog
     ?- consult('src/main.pl').
     ```

4. **Start Playing:**
   - To start playing the game, type:
     ```prolog
     ?- play.
     ```
   -  Enjoy the game!

## Description of the game

**Splut!** is a 2-4 players abstract board game. In **Splut!**, players control unique characters like Stonetrolls, Dwarves, and Sorcerers on a diamond-shaped board. The goal is simple: eliminate all opposing Sorcerers by cleverly moving your pieces and strategically using Rocks.

### Game Objective

The objective of Splut! is to eliminate all opposing Sorcerers by landing a Rock on their heads. When a Sorcerer is eliminated, the entire team is removed from the board.

### Components

- **Game Board:** A diamond-shaped board with squares.
- **Players:** Red and Blue, each with a Stonetroll, a Dwarf, and a Sorcerer.
- **Special Pieces:** 4 Rocks, each placed in a corner of the diamond-shaped game board.

### Gameplay

- Players take turns, with each player making 3 steps per turn.
- The first player makes 1 step on their first turn, and the second player makes 2 steps on their first turn.
- Steps involve moving one of your pieces to an adjacent square horizontally or vertically.

### Stonetroll Moves

- **Pulling a Rock:** Stonetrolls can pull Rocks from squares right behind them.
- **Throwing a Rock:** Stonetrolls can throw Rocks horizontally or vertically. Rocks continue until they hit an obstacle (board edge, Stonetroll, Sorcerer, or another Rock). If a Sorcerer is hit, the corresponding team is eliminated. Throwing a Rock immediately ends the player's turn even if he made less than 3 steps.

### Dwarf Moves

- **Pushing Pieces:** Dwarves can push consecutive pieces (including Rocks) in a straight line. All pushed pieces move one square in the direction of the Dwarf's movement.

### Sorcerer Moves

- **Levitating Rocks:** Sorcerers can levitate stationary Rocks to an empty square in the same direction as their movement. Levitation is optional but continuous during a turn.



## Game Logic

### Internal Game State Representation

The game state is represented by:

- **Board:** a square matrix (list of lists) with different atoms for the pieces (*r* for rocks, *s* for sorcerers, *t* for stonetrolls, *d* for dwarves, *e* for empty spaces, *-* for cells outside the playing board)
- **Player:** the current player taking their turn, denoted by atoms *p1* or *p2*.
- **NrMoves:** the number of moves that the player has played in their turn. It can go up to 3.
- **NrTurns:** the number of turns that were played until the game finished. 


---

### Game State Visualization

Firstly, we have crafted a user-friendly **menu system** to enhance the overall gaming experience. Within our menu system, players can:

- **(1)** Select their desired **game mode**, whether it is Human/Human, Human/Computer, or computer/Computer.
- **(2)** Enter their **names** to join the game
- **(3)** Choose a customizable **board size**
- **(4)** Select **level of difficulty** when playing against the computer

To maintain the integrity of the system, we have implemented robust input validation mechanisms, ensuring that users provide valid inputs:

- in **(1)** and **(4)**, the input is validated using the predicate **select_option/3**, which prompts the user for input and ensures that the input is within a specified range (Min to Max). If the input is within the range, the predicate succeeds; otherwise, it fails and asks the user for input again. 

- in **(2)**, we opt to use the predicate **read_string_input/2** to enhand user experience, as it reads a string from the input character by character until a newline character is reached.

- in **(3)**, the predicate **select_board/1** prompts the user for a board size, ensuring that the input is an odd number greater than 7.

Moving on to the game playing state, the board is displayed using **display_game/1**

