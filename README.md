# Anaash
## Project description
Anaash is a **two-player** strategy game involving alternating turns where players move stacks of pieces on a NxN grid-based board, aiming to **capture all of the opponent’s pieces**. Each move requires careful planning and adherence to specific rules for **positional** movement, **stacking**, and **capturing**.
#### Grade: 18/20

## Rules
1. Players alternate turns, **starting with Red**.
2. A player must move if possible; otherwise, they **skip** his turn.
3. **Positional moves**: move a stack to an adjacent, unoccupied square closer to its nearest stack. Only stacks with no orthogonal neighbors can perform this move.
4. **Stacking moves**: move a stack onto an adjacent, friendly stack of equal or greater height. This increases the height of the stack, enhancing its strength.
5. **Capturing moves**: capture an adjacent enemy stack of equal or lesser height.
6. The game ends when one player **captures all of the opponent's pieces**.

## Installation and execution
### Linux and Windows:
1. Install SICStus Prolog 4.9.
2. Extract PFL_TP2_T02_Anaash_01.zip folder
3. Navigate to the game directory (on terminal): `cd path_to/PFL_TP2_T02_Anaash_01/src`
4. Start SICStus Prolog: `sicstus`
5. Load the game: `[game].`
6. Start the game: `play.`

If SICStus is not in the system’s `PATH` (i.e., ‘sicstus’ is not recognized as an internal or external command), use the full path to the SICStus executable (`cd path_to\sicstus.exe` on Windows and `cd path_to/sicstus` on Linux) and then on SICStus window type `prolog:set_current_directory(‘path_to/PFL_TP2_T02_Anaash_01/src’)`.

## Considerations for game extensions
When considering the game design, several factors were considered to ensure flexibility, engage, customization and scalability.

### Variable-sized boards
The game allows variable board sizing through the `board/2` predicate, allowing different difficulty levels and preferences for a customized gameplay. After selecting an option from the main menu, users are prompted to enter the board size preferable, which must be an **even and integer number**.

### AI difficulty levels
Players can select from multiple difficulty levels, ranging from random moves to a greedy AI strategy.

The **Random AI (Level 1)** chooses a move by selecting randomly from the list of all valid moves. The `choose_random_move/2` predicate first generates the list of valid moves using the `valid_moves/2` predicate, which evaluates all possible positions a piece can move to. After that, the `random_member/2` predicate (from Prolog’s random library) is used to pick a move randomly from the list, ensuring that the AI’s decision-making process is entirely based on chance.

The **Greedy AI (Level 2)** works by evaluating all valid moves and selecting the one that maximizes the player’s advantage. The `choose_greedy_move/2` predicate also generates a list of valid moves using the `valid_moves/2` predicate, then for each move, it calculates a value that represents the strategic advantage of making that move. This value is computed using the `evaluate_move/3` predicate, which simulates the move and evaluates the resulting game state with the `value/3` predicate. The `value/3` predicate computes a score based on **two factors**: the difference in the number of pieces between the two players and the difference in the total height of the stacks on the board. It calculates the value for each valid move, sorts these values, and selects the move with the highest value, ensuring the move with the greatest benefit based on the current game state is chosen.

## References
- [Official Rulebook](Anaash_rules.pdf): comprehensive details on game rules and strategies.
