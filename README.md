### Identification of the Topic and Group

**Game:** Anaash  
**Group Designation:** Group 1  
**Members:**
- Student 1: John Doe (Student Number: 123456) - Contribution: 50% (Implemented game logic, move validation, and AI decision-making)
- Student 2: Jane Smith (Student Number: 654321) - Contribution: 50% (Developed user interface, game loop, and documentation)

### Installation and Execution

**Linux:**
1. Install SICStus Prolog 4.9.
2. Clone the repository: `git clone https://github.com/your-repo/AnaashGame.git`
3. Navigate to the game directory: `cd AnaashGame/src`
4. Start SICStus Prolog: `sicstus`
5. Load the game: `[game].`
6. Start the game: `play.`

**Windows:**
1. Install SICStus Prolog 4.9.
2. Clone the repository: `git clone https://github.com/your-repo/AnaashGame.git`
3. Navigate to the game directory: `cd AnaashGame\src`
4. Start SICStus Prolog.
5. Load the game: `[game].`
6. Start the game: `play.`

### Description of the Game

Anaash is a two-player strategy game where players alternate turns moving stacks of pieces. The objective is to capture all of the opponent's pieces. Players can move their stacks to adjacent squares, stack their pieces, or capture opponent pieces based on specific rules.

**Rules:**
1. Players alternate turns, starting with Red.
2. A player must move if possible; otherwise, they skip their turn.
3. Positional Moves: Move a stack to an adjacent, unoccupied square closer to its nearest stack.
4. Stacking Moves: Move a stack onto an adjacent, friendly stack of equal or greater height.
5. Capturing Moves: Capture an adjacent enemy stack of equal or lesser height.
6. The game ends when one player captures all of the opponent's pieces.

**Links:**
- [Official Game Website](https://example.com)
- [Rule Book](https://example.com/rulebook)

### Considerations for Game Extensions

When extending the game design, we considered:
1. Variable-sized boards: The board size can be adjusted by modifying the `board/2` predicate.
2. Optional rules: Simplified rules for novice players and additional rules for expert players can be toggled via game settings.
3. Additional features: Implementing different AI difficulty levels and multiplayer support.

### Game Logic

**Game Configuration Representation:**
- The game configuration is represented by the `initial_state/2` predicate, which initializes the board and players.

**Internal Game State Representation:**
- The game state is represented by the `game(Board, CurrentPlayer, Players)` structure.
- `Board`: A list of lists representing the board.
- `CurrentPlayer`: The player whose turn it is.
- `Players`: A list of players.

**Move Representation:**
- Moves are represented by tuples `(X1, Y1, X2, Y2)`, indicating the start and end coordinates.
- The `move/3` predicate applies the move to the game state.

**User Interaction:**
- The game menu system is implemented in the `play/0` predicate.
- User interaction is handled by reading input and validating it using predicates like `prompt_move/2`.

### Conclusions

The Anaash game implementation in Prolog was a challenging but rewarding project. We successfully implemented the game logic, user interface, and AI decision-making. However, there are some limitations, such as the lack of advanced AI strategies and multiplayer support. Future improvements could include enhanced AI, networked multiplayer, and additional game modes.

### Bibliography

- [SICStus Prolog Documentation](https://sicstus.sics.se/documentation.html)
- [Prolog Programming for Artificial Intelligence](https://www.amazon.com/Prolog-Programming-Artificial-Intelligence-4th/dp/0321417461)
- [Official Game Website](https://example.com)
- [Rule Book](https://example.com/rulebook)