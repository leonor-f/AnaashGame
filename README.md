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
