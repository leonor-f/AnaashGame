:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

% play/0
% -------------------------------------------------------------------------
% Purpose:
%   Main predicate to start the game. Displays the game menu, prompts the
%   user for a choice, and processes the choice using the process_choice/1 predicate.
%
% Details:
%   - The menu provides options to display rules, start different types of games,
%     or exit.
%   - Based on user input, the game type and configuration are initialized.
play :-
    write('+--------------------------+'), nl,
    write('|          Anaash          |'), nl,
    write('+--------------------------+'), nl,
    write('| 0. Rules                 |'), nl,
    write('| 1. Human vs Human        |'), nl,
    write('| 2. Human vs Computer     |'), nl,
    write('| 3. Computer vs Human     |'), nl,
    write('| 4. Computer vs Computer  |'), nl,
    write('| 5. Exit                  |'), nl,
    write('+--------------------------+'), nl,
    write('Choose an option: '),
    read(Choice),
    process_choice(Choice).

% Process user choice from the main menu
process_choice(0) :-
    display_rules,
    play.
process_choice(1) :-
    enter_board_size(Size),
    start_game(player(red, human), player(blue, human), Size).
process_choice(2) :-
    enter_board_size(Size),
    select_difficulty(Level),
    start_game(player(red, human), player(blue, computer(Level)), Size).
process_choice(3) :-
    enter_board_size(Size),
    select_difficulty(Level),
    start_game(player(red, computer(Level)), player(blue, human), Size).
process_choice(4) :-
    enter_board_size(Size),
    select_difficulty(Level1),
    select_difficulty(Level2),
    start_game(player(red, computer(Level1)), player(blue, computer(Level2)), Size).
process_choice(5) :-
    write('Goodbye!'), nl.
process_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    play.

% Display the rules of the game
display_rules :-
    write('------------------------------------------------------------------------------'), nl,
    write('                                 Anaash Rules                                 '), nl,
    write('------------------------------------------------------------------------------'), nl, nl,
    write('# INTRODUCTION'), nl,
    write('The 2 players, *Red* and *Blue*, alternate turns moving 1 stack each, starting with *Red*.'), nl,
    write('A player must move if possible; if not, *skip his turn*.'), nl,
    write('At least 1 player always has a *valid move*.'), nl, nl,
    write('# POSITIONAL MOVES'), nl,
    write('Move 1 of your stacks to an adjacent, unoccupied square *1 step closer (Manhattan distance)* to its nearest stack.'), nl,
    write('Only stacks with *no orthogonal adjacencies* can make this move.'), nl, nl,
    write('# STACKING MOVES'), nl,
    write('Move 1 of your stacks onto an *orthogonally adjacent, friendly* stack of >/= height.'), nl, nl,
    write('# CAPTURING MOVES'), nl,
    write('Capture an *orthogonally adjacent enemy* stack of </= height.'), nl, nl,
    write('# GOAL OF THE GAME'), nl,
    write('To win you must *capture all enemy checkers*.'), nl, nl.

% Prompt user to select difficulty level
select_difficulty(Level) :-
    write('Select difficulty level:'), nl,
    write('1. Random'), nl,
    write('2. Greedy'), nl,
    read(Level),
    member(Level, [1, 2]), !.
select_difficulty(Level) :-
    write('Invalid choice. Please try again.'), nl,
    select_difficulty(Level).

% Prompt user to enter board size
enter_board_size(Size) :-
    write('Enter the board size (e.g., 6 for 6x6):'), nl,
    read(Size),
    (integer(Size), Size > 0), !.
enter_board_size(Size) :-
    write('Invalid board size. Please try again.'), nl,
    enter_board_size(Size).

% Start the game with the given players
start_game(Player1, Player2, Size) :-
    initial_state([Player1, Player2], Size, GameState),
    game_loop(GameState).

% initial_state(+GameConfig, -GameState)
% -------------------------------------------------------------------------
% Purpose:
%   Initializes the game state based on the provided game configuration.
%
% Parameters:
%   +GameConfig : A list containing Player1, Player2, and the board size.
%   -GameState  : The resulting game state represented as 
%                 game(Board, CurrentPlayer, [Player1, Player2]).
%
% Details:
%   - Generates an empty board of the specified size.
%   - Sets Player1 as the starting player.
initial_state([Player1, Player2], Size, game(Board, Player1, [Player1, Player2])) :- 
    board(Size, Board).

% display_game(+GameState)
% -------------------------------------------------------------------------
% Purpose:
%   Displays the current game state, including the board and the current player.
%
% Parameters:
%   +GameState : The current game state represented as 
%                game(Board, CurrentPlayer, _).
%
% Details:
%   - Displays the board using display_board/1.
%   - Indicates the current player whose turn it is to move.
display_game(game(Board, CurrentPlayer, _)) :-
    display_board(Board),
    format('Current player: ~w~n', [CurrentPlayer]).

% move(+GameState, +Move, -NewGameState)
% -------------------------------------------------------------------------
% Purpose:
%   Validates and applies a move to the current game state, resulting in a
%   new game state.
%
% Parameters:
%   +GameState    : The current game state before the move.
%   +Move         : The move to be executed, typically a tuple (X1, Y1, X2, Y2).
%   -NewGameState : The updated game state after applying the move.
%
% Details:
%   - Validates the move using valid_move/3.
%   - Updates the board by applying the move with apply_move/4.
%   - Switches the turn to the next player using next_player/3.
move(game(Board, CurrentPlayer, Players), Move, game(NewBoard, NextPlayer, Players)) :-
    valid_move(Board, CurrentPlayer, Move),
    apply_move(Board, CurrentPlayer, Move, NewBoard),
    next_player(Players, CurrentPlayer, NextPlayer).

% valid_moves(+GameState, -ListOfMoves)
% -------------------------------------------------------------------------
% Purpose:
%   Generates a list of all valid moves for the current player in the
%   specified game state.
%
% Parameters:
%   +GameState : The current game state.
%   -Moves     : A list of all valid moves (as unique tuples) for the active player.
%
% Details:
%   - Iterates through all positions on the board and determines possible moves.
%   - Ensures moves are within bounds and adhere to game rules.
%   - Removes duplicate moves using sort/2 to produce unique moves.
valid_moves(game(Board, player(Color, _), _), ListOfMoves) :-
    length(Board, Size),
    findall((X1, Y1, X2, Y2),
        (between(1, Size, X1), between(1, Size, Y1), 
         get_piece(Board, X1, Y1, Piece),
         piece_color(Piece, Color),
         between(1, Size, X2), between(1, Size, Y2), 
         one_step_move((X1, Y1), (X2, Y2)),
         valid_move(Board, player(Color, _), (X1, Y1, X2, Y2))),
        Moves),
    sort(Moves, ListOfMoves).

% game_over(+GameState, -Winner)
% -------------------------------------------------------------------------
% Purpose:
%   Checks whether the game is over and identifies the winner.
%
% Parameters:
%   +GameState : The current game state.
%   -Winner    : The winner of the game (red or blue).
%
% Details:
%   - The game is over if one players pieces are completely captured.
%   - Winner is set to the color of the player with remaining pieces.
game_over(game(Board, _, _), Winner) :-
    count_pieces(Board, red, 0), !, Winner = blue.
game_over(game(Board, _, _), Winner) :-
    count_pieces(Board, blue, 0), !, Winner = red.
% -------------------------------------------------------------------------
% Purpose:
%   Evaluates the current game state for a specified player, providing a
%   numerical value indicating the players advantage.
%
% Parameters:
%   +GameState : The current game state.
%   +Player    : The player (with their color) being evaluated.
%   -Value     : The evaluation score (positive indicates advantage, negative disadvantage).
%
% Details:
%   - Counts the pieces of the player and their opponent.
%   - The value is calculated as PlayerCount - OpponentCount.
value(game(Board, _, _), player(Color, _), Value) :-
    count_pieces(Board, Color, PlayerCount),
    opponent(Color, Opponent),
    count_pieces(Board, Opponent, OpponentCount),
    Value is PlayerCount - OpponentCount.

% choose_move(+GameState, +Player, -Move)
% -------------------------------------------------------------------------
% Purpose:
%   Chooses a move for the current player. For human players, prompts for input.
%   For computer players, selects a move based on the difficulty level.
%
% Parameters:
%   +GameState : The current game state.
%   +Player    : The current player (human or computer).
%   -Move      : The selected move.
%
% Details:
%   - Human players are prompted for input via read_move/2.
%   - Computer players choose a move based on the difficulty level:
%       Level 1: Random move (choose_random_move/2).
%       Level 2: Greedy move (choose_greedy_move/2).
choose_move(GameState, player(_, human), Move) :-
    read_move(GameState, Move).

choose_move(GameState, player(_, computer(Level)), Move) :-
    choose_move_by_level(Level, GameState, Move).

choose_move_by_level(1, GameState, Move) :-
    choose_random_move(GameState, Move).
choose_move_by_level(2, GameState, Move) :-
    choose_greedy_move(GameState, Move).

% choose_random_move(+GameState, -Move)
% -------------------------------------------------------------------------
% Purpose:
%   Selects a random valid move for the computer player.
%
% Parameters:
%   +GameState : The current game state.
%   -Move      : A randomly selected valid move.
%
% Details:
%   - Retrieves all valid moves using valid_moves/2.
%   - Randomly picks one move using random_member/2.
choose_random_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

% choose_greedy_move(+GameState, -Move)
% -------------------------------------------------------------------------
% Purpose:
%   Selects the best move for the computer player based on a greedy strategy.
%
% Parameters:
%   +GameState : The current game state.
%   -Move      : The move with the highest evaluation score.
%
% Details:
%   - Evaluates all valid moves and assigns scores to each.
%   - Selects the move with the highest score using keysort/2 and last/2.
choose_greedy_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    findall(Value-M, (member(M, Moves), evaluate_move(GameState, M, Value)), ValuedMoves),
    keysort(ValuedMoves, Sorted),
    last(Sorted, _-Move).

% read_move(+GameState, -Move)
% -------------------------------------------------------------------------
% Purpose:
%   Prompts the user to input a move and ensures it is valid.
%
% Parameters:
%   +GameState : The current game state.
%   -Move      : The valid move entered by the user.
%
% Details:
%   - Checks if the move is among the valid options.
%   - Repeats the prompt if the input is invalid or has syntax errors.
read_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    write('Enter your move in the format (X1, Y1, X2, Y2): '), nl,
    catch(read(UserInput), _, (write('Syntax error. Try again.'), nl, read_move(GameState, Move))),
    (member(UserInput, Moves) -> Move = UserInput; write('Invalid move. Try again.'), nl, read_move(GameState, Move)).

% evaluate_move(+GameState, +Move, -Value)
% -------------------------------------------------------------------------
% Purpose:
%   Simulates a move and calculates its resulting value for AI decision-making.
%
% Parameters:
%   +GameState : The current game state.
%   +Move      : The move to evaluate.
%   -Value     : The computed value of the resulting state.
evaluate_move(GameState, Move, Value) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, _, Value).

% game_loop(+GameState)
% -------------------------------------------------------------------------
% Purpose:
%   Manages the main game flow, including turns and endgame checks.
%
% Parameters:
%   +GameState : The current game state.
%
% Details:
%   - Displays the game board.
%   - Checks for game-over conditions and declares the winner.
%   - Alternates turns between players until the game ends.
game_loop(GameState) :-
    display_game(GameState),
    ( game_over(GameState, Winner) ->
        write('---------------------------'), nl,
        format('  GAME OVER! Winner: ~w~n', [Winner]),
        write('---------------------------'), nl,
        play
    ; GameState = game(_, CurrentPlayer, _),
      valid_moves(GameState, Moves),
      handle_moves(GameState, Moves, CurrentPlayer)
    ).

handle_moves(GameState, [], CurrentPlayer) :-
    format('No valid moves for ~w. Skipping turn.~n', [CurrentPlayer]),
    next_player(GameState, NewGameState),
    game_loop(NewGameState).

handle_moves(GameState, Moves, CurrentPlayer) :-
    GameState = game(Board, CurrentPlayer, _),
    CurrentPlayer = player(Color, _),
    format('Valid moves for ~w: ~w~n', [Color, Moves]),
    format('~w\'s turn.~n', [CurrentPlayer]),
    choose_move(GameState, CurrentPlayer, Move),
    format('Chosen move: ~w~n', [Move]),
    move(GameState, Move, NewGameState),
    move_type(Board, Color, Move, Type),
    Move = (X1, Y1, X2, Y2),
    format('Applied ~w move: ~w -> ~w~n', [Type, (X1, Y1), (X2, Y2)]),
    game_loop(NewGameState).

% next_player(+GameState, -NewGameState)
% -------------------------------------------------------------------------
% Purpose:
%   Updates the game state to switch to the next player.
%
% Parameters:
%   +GameState    : The current game state.
%   -NewGameState : The updated game state with the next players turn.
next_player(game(Board, CurrentPlayer, Players), game(Board, NextPlayer, Players)) :-
    next_player(Players, CurrentPlayer, NextPlayer).

next_player([Player1, Player2], Player1, Player2).
next_player([Player1, Player2], Player2, Player1).

% board(+Size, -Board)
% -------------------------------------------------------------------------
% Purpose:
%   Initializes a game board of a given size.
%
% Parameters:
%   +Size  : The size of the board (e.g., 6 for a 6x6 board).
%   -Board : The generated game board.
board(Size, Board) :-
    length(Board, Size),
    maplist(length_(Size), Board),
    fill_board(Board).

% fill_board(-Board)
% -------------------------------------------------------------------------
% Purpose:
%   Fills the board with the initial checkered pattern of pieces.
%
% Parameters:
%   -Board : The board to be filled.
fill_board(Board) :-
    length(Board, Size),
    numlist(1, Size, Indices),
    maplist(fill_row, Board, Indices).

fill_row(Row, RowIndex) :-
    length(Row, Size),
    numlist(1, Size, ColIndices),
    maplist(fill_cell(RowIndex), Row, ColIndices).

fill_cell(RowIndex, Cell, ColIndex) :-
    fill_cell_color(RowIndex, ColIndex, Cell).

fill_cell_color(RowIndex, ColIndex, red(1)) :-
    (RowIndex + ColIndex) mod 2 =:= 0.
fill_cell_color(RowIndex, ColIndex, blue(1)) :-
    (RowIndex + ColIndex) mod 2 =\= 0.

% display_board(+Board)
% -------------------------------------------------------------------------
% Purpose:
%   Displays the current game board row by row.
%
% Parameters:
%   +Board : The board to display.
display_board(Board) :-
    maplist(display_row, Board).

display_row(Row) :-
    maplist(display_cell, Row),
    nl.

display_cell(red(H)) :- format(' r~d ', [H]).
display_cell(blue(H)) :- format(' b~d ', [H]).
display_cell(empty) :- format('  . ', []).

% count_pieces(+Board, +Color, -Count)
% -------------------------------------------------------------------------
% Purpose:
%   Counts the number of pieces of a specific color on the board.
%
% Parameters:
%   +Board : The current game board.
%   +Color : The color to count (e.g., red, blue).
%   -Count : The total number of pieces of the given color.
count_pieces(Board, Color, Count) :-
    findall(Piece, 
            (member(Row, Board), 
             member(Piece, Row), 
             piece_color(Piece, Color)), 
            Pieces),
    length(Pieces, Count).

% opponent(+Color, -Opponent)
% -------------------------------------------------------------------------
% Purpose:
%   Determines the opponents color.
%
% Parameters:
%   +Color    : The color of the current player.
%   -Opponent : The color of the opponent.
opponent(red, blue).
opponent(blue, red).

% valid_move(+Board, +Player, +Move)
% -------------------------------------------------------------------------
% Purpose:
%   Validates a move by determining its type and ensuring it conforms to rules.
%
% Parameters:
%   +Board : The current game board.
%   +Player: The player making the move (includes color and other details).
%   +Move  : The move to validate (e.g., (X1, Y1, X2, Y2)).
valid_move(Board, player(Color, _), (X1, Y1, X2, Y2)) :-
    move_type(Board, Color, (X1, Y1, X2, Y2), Type),
    valid_move(Board, Color, (X1, Y1, X2, Y2), Type).

% valid_move(+Board, +Color, +Move, positional)
% -------------------------------------------------------------------------
% Purpose:
%   Validates a positional move.
%
% Conditions:
%   - The target position (X2, Y2) must be empty (`is_empty/3`), i.e., unoccupied.
%   - The move must be a "manhattan distance of 1" (meaning one square closer in one direction) 
%     from the start position (`manhattan_distance/3`).
%   - The starting position (X1, Y1) must not have any orthogonal adjacencies to other pieces (`no_orthogonal_adjacencies/3`).
%   - The move must bring the piece closer to the nearest stack (`closer_to_nearest_stack/3`), which ensures the piece is moving towards a stack.
valid_move(Board, _, (X1, Y1, X2, Y2), positional) :-
    manhattan_distance((X1, Y1), (X2, Y2), 1),
    is_empty(Board, X2, Y2),
    no_orthogonal_adjacencies(Board, X1, Y1),
    closer_to_nearest_stack(Board, (X1, Y1), (X2, Y2)).

% valid_move(+Board, +Color, +Move, stacking)
% -------------------------------------------------------------------------
% Purpose:
%   Validates a stacking move.
%
% Conditions:
%   - The piece at the starting position (X1, Y1) must belong to the player (`piece_color/2`).
%   - The piece at the target position (X2, Y2) must also belong to the player (`piece_color/2`).
%   - The height of the piece at the starting position (X1, Y1) must be less than or equal to the height of the piece at the target position (X2, Y2) (`piece_height/2`).
%   - The move must be a valid "one square orthogonal" move (`manhattan_distance/3`).
valid_move(Board, Color, (X1, Y1, X2, Y2), stacking) :-
    get_piece(Board, X1, Y1, Piece1),
    piece_color(Piece1, Color),
    get_piece(Board, X2, Y2, Piece2),
    piece_color(Piece2, Color),
    piece_height(Piece1, H1),
    piece_height(Piece2, H2),
    H1 =< H2,
    manhattan_distance((X1, Y1), (X2, Y2), 1).

% valid_move(+Board, +Color, +Move, capturing)
% -------------------------------------------------------------------------
% Purpose:
%   Validates a capturing move.
%
% Conditions:
%   - The piece at the starting position (X1, Y1) must belong to the player (`piece_color/2`).
%   - The piece at the target position (X2, Y2) must belong to the opponent (`piece_color/2` and `opponent/2`).
%   - The height of the piece at the starting position (X1, Y1) must be greater than or equal to the height of the piece at the target position (X2, Y2) (`piece_height/2`).
%   - The move must be a valid "one square orthogonal" move (`manhattan_distance/3`).
valid_move(Board, Color, (X1, Y1, X2, Y2), capturing) :-
    get_piece(Board, X1, Y1, Piece1),
    piece_color(Piece1, Color),
    get_piece(Board, X2, Y2, Piece2),
    opponent(Color, Opponent),
    piece_color(Piece2, Opponent),
    piece_height(Piece1, H1),
    piece_height(Piece2, H2),
    H1 >= H2,
    manhattan_distance((X1, Y1), (X2, Y2), 1).

% one_step_move(+Start, +End)
% -------------------------------------------------------------------------
% Purpose:
%   Checks if a move is exactly one step in any orthogonal direction.
%
% Parameters:
%   +Start : The starting position (X1, Y1).
%   +End   : The ending position (X2, Y2).
one_step_move((X1, Y1), (X2, Y2)) :-
    DX is abs(X2 - X1),
    DY is abs(Y2 - Y1),
    DX + DY =:= 1.

% apply_move(+Board, +Player, +Move, -NewBoard)
% -------------------------------------------------------------------------
% Purpose:
%   Applies a move to the board and updates the game state.
%
% Parameters:
%   +Board    : The current game board.
%   +Player   : The player making the move.
%   +Move     : The move to apply.
%   -NewBoard : The updated game board after the move.
%
% Conditions:
%   - Depending on the type of move, the board is modified in different ways.
%   - For positional moves (`positional`), the piece is moved from the starting position (X1, Y1)
%     to the target position (X2, Y2), and the starting position becomes empty.
%   - For stacking moves (`stacking`), the pieces at both the start and target positions are merged,
%     increasing the height of the piece at the target position.
%   - For capturing moves (`capturing`), the piece at the target position is removed, and the piece at
%     the starting position is moved to the target position.
apply_move(Board, player(Color,_), (X1, Y1, X2, Y2), NewBoard) :-
    move_type(Board, Color, (X1, Y1, X2, Y2), MoveType),
    apply_move(Board, MoveType, (X1, Y1, X2, Y2), NewBoard).

% Apply a positional move to the board
apply_move(Board, positional, (X1, Y1, X2, Y2), NewBoard) :-
    get_piece(Board, X1, Y1, Piece),
    set_piece(Board, X1, Y1, empty, TempBoard),
    set_piece(TempBoard, X2, Y2, Piece, NewBoard).

% Apply a stacking move to the board
apply_move(Board, stacking, (X1, Y1, X2, Y2), NewBoard) :-
    get_piece(Board, X1, Y1, Piece1),
    get_piece(Board, X2, Y2, Piece2),
    piece_height(Piece1, H1),
    piece_height(Piece2, H2),
    NewHeight is H1 + H2,
    change_height(Piece2, NewHeight, NewPiece),
    set_piece(Board, X1, Y1, empty, TempBoard),
    set_piece(TempBoard, X2, Y2, NewPiece, NewBoard).

% Apply a capturing move to the board
apply_move(Board, capturing, (X1, Y1, X2, Y2), NewBoard) :-
    get_piece(Board, X1, Y1, Piece1),
    piece_height(Piece1, H1),
    set_piece(Board, X1, Y1, empty, TempBoard),
    change_height(Piece1, H1, NewPiece),
    set_piece(TempBoard, X2, Y2, NewPiece, NewBoard).

% move_type(+Board, +Color, +Move, -Type)
% -------------------------------------------------------------------------
% Purpose:
%   Determines the type of a move (positional, stacking, or capturing).
%
% Parameters:
%   +Board : The current game board.
%   +Color : The color of the players pieces.
%   +Move  : The move to evaluate.
%   -Type  : The determined type of move.
%
% Conditions:
%   - For a positional move (`positional`), the target position must be empty, the move must be
%     one square closer to a stack, and there must be no orthogonal adjacencies at the starting position.
%   - For a stacking move (`stacking`), both the source and target positions must have pieces of the
%     same color, and the height of the piece at the source position must be less than or equal to the
%     height of the piece at the target position.
%   - For a capturing move (`capturing`), the source and target positions must be occupied by pieces
%     of different colors, and the height of the piece at the source position must be greater than or equal
%     to the height of the piece at the target position.
move_type(Board, Color, (X1, Y1, X2, Y2), positional) :-
    get_piece(Board, X1, Y1, Piece),
    piece_color(Piece, Color),
    is_empty(Board, X2, Y2),
    no_orthogonal_adjacencies(Board, X1, Y1),
    closer_to_nearest_stack(Board, (X1, Y1), (X2, Y2)).

move_type(Board, Color, (X1, Y1, X2, Y2), stacking) :-
    get_piece(Board, X1, Y1, Piece1),
    piece_color(Piece1, Color),
    get_piece(Board, X2, Y2, Piece2),
    piece_color(Piece2, Color),
    piece_height(Piece1, H1),
    piece_height(Piece2, H2),
    H1 =< H2.

move_type(Board, Color, (X1, Y1, X2, Y2), capturing) :-
    get_piece(Board, X1, Y1, Piece1),
    piece_color(Piece1, Color),
    get_piece(Board, X2, Y2, Piece2),
    opponent(Color, Opponent),
    piece_color(Piece2, Opponent),
    piece_height(Piece1, H1),
    piece_height(Piece2, H2),
    H1 >= H2.


% Auxiliary predicates

% get_piece(+Board, +X, +Y, -Piece)
% -------------------------------------------------------------------------
% Purpose:
%   Retrieves the piece at a specific position on the board.
%
% Parameters:
%   +Board : The game board.
%   +X, +Y : The coordinates of the position.
%   -Piece : The piece at the specified position (or empty if none).
get_piece(Board, X, Y, Piece) :-
    % nth1(Y, Board, Row),
    % nth1(X, Row, Piece).
    reverse(Board, ReversedBoard),
    nth1(Y, ReversedBoard, Row),
    nth1(X, Row, Piece).

% set_piece(+Board, +X, +Y, +Piece, -NewBoard)
% -------------------------------------------------------------------------
% Purpose:
%   Places a piece at a specific position on the board, creating a new board state.
%
% Parameters:
%   +Board    : The current game board.
%   +X, +Y    : The coordinates of the position.
%   +Piece    : The piece to place (or empty to clear the position).
%   -NewBoard : The updated game board.
set_piece(Board, X, Y, Piece, NewBoard) :-
    % nth1(Y, Board, Row, RestRows),
    % nth1(X, Row, _, RestCells),
    % nth1(X, NewRow, Piece, RestCells),
    % nth1(Y, NewBoard, NewRow, RestRows).
    reverse(Board, ReversedBoard),
    nth1(Y, ReversedBoard, Row, RestRows),
    nth1(X, Row, _, RestCells),
    nth1(X, NewRow, Piece, RestCells),
    nth1(Y, NewReversedBoard, NewRow, RestRows),
    reverse(NewReversedBoard, NewBoard).

% is_empty(+Board, +X, +Y)
% -------------------------------------------------------------------------
% Purpose:
%   Checks if a specific position on the board is empty.
%
% Parameters:
%   +Board : The game board.
%   +X, +Y : The coordinates of the position.
is_empty(Board, X, Y) :-
    get_piece(Board, X, Y, empty).

length_(Length, List) :- length(List, Length).

% piece_color(+Piece, -Color)
% -------------------------------------------------------------------------
% Purpose:
%   Extracts the color of a piece.
%
% Parameters:
%   +Piece : The piece to examine.
%   -Color : The color of the piece.
piece_color(Piece, Color) :-
    Piece =.. [Color, _].

% piece_height(+Piece, -Height)
% -------------------------------------------------------------------------
% Purpose:
%   Extracts the height of a piece.
%
% Parameters:
%   +Piece  : The piece to examine.
%   -Height : The height of the piece.
piece_height(Piece, Height) :-
    Piece =.. [_, Height].

% change_height(+Piece, +NewHeight, -NewPiece)
% -------------------------------------------------------------------------
% Purpose:
%   Updates the height of a piece to create a new piece.
%
% Parameters:
%   +Piece     : The original piece.
%   +NewHeight : The new height of the piece.
%   -NewPiece  : The piece with the updated height.
change_height(Piece, NewHeight, NewPiece) :-
    Piece =.. [Color, _],
    NewPiece =.. [Color, NewHeight].

% manhattan_distance(+Pos1, +Pos2, -Distance)
% -------------------------------------------------------------------------
% Purpose:
%   Calculates the Manhattan distance between two positions.
%
% Parameters:
%   +Pos1, +Pos2 : The positions to compare.
%   -Distance    : The Manhattan distance between the positions.
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

no_orthogonal_adjacencies(Board, X, Y) :-
    \+ orthogonal_adjacency(Board, X, Y, _).

% orthogonal_adjacency(+Board, +X, +Y, -Neighbor)
% -------------------------------------------------------------------------
% Purpose:
%   Finds an orthogonally adjacent position that is not empty.
%
% Parameters:
%   +Board    : The game board.
%   +X, +Y    : The coordinates of the current position.
%   -Neighbor : The coordinates of the adjacent position.
orthogonal_adjacency(Board, X, Y, (X2, Y)) :-
    X2 is X + 1,
    get_piece(Board, X2, Y, Piece),
    Piece \= empty.

orthogonal_adjacency(Board, X, Y, (X2, Y)) :-
    X2 is X - 1,
    get_piece(Board, X2, Y, Piece),
    Piece \= empty.

orthogonal_adjacency(Board, X, Y, (X, Y2)) :-
    Y2 is Y + 1,
    get_piece(Board, X, Y2, Piece),
    Piece \= empty.

orthogonal_adjacency(Board, X, Y, (X, Y2)) :-
    Y2 is Y - 1,
    get_piece(Board, X, Y2, Piece),
    Piece \= empty.

% closer_to_nearest_stack(+Board, +Start, +End)
% -------------------------------------------------------------------------
% Purpose:
%   Checks if a move reduces the distance to the nearest stack.
%
% Parameters:
%   +Board : The game board.
%   +Start : The starting position.
%   +End   : The proposed ending position.
closer_to_nearest_stack(Board, (X1, Y1), (X2, Y2)) :-
    nearest_stack(Board, (X1, Y1), (XN, YN)),
    manhattan_distance((X1, Y1), (XN, YN), D1),
    manhattan_distance((X2, Y2), (XN, YN), D2),
    D2 < D1.

% nearest_stack(+Board, +Pos, -NearestStack)
% -------------------------------------------------------------------------
% Purpose:
%   Finds the nearest stack to a given position on the board.
%
% Parameters:
%   +Board        : The game board.
%   +Pos          : The position to search from.
%   -NearestStack : The coordinates of the nearest stack.
nearest_stack(Board, (X, Y), (XN, YN)) :-
    findall((X2, Y2), (get_piece(Board, X2, Y2, Piece), Piece \= empty, (X2, Y2) \= (X, Y)), Stacks),
    maplist(manhattan_distance((X, Y)), Stacks, Distances),
    % min_list(Distances, MinDistance),
    min_in_list(Distances, MinDistance),
    nth1(Index, Distances, MinDistance),
    nth1(Index, Stacks, (XN, YN)).

min_in_list([Min], Min).
min_in_list([H|T], Min) :-
    min_in_list(T, TailMin),
    Min is min(H, TailMin).


% Test cases
% Red plays first
first_move([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    % board(6, Board),
    format('Chosen move: (6,5,6,6)~n'), nl,
    format('Applied capturing move: 6,5 -> 6,6~n'),
    board(6, [
        [red(1), blue(1), red(1), blue(1), red(1), blue(1)],
        [blue(1), red(1), blue(1), red(1), blue(1), empty],
        [red(1), blue(1), red(1), blue(1), red(1), blue(1)],
        [blue(1), red(1), blue(1), red(1), blue(1), red(1)],
        [red(1), blue(1), red(1), blue(1), red(1), blue(1)],
        [blue(1), red(1), blue(1), red(1), blue(1), red(1)]
    ]),
    display_board(Board),
    display_game(game(Board, Player1, [Player1, Player2])).

% Intermediate game states
stacking_move([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    format('Chosen move: (4,5,5,5)~n'), nl,
    format('Applied stacking move: 4,5 -> 5,5~n'),
    Board = [
        [red(1), blue(1), red(1), blue(1), empty, empty],
        [blue(1), red(1), blue(1), empty, red(3), empty],
        [red(1), blue(1), red(1), blue(1), blue(1), empty],
        [blue(1), red(1), blue(1), red(1), empty, blue(1)],
        [red(1), blue(1), red(1), blue(1), red(1), empty],
        [blue(1), red(1), blue(1), red(1), blue(1), blue(1)]
    ].

positional_move([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    format('Chosen move: (6,1,6,2)~n'), nl,
    format('Applied positional move: 6,1 -> 6,2~n'),
    Board = [
        [red(1), empty, red(1), blue(1), empty, empty],
        [empty, red(1), blue(1), empty, empty, empty],
        [red(1), blue(1), empty, blue(1), empty, red(3)],
        [blue(1), empty, blue(1), red(1), empty, empty],
        [red(1), blue(1), red(1), blue(1), blue(1), blue(1)],
        [empty, red(1), blue(1), red(1), empty, empty]
    ].

capturing_move([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    format('Chosen move: (2,2,2,1)~n'), nl,
    format('Applied capturing move: 2,2 -> 2,1~n'),
    Board = [
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [blue(2), empty, empty, empty, empty, empty],
        [empty, red(5), empty, empty, empty, empty]
    ].

% Near-final game state
near_final_state([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    format('Chosen move: (1,2,2,2)~n'), nl,
    format('Applied positional move: 1,2 -> 2,2~n'),
    Board = [
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, blue(2), empty, empty, empty, empty],
        [empty, red(5), empty, empty, empty, empty]
    ].

% Final game state
final_state([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    format('Chosen move: (2,1,2,2)~n'), nl,
    format('Applied capturing move: 2,1 -> 2,2~n'),
    Board = [
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, red(5), empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty]
    ],
    format('Current player: player(blue,human)~n'),
    format('---------------------------~n'),
    format('  GAME OVER!  Winner: red  ~n'),
    format('---------------------------~n').

% No valid moves for red
no_moves([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    Board = [
        [empty, empty, empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty],
        [empty, blue(5), empty, empty, empty, empty],
        [blue(3), red(1), blue(2), empty, empty, empty],
        [empty, blue(3), empty, empty, empty, empty],
        [empty, empty, empty, empty, empty, empty]
    ],
    format('Current player: player(red,human)~n'),
    format('No valid moves for player(red,human). Skipping turn.~n'),
    format('Current player: player(blue,human)~n'),
    format('Valid moves for blue: [(3, 2, 2, 2), (3, 2, 4, 2), (3, 2, 3, 1), (3, 2, 3, 3), (4, 1, 3, 1), (4, 1, 5, 1), (4, 1, 4, 2), (4, 3, 3, 3), (4, 3, 5, 3), (4, 3, 4, 2), (4, 3, 4, 4), (5, 2, 4, 2), (5, 2, 5, 1), (5, 2, 5, 3)]').