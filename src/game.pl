:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(format)).
:- use_module(library(random)).

% Define the colors using ANSI escape codes
color(red, '\e[31m\e[0m').    % Red
color(blue, '\e[34m\e[0m').   % Blue
color(empty, '\e[37m\e[0m').  % Grey

% Main predicate to start the game
play :-
    nl,
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
    start_game(player(red, human), player(blue, human)).
process_choice(2) :-
    select_difficulty(Level),
    start_game(player(red, human), player(blue, computer(Level))).
process_choice(3) :-
    select_difficulty(Level),
    start_game(player(red, computer(Level)), player(blue, human)).
process_choice(4) :-
    select_difficulty(Level1),
    select_difficulty(Level2),
    start_game(player(red, computer(Level1)), player(blue, computer(Level2))).
process_choice(5) :-
    write('Goodbye!'), nl.
process_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    play.

% Display the rules of the game
display_rules :-
    write('----------------------------------------------------------------------------------'), nl,
    write('                                   Anaash Rules                                   '), nl,
    write('----------------------------------------------------------------------------------'), nl, nl,
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
    (Level = 1 ; Level = 2), !.
select_difficulty(Level) :-
    write('Invalid choice. Please try again.'), nl,
    select_difficulty(Level).

% Start the game with the given players
start_game(Player1, Player2) :-
    initial_state([Player1, Player2], GameState),
    game_loop(GameState).

% Initialize the game state
initial_state([Player1, Player2], game(Board, Player1, [Player1, Player2])) :-
    board(6, Board). % Default board size is 6x6

% Display the current game state (player and board)
display_game(game(Board, CurrentPlayer, _)) :-
    display_board(Board),
    format('                                             ', []),
    CurrentPlayer = player(Color, _),
    format('~w player\'s turn!~n', [Color]), nl.

% Validate and execute a move
move(game(Board, CurrentPlayer, Players), Move, game(NewBoard, NextPlayer, Players)) :-
    valid_move(Board, CurrentPlayer, Move),
    apply_move(Board, CurrentPlayer, Move, NewBoard),
    next_player(Players, CurrentPlayer, NextPlayer).

% Generate a list of all valid moves
valid_moves(game(Board, player(Color, _), _), Moves) :-
    findall((X1, Y1, X2, Y2), 
        (between(1, 6, X1), between(1, 6, Y1),
         get_piece(Board, X1, Y1, Piece),
         piece_color(Piece, Color),
         between(1, 6, X2), between(1, 6, Y2), 
         one_step_move((X1, Y1), (X2, Y2)),
         valid_move(Board, player(Color, _), (X1, Y1, X2, Y2))),
        Moves).

% Check if the game is over and determine the winner
game_over(game(Board, _, _), Winner) :-
    count_pieces(Board, red, RedCount),
    count_pieces(Board, blue, BlueCount),
    (RedCount = 0 -> Winner = blue ;
     BlueCount = 0 -> Winner = red ;
     fail).

% Evaluate the game state for a given player
value(game(Board, _, _), player(Color, _), Value) :-
    count_pieces(Board, Color, PlayerCount),
    opponent(Color, Opponent),
    count_pieces(Board, Opponent, OpponentCount),
    Value is PlayerCount - OpponentCount.

% Choose a move for the computer player
choose_move(GameState, player(_, human), Move) :-
    prompt_move(GameState, Move).
choose_move(GameState, player(_, computer(Level)), Move) :-
    (Level = 1 -> choose_random_move(GameState, Move)
    ; Level = 2 -> choose_greedy_move(GameState, Move)).

choose_random_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

choose_greedy_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    findall(Value-M, (member(M, Moves), evaluate_move(GameState, M, Value)), ValuedMoves),
    keysort(ValuedMoves, Sorted),
    last(Sorted, _-Move).

% Prompt the user to input a move
prompt_move(GameState, Move) :-
    valid_moves(GameState, Moves),
    write('Enter your move in the format (X1, Y1, X2, Y2): '), nl,
    catch(read(UserInput), _, (write('Syntax error. Try again.'), nl, prompt_move(GameState, Move))),
    (member(UserInput, Moves) -> Move = UserInput; write('Invalid move. Try again.'), nl, prompt_move(GameState, Move)).

% Evaluate a move for AI decision-making
evaluate_move(GameState, Move, Value) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, _, Value).

% Main gameplay loop
game_loop(GameState) :-
    display_game(GameState),
    ( game_over(GameState, Winner) ->
        format('                                   ', []),
        write('---------------------------'), nl,
        format('                                   ', []),
        format('  GAME OVER! Winner: ~w~n', [Winner]),
        format('                                   ', []),
        write('---------------------------'), nl,
        play
    ; GameState = game(_, CurrentPlayer, _),
      valid_moves(GameState, Moves),
      % Extract Color from CurrentPlayer
      CurrentPlayer = player(Color, _),
      format('Valid moves for ~w: ~w~n', [Color, Moves]), nl,
      ( Moves = [] ->
          format('No valid moves for ~w. Skipping turn.~n', [CurrentPlayer]),
          next_player(GameState, NewGameState),
          game_loop(NewGameState)
      ; choose_move(GameState, CurrentPlayer, Move),
        format('Move chosen: ~w~n', [Move]),
        move(GameState, Move, NewGameState),
        game_loop(NewGameState)
      )
    ).

% Determine the next player and update the game state
next_player(game(Board, CurrentPlayer, Players), game(Board, NextPlayer, Players)) :-
    next_player(Players, CurrentPlayer, NextPlayer).

% Define the initial board setup
board(Size, Board) :-
    length(Board, Size),
    maplist(length_(Size), Board),
    fill_board(Board).

% Fill the board with the initial checkered pattern
fill_board(Board) :-
    maplist(fill_row, Board, [1,2,3,4,5,6]).

fill_row(Row, RowIndex) :-
    maplist(fill_cell(RowIndex), Row, [1,2,3,4,5,6]).

fill_cell(RowIndex, Cell, ColIndex) :-
    ( (RowIndex + ColIndex) mod 2 =:= 0 -> Cell = red(1) ; Cell = blue(1) ).

display_board(Board) :-
    length(Board, Size),
    format('                                       ', []),
    display_top_coordinates(Size),
    display_rows(Board, 1).

% Display the top coordinates
display_top_coordinates(Size) :-
    format('    ', []),
    forall(between(1, Size, X), format('  ~w ', [X])),
    nl.

% Display each row with the left coordinates
display_rows([], _).
display_rows([Row|Rows], N) :-
    format('                                       ', []),
    format('~w | ', [N]),
    display_row(Row),
    nl,
    N1 is N + 1,
    display_rows(Rows, N1).

% Display a single row
display_row(Row) :-
    maplist(display_cell, Row).

display_cell(Cell) :-
    ( Cell = red(H) -> format('\e[41;30m r~d \e[0m', [H])
    ; Cell = blue(H) -> format('\e[44;30m b~d \e[0m', [H])
    ; format('\e[47;30m  . \e[0m', [])
    ).

% Determine the next player
next_player([Player1, Player2], Player1, Player2).
next_player([Player1, Player2], Player2, Player1).

% Count the pieces of a given color
count_pieces(Board, Color, Count) :-
    findall(Piece, 
            (member(Row, Board), 
             member(Piece, Row), 
             piece_color(Piece, Color)), 
            Pieces),
    length(Pieces, Count).

% Determine the opponents color
opponent(red, blue).
opponent(blue, red).

% Validate a move
valid_move(Board, player(Color, _), (X1, Y1, X2, Y2)) :-
    move_type(Board, Color, (X1, Y1, X2, Y2), Type),
    valid_move(Board, Color, (X1, Y1, X2, Y2), Type).

/* Move one of the stacks to an orthogonally adjacent, unoccupied square which is one square closer
(Manhattan distance) to its nearest stack, regardless of color or height */
valid_move(Board, Color, (X1, Y1, X2, Y2), positional) :-
    get_piece(Board, X1, Y1, Piece),
    piece_color(Piece, Color),
    manhattan_distance((X1, Y1), (X2, Y2), 1),
    is_empty(Board, X2, Y2),
    no_orthogonal_adjacencies(Board, X1, Y1),
    closer_to_nearest_stack(Board, (X1, Y1), (X2, Y2)).

% Move one of the stacks onto an orthogonally adjacent, friendly stack of equal or larger height
valid_move(Board, Color, (X1, Y1, X2, Y2), stacking) :-
    get_piece(Board, X1, Y1, Piece1),
    piece_color(Piece1, Color),
    get_piece(Board, X2, Y2, Piece2),
    piece_color(Piece2, Color),
    piece_height(Piece1, H1),
    piece_height(Piece2, H2),
    H1 =< H2,
    manhattan_distance((X1, Y1), (X2, Y2), 1).

% Capture an orthogonally adjacent enemy stack of equal or smaller height
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

% Check if the move is one step in any direction
one_step_move((X1, Y1), (X2, Y2)) :-
    DX is abs(X2 - X1),
    DY is abs(Y2 - Y1),
    DX + DY =:= 1.

% Apply a move to the board
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

% Determine the type of move
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
get_piece(Board, X, Y, Piece) :-
    nth1(Y, Board, Row),
    nth1(X, Row, Piece).

set_piece(Board, X, Y, Piece, NewBoard) :-
    nth1(Y, Board, Row, RestRows),
    nth1(X, Row, _, RestCells),
    nth1(X, NewRow, Piece, RestCells),
    nth1(Y, NewBoard, NewRow, RestRows).

is_empty(Board, X, Y) :-
    get_piece(Board, X, Y, empty).

length_(Length, List) :- length(List, Length).

piece_color(Piece, Color) :-
    Piece =.. [Color, _].

piece_height(Piece, Height) :-
    Piece =.. [_, Height].

change_height(Piece, NewHeight, NewPiece) :-
    Piece =.. [Color, _],
    NewPiece =.. [Color, NewHeight].

manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

no_orthogonal_adjacencies(Board, X, Y) :-
    \+ orthogonal_adjacency(Board, X, Y, _).

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

closer_to_nearest_stack(Board, (X1, Y1), (X2, Y2)) :-
    nearest_stack(Board, (X1, Y1), (XN, YN)),
    manhattan_distance((X1, Y1), (XN, YN), D1),
    manhattan_distance((X2, Y2), (XN, YN), D2),
    D2 < D1.

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

% Custom implementation of flatten_board/2
flatten_board(List, FlatList) :-
    flatten_board(List, [], FlatList).

flatten_board([], Acc, Acc).
flatten_board([Head|Tail], Acc, FlatList) :-
    !,
    flatten_board(Head, NewAcc, FlatList),
    flatten_board(Tail, Acc, NewAcc).
flatten_board(Atom, Acc, [Atom|Acc]) :-
    \+ is_list(Atom).