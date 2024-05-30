(* cloc --by-file --include-lang=OCaml . *)
open Battleship.Grid
open Battleship.Ai

let debug_mode = false
let () = print_endline ""
let () = print_endline "Welcome to Battleship!\n"

(** Print ship graphic *)
let tanker_ship =
  "\n\
  \              __/_____\\__\n\
  \          ____|_________|____\n\
  \                o o o o    /\n\
   ~~~~~~~~~~~`-(_(_(______)_)-'~~~~~~~~~~~\n"

let print_tanker_ships () = print_endline tanker_ship

let () =
  print_tanker_ships ();
  print_endline ""

(** [tutorial_check] is used to see to play a faster game where the ships are
    already placed down *)
let rec tutorial_check () =
  print_string "Would you like a tutorial of how to play the game? [y] or [n]: ";
  match String.lowercase_ascii (read_line ()) with
  | "y" -> true
  | "n" -> false
  | _ -> tutorial_check ()

let game_tutorial = tutorial_check ()

let rec print_with_delay messages =
  match messages with
  | [] -> ()
  | msg :: rest ->
      print_endline msg;
      Unix.sleep 2;
      print_with_delay rest

let tutorial_messages =
  [
    "Awesome, here's how to play Battleship";
    "Objective: Sink all your opponent's ships before they sink yours.";
    "Instructions:";
    "1. Each player places their ships on their own grid.";
    "In our game, you'll place ships on a 10 by 10 grid with coordinates 0 \
     through 9";
    "They look something like this!";
    "   0 1 2 3 4 5 6 7 8 9\n\
    \ +--------------------+\n\
    \ 0|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 1|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 2|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 3|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 4|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 5|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 6|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 7|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 8|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 9|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|";
    "You will each have 5 ships: Aircraft_carrier | Battleship | Cruiser | \
     Submarine | Destroyer";
    "Each ship has a different length and they must not overlap on your grids";
    "When you place a ship, it'll show up as a red squares like such- ";
    "   0 1 2 3 4 5 6 7 8 9\n\
    \  +--------------------+\n\
    \ 0|⬛️⬛️🟥⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 1|⬛️⬛️🟥⬛️⬛️⬛️🟥🟥🟥⬛️|\n\
    \ 2|⬛️⬛️🟥⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 3|⬛️⬛️🟥⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 4|⬛️⬛️⬛️⬛️⬛️⬛️⬛️🟥⬛️⬛️|\n\
    \ 5|⬛️⬛️🟥🟥🟥⬛️⬛️🟥⬛️⬛️|\n\
    \ 6|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 7|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 8|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 9|⬛️⬛️⬛🟥🟥🟥🟥🟥⬛️⬛️|";
    "Playing by yourself? No problem, play against an AI opponent";
    "2. Players take turns guessing coordinates to try and hit their \
     opponent's ships.";
    "If you miss the opponent's ship, it will show as a white square on your \
     opponent's grid.";
    "   0 1 2 3 4 5 6 7 8 9\n\
    \  +--------------------+\n\
    \ 0|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 1|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 2|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 3|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 4|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 5|⬛️⬛️⬜️⬜️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 6|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 7|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 8|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 9|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|";
    "But if you hit the opponent's ship, it will show as a green square on \
     your opponent's grid.";
    "   0 1 2 3 4 5 6 7 8 9\n\
    \  +--------------------+\n\
    \ 0|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 1|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 2|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 3|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 4|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 5|⬛️⬛️🟩🟩⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 6|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 7|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 8|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|\n\
    \ 9|⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️⬛️|";
    "3. The game ends when all of one player's ships are sunk. Best of luck in \
     Battlship!";
    "Before we get started, here's a few more questions.";
  ]

let () =
  print_endline "";
  if game_tutorial then (
    print_with_delay tutorial_messages;
    print_endline "")
  else
    print_endline "Alright, here's a few more questions before we get started. "

let init_grid = create_grid 10 10 0

(** [place_ai_ships] returns a grid with randomly placed ships *)
let place_ai_ships grid =
  let random_ship_list = generate_random_ship_list () in
  ai_place_ships_v3 random_ship_list grid

(** [quickplay_check] is used to see to play a faster game where the ships are
    already placed down (mainly for debugging) *)
let rec quickplay_check () =
  print_string
    "Do you want to have the ships already added? (quickplay mode) (eventually \
     get rid of) [y] or [n]: ";
  match String.lowercase_ascii (read_line ()) with
  | "y" -> false
  | "n" -> true
  | _ -> quickplay_check ()

let quickplay_off = if debug_mode then quickplay_check () else true

(** [check_other_grid] is used to see to see if the user wants to display the
    enemy's ships as they play (for debugging)*)
let rec check_other_grid () =
  print_string "Enable enemy grid display? (debug mode) [y] or [n]: ";
  match String.lowercase_ascii (read_line ()) with
  | "y" -> true
  | "n" -> false
  | _ -> check_other_grid ()

let see_other_grid = check_other_grid ()

let rec play_with_ai () =
  print_string "Would you like to play against the computer? [y] or [n]: ";
  match String.lowercase_ascii (read_line ()) with
  | "y" -> true
  | "n" -> false
  | _ -> play_with_ai ()

let ai_mode = play_with_ai ()

let rec get_difficulty_lvl () =
  if ai_mode then begin
    print_string "How strong of an AI? [1] or [2] or [3] or [4]: ";
    match read_line () with
    | "1" -> 1
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | _ -> get_difficulty_lvl ()
  end
  else 0

let difficulty_lvl = get_difficulty_lvl ()

let () =
  print_endline "";
  print_endline "OK, let's get the game started!";
  print_endline ""

let print_whitespace () =
  for _ = 1 to 20 do
    print_endline ""
  done

(* Depending on whether or not quickplay is active or not, places ships based on
   what the user wants to do *)
let player1_grid =
  if quickplay_off then (
    print_endline "Player 1 select your ships";
    print_grid false init_grid;
    ref (Battleship.Logic.player_place_ships init_grid []))
  else (
    print_endline "Player 1 grid:";
    let new_grid =
      init_grid |> add_ship 3 5 |> add_ship 4 5 |> add_ship 5 5 |> add_ship 6 5
      |> add_ship 7 5 |> add_ship 1 3 |> add_ship 1 4 |> add_ship 1 5
    in
    print_grid false new_grid;
    ref new_grid)

let player2_grid =
  if quickplay_off = true && ai_mode = false then (
    (* quickplay is off, ai_mode is off *)
    (* add code to add a waiter so player 1  *)
    print_whitespace ();
    print_endline "Player 2 select your ships";
    print_grid (not see_other_grid) init_grid;
    ref (Battleship.Logic.player_place_ships init_grid []))
  else if ai_mode then (
    (* quickplay is off, ai_mode is on OR quickplay is on, ai_mode is on *)
    let ai_grid = ref (place_ai_ships init_grid) in
    print_endline "\nThe AI has placed ships";
    (* print_grid (not see_other_grid) !ai_grid; *)
    ai_grid)
  else (
    (* quickplay is on, ai_mode is off *)
    print_endline "Player 2 grid:";
    let new_grid =
      init_grid |> add_ship 5 5 |> add_ship 5 4 |> add_ship 5 3 |> add_ship 5 6
      |> add_ship 5 7
    in
    print_grid (not see_other_grid) new_grid;
    ref new_grid)

(* player1/2_sees is what the player would see of the enemy's grid (they will
   see hits but not the actual location of ships) *)
let player1_sees = ref !player2_grid
let player2_sees = ref !player1_grid

(** [player_won grid] returns true if all ships in [grid] are hit and false
    otherwise. *)
let player_won grid = Battleship.Logic.player_won grid

(** [choose_shot grid] takes 2 integers from the user and updates the square to
    be hit and gives a short message about whether or not a ship was at the
    coordinates *)
let rec player_choose_shot grid turn =
  try
    print_string "Pick the x coordinate of your shot: ";
    let x_coord = int_of_string (read_line ()) in
    if x_coord < 0 then
      failwith "Your X coordinate of a shot cannot be negative";

    print_string "Pick the y coordinate of your shot: ";
    let y_coord = int_of_string (read_line ()) in
    if y_coord < 0 then
      failwith "Your Y coordinate of a shot cannot be negative";

    let enemy_grid = update_hit x_coord y_coord grid in
    let cell = get_cell x_coord y_coord 0 enemy_grid in
    let is_ship_hit = Battleship.Cell.is_ship_hit_or_empty cell in
    print_string ("\nPlayer " ^ string_of_int turn);
    Battleship.Cell.ship_hit_print_msg (get_cell x_coord y_coord 0 enemy_grid);
    if turn = 1 then player1_sees := update_hit x_coord y_coord !player1_sees
    else player2_sees := update_hit x_coord y_coord !player2_sees;

    if turn = 1 then (
      print_grid true !player1_sees;
      print_endline "Player 2's grid";
      if see_other_grid then print_grid false enemy_grid)
    else (
      print_grid true !player2_sees;
      print_endline "Player 1's grid";
      if see_other_grid then print_grid false enemy_grid);

    if is_ship_hit then enemy_grid else grid
  with Failure exn ->
    if debug_mode then print_string ("Failure: " ^ exn);
    print_endline "Please choose a valid coordinate";
    player_choose_shot grid turn

let rec find_hit_cells acc row =
  match row with
  | [] -> acc
  | cell :: rest ->
      if
        Battleship.Cell.is_ship_hit_or_empty cell
        && Battleship.Cell.is_cell_occupied cell
      then find_hit_cells ((cell.x, cell.y) :: acc) rest
      else find_hit_cells acc rest

let filter_adjacent_cells lst grid =
  List.filter
    (fun (x, y) ->
      let cell = get_cell x y 0 grid in
      x >= 0 && x < 10 && y >= 0 && y < 10
      && not (Battleship.Cell.is_cell_hit cell))
    lst

let adjacent_cells x y = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

(* [choose_informed_shot lvl prob grid] outputs coordinates of cells with the
   higher potential to be a ship.

   It finds cells that have been and are occupied, and checks cells directly
   adjacent to it to see if they have been hit, if so then it takes a random
   shot/hits a ship based difficulty level. If the ships directly adjacent to
   the cell have not been hit, it tries shooting the adjacent cells *)
let choose_informed_shot lvl prob grid =
  (* [find_hit_cells] gives a list of hit cells with ship *)
  let hit_cells =
    List.fold_left (fun acc row -> find_hit_cells acc row) [] grid
  in
  let _ =
    if debug_mode then
      List.iter
        (fun (x, y) -> Printf.printf "\nCell hit: (%i, %i)" x y)
        hit_cells
  in

  (* For each cell hit, check if adjacent cells hit, if not shoot adjacent
     cells. *)
  let filtered_adjacent =
    let lst = List.concat_map (fun (x, y) -> adjacent_cells x y) hit_cells in
    filter_adjacent_cells lst grid
  in
  let _ =
    if debug_mode then
      List.iter
        (fun (x, y) -> Printf.printf "\nCells to test:(%i, %i)" x y)
        filtered_adjacent
  in
  (* if no nearby cells, choose a random shot/hit a ship based on
     difficulty/probability*)
  match filtered_adjacent with
  | [] ->
      if lvl = 3 && Random.float 1.0 < prob then
        gen_random_coords_with_ship 10 grid
      else gen_random_coordinates 10 grid
  | (x, y) :: _ -> (x, y)

(* [ai_choose_shot grid turn] updates player 2's grid (ai grid) to take random
   shots on player 1's grid. *)
let rec ai_choose_shot lvl prob grid turn =
  try
    let x_coord, y_coord =
      if lvl = 4 then gen_random_coords_with_ship 10 grid
      else if lvl = 2 || lvl = 3 then choose_informed_shot lvl prob grid
      else if lvl = 2 && Random.float 1.0 < prob then
        gen_random_coords_with_ship 10 grid
      else gen_random_coordinates 10 grid
    in
    let enemy_grid = update_hit x_coord y_coord grid in
    let cell = get_cell x_coord y_coord 0 enemy_grid in
    Printf.printf "\nThe AI chose a shot at (%i, %i)\n" x_coord y_coord;
    let is_ship_hit = Battleship.Cell.is_ship_hit_or_empty cell in
    print_string "The AI";
    Battleship.Cell.ship_hit_print_msg (get_cell x_coord y_coord 0 enemy_grid);
    if turn = 1 then player1_sees := update_hit x_coord y_coord !player1_sees
    else player2_sees := update_hit x_coord y_coord !player2_sees;

    if turn = 1 then (
      print_grid true !player1_sees;
      print_endline "AI's grid";
      if see_other_grid then print_grid false enemy_grid)
    else (
      print_grid true !player2_sees;
      print_endline "Player 1's grid";
      if see_other_grid then print_grid false enemy_grid);

    if is_ship_hit then enemy_grid else grid
  with Failure _ ->
    print_endline "Please choose a valid coordinate";
    ai_choose_shot lvl prob grid turn

(** [player1_turn] prompts Player 1 for coordinates and updates Player 2's grid.
    Player 2's grid is checked to see if all ships are hit, and ends the game
    with Player 1 as winner if all of Player 2's ships are hit *)
let player1_turn () =
  print_endline "\nPlayer 1, take a shot";
  let updated_grid = player_choose_shot !player2_grid 1 in
  player2_grid := updated_grid;

  let won = player_won !player2_grid in
  (* print_endline ("Player 1 won: " ^ string_of_bool won); *)
  if won then print_endline "🎉You Won!!!!! Congrats Player 1!🎉" else ()

(** [player2_turn] prompts Player 1 for coordinates and updates Player 2's grid.
    Player 2's grid is checked to see if all ships are hit, and ends the game
    with Player 1 as winner if all of Player 2's ships are hit *)
let player2_turn () =
  if ai_mode then print_endline "\nAI, take a shot"
  else print_endline "\nPlayer 2, take a shot";
  let updated_grid =
    let chance = if debug_mode then 1.0 else 0.15 in
    if ai_mode then ai_choose_shot difficulty_lvl chance !player1_grid 2
    else player_choose_shot !player1_grid 2
  in
  player1_grid := updated_grid;

  let won = player_won !player1_grid in
  (* print_endline ("Player 2 won: " ^ string_of_bool won); *)
  if won then
    if ai_mode then print_endline "Sorry the AI won. Better luck next time!"
    else print_endline "🎉You Won!!!!! Congrats Player 2!🎉"

(** [main] lets player 1 and then player 2 take turns shooting each other until
    someone wins the game *)
let main () =
  let rec game_loop current_player () =
    if player_won !player1_grid || player_won !player2_grid then ()
    else begin
      let next_player =
        if current_player = 1 then begin
          player1_turn ();
          2
        end
        else begin
          player2_turn ();
          1
        end
      in
      game_loop next_player ()
    end
  in
  game_loop 1 ()

let () = main ()
