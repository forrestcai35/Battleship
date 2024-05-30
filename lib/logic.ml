open Grid

let debug_mode = false

let ships_overlap x y grid =
  let cell = Grid.get_cell x y 0 grid in
  List.fold_left
    (List.fold_left (fun acc grid_cell ->
         acc || Cell.cells_overlap cell grid_cell))
    false grid

(* issue here is that it only checks 2 below if len >= *)
let check_valid_ship_vert len x y grid =
  let check_top =
    if len <= 4 then
      within_grid x (y - 1) grid && not (ships_overlap x (y - 1) grid)
    else
      within_grid x (y - 2) grid
      && (not (ships_overlap x (y - 2) grid))
      && within_grid x (y - 1) grid
      && not (ships_overlap x (y - 1) grid)
  in
  let check_bot =
    if len = 2 then true
    else if len < 4 && len > 2 then
      within_grid x (y + 1) grid && not (ships_overlap x (y + 1) grid)
    else
      within_grid x (y + 2) grid
      && (not (ships_overlap x (y + 2) grid))
      && within_grid x (y + 1) grid
      && not (ships_overlap x (y + 1) grid)
  in
  if debug_mode then (
    print_endline ("Top: " ^ string_of_bool check_top);
    print_endline ("Bot: " ^ string_of_bool check_bot));

  check_top && check_bot

let check_valid_ship_horiz len x y grid =
  let check_left =
    if len <= 4 then
      within_grid (x - 1) y grid && not (ships_overlap (x - 1) y grid)
    else
      within_grid (x - 2) y grid
      && (not (ships_overlap (x - 2) y grid))
      && within_grid (x - 1) y grid
      && not (ships_overlap (x - 1) y grid)
  in
  let check_right =
    if len = 2 then true
    else if len = 3 then
      within_grid (x + 1) y grid && not (ships_overlap (x + 1) y grid)
    else
      within_grid (x + 2) y grid
      && (not (ships_overlap (x + 2) y grid))
      && within_grid (x + 1) y grid
      && not (ships_overlap (x + 1) y grid)
  in
  if debug_mode then (
    print_endline ("Left: " ^ string_of_bool check_left);
    print_endline ("Right: " ^ string_of_bool check_right));

  check_left && check_right

let rec vert () =
  print_string
    "Would you like to place it vertically or horizontally. Enter [v] or [h]: ";
  let input = read_line () in
  match input with
  | "v" -> true
  | "h" -> false
  | _ ->
      print_endline "You must enter [v] or [h]!";
      vert ()

let rec get_valid_x_coord () =
  print_string "Pick the x coordinate of your ship: ";
  try
    let x_coord = int_of_string (read_line ()) in
    if x_coord < 0 then failwith "negative x coord" else x_coord
  with Failure exn ->
    if debug_mode then print_endline ("Failure: " ^ exn);
    print_endline "That is not a valid x coordinate";
    get_valid_x_coord ()

let rec get_valid_y_coord () =
  print_string "Pick the y coordinate of your ship: ";
  try
    let y_coord = int_of_string (read_line ()) in
    if y_coord < 0 then failwith "Negative y coord" else y_coord
  with Failure exn ->
    if debug_mode then print_endline ("Failure: " ^ exn);
    print_endline "That is not a valid y coordinate";
    get_valid_y_coord ()

let place_ships_even len vertical x y grid =
  if vertical then
    let new_grid1 = add_ship x (y - 1) grid in
    if len = 4 then new_grid1 |> add_ship x (y + 1) |> add_ship x (y + 2)
    else new_grid1
  else
    (* horizontal *)
    let new_grid2 = add_ship (x - 1) y grid in
    if len = 4 then new_grid2 |> add_ship (x + 1) y |> add_ship (x + 2) y
    else new_grid2

let place_ships_odd len vertical x y grid =
  if vertical then
    let new_grid1 = grid |> add_ship x (y + 1) |> add_ship x (y - 1) in
    if len = 5 then new_grid1 |> add_ship x (y + 2) |> add_ship x (y - 2)
    else new_grid1
  else
    (* horizontal *)
    let new_grid2 = grid |> add_ship (x + 1) y |> add_ship (x - 1) y in
    if len = 5 then new_grid2 |> add_ship (x + 2) y |> add_ship (x - 2) y
    else new_grid2

(* [place_ship_on_grid len grid] gets valid x and y coordinates and ensures that
   the ship is fully within the grid. If it is then it calls the functions
   depending on the length [place_ships_even/odd]. If not valid, prints a
   message and calls [place_ship_on_grid] again*)
let rec place_ship_on_grid len grid =
  if len = 0 then grid
  else
    let x_coord = get_valid_x_coord () in

    let y_coord = get_valid_y_coord () in

    let vertical = vert () in
    let updated_grid = add_ship x_coord y_coord grid in

    let check_valid =
      (not (ships_overlap x_coord y_coord grid))
      &&
      if vertical then
        let check_vert = check_valid_ship_vert len x_coord y_coord grid in
        if debug_mode then (
          print_endline ("Vertical valid: " ^ string_of_bool check_vert);
          check_vert)
        else check_vert
      else
        let check_horiz = check_valid_ship_horiz len x_coord y_coord grid in
        if debug_mode then (
          print_endline ("Horizontal valid: " ^ string_of_bool check_horiz);
          check_horiz)
        else check_horiz
    in

    if check_valid then (
      if (* even length ships *)
         len mod 2 = 0 then (
        let new_grid =
          place_ships_even len vertical x_coord y_coord updated_grid
        in
        print_string "\n";
        print_grid false new_grid;
        new_grid)
      else
        (* odd length ships *)
        let new_grid =
          place_ships_odd len vertical x_coord y_coord updated_grid
        in
        print_string "\n";
        print_grid false new_grid;
        new_grid)
    else (
      print_endline
        "\n\
         The ships you place must be within the grid and must not overlap. \
         Please see the rules if you are unusre about how placement works";
      print_grid false grid;
      place_ship_on_grid len grid)

type ship_type =
  | Aircraft_carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer

let ship_len ship =
  match ship with
  | Aircraft_carrier -> 5
  | Battleship -> 4
  | Cruiser -> 3
  | Submarine -> 3
  | Destroyer -> 2

(* [matching_inputted_ship input placed_ships grid] places a ship based on
   [input] and whether or not it has already been placed.

   It takes in [input] of the type of ship it wants to place. If the ship has
   not been placed yet (not in [placed_sips]), it calls [place_ship_on_grid]. It
   returns a tuple of the updated_grid (where the ship was placed), and adds the
   ship type (that was just added to the grid) to [placed_ships].

   However, if no ship is matched, it throws a [Failure] (this failure is caught
   in [player_place_ship] and will give a message that it's not a valid ship)*)
let matching_inputted_ship input placed_ships (grid : grid_t) =
  if List.length placed_ships < 5 then begin
    match input with
    | "ac" ->
        if List.mem Aircraft_carrier placed_ships then (
          print_endline "Aircraft carrier already placed.";
          (grid, placed_ships))
        else
          let updated_grid =
            place_ship_on_grid (ship_len Aircraft_carrier) grid
          in
          (updated_grid, Aircraft_carrier :: placed_ships)
    | "bs" ->
        if List.mem Battleship placed_ships then (
          print_endline "Battleship already placed.";
          (grid, placed_ships))
        else
          let updated_grid = place_ship_on_grid (ship_len Battleship) grid in
          (updated_grid, Battleship :: placed_ships)
    | "cru" ->
        if List.mem Cruiser placed_ships then (
          print_endline "Cruiser already placed.";
          (grid, placed_ships))
        else begin
          let updated_grid = place_ship_on_grid (ship_len Cruiser) grid in
          (updated_grid, Cruiser :: placed_ships)
        end
    | "sub" ->
        if List.mem Submarine placed_ships then (
          print_endline "Submarine already placed.";
          (grid, placed_ships))
        else begin
          let updated_grid = place_ship_on_grid (ship_len Submarine) grid in
          (updated_grid, Submarine :: placed_ships)
        end
    | "dest" ->
        if List.mem Destroyer placed_ships then (
          print_endline "Destroyer already placed.";
          (grid, placed_ships))
        else begin
          let updated_grid = place_ship_on_grid (ship_len Destroyer) grid in
          (updated_grid, Destroyer :: placed_ships)
        end
    | _ -> failwith "Could not match a ship"
  end
  else (grid, placed_ships)

let rec player_place_ships grid ships_placed =
  if List.length ships_placed <> 5 then begin
    print_string
      "Input the type of ship you want to place [AC], [BS], [Cru], [Sub], \
       [Dest]: ";
    let ship = String.lowercase_ascii (read_line ()) in
    try
      let grid_and_placed_ships =
        matching_inputted_ship ship ships_placed grid
      in
      player_place_ships (fst grid_and_placed_ships) (snd grid_and_placed_ships)
    with Failure exn ->
      if debug_mode then print_endline ("Failure: " ^ exn);
      if exn = "empty grid" || exn = "empty row" then
        print_endline "That coordinate is not within the grid"
      else print_endline "That is not a valid ship";
      print_grid false grid;
      player_place_ships grid ships_placed
  end
  else grid

(* [ai_place_ships lst grid] breaks down the type of ships the ai needs to place
   and does wokr until all ships have been placed. Not unfinished. Need to fix
   place_ship_on_grid for the ai to randomly choose spots *)
(* let rec ai_place_ships ships_to_place grid = match ships_to_place with | []
   -> grid | h :: t -> let ship_len = ship_len h in let updated_grid =
   place_ship_on_grid ship_len grid in ai_place_ships t updated_grid *)

let player_won grid =
  List.fold_left
    (List.fold_left (fun acc ship -> acc && Cell.is_ship_hit_or_empty ship))
    true grid
