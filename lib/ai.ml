open Logic
open Grid

(** TODO 1: ai places ships *)

let () = Random.self_init ()

(** [random_int min max] generates a random integer between the bounds [min] and
    [max] (inclusive) *)

(** [random_bool] generates a random boolean value *)
let random_bool () = Random.bool ()

let rec random_place_ships len grid =
  let grid_size = 10 in
  let is_vertical = random_bool () in
  let x, y = gen_random_coordinates grid_size grid in
  if is_vertical then begin
    if check_valid_ship_vert len x y grid then
      if len mod 2 = 0 then place_ships_even len true x y grid
      else place_ships_odd len true x y grid
    else random_place_ships len grid
  end
  else begin
    if check_valid_ship_horiz len x y grid then
      if len mod 2 = 0 then place_ships_even len false x y grid
      else place_ships_odd len false x y grid
    else random_place_ships len grid
  end

let ship_types = [ Aircraft_carrier; Battleship; Cruiser; Submarine; Destroyer ]

let generate_random_ship_list () =
  let shuffled_ships = List.sort (fun _ _ -> Random.int 3 - 1) ship_types in
  shuffled_ships

let rec place_ship_on_grid len grid =
  if len = 0 then grid
  else
    let x_coord, y_coord = gen_random_coordinates 10 grid in

    let vertical = random_bool () in
    let updated_grid = add_ship x_coord y_coord grid in

    let check_valid =
      (not (ships_overlap x_coord y_coord grid))
      &&
      if vertical then check_valid_ship_vert len x_coord y_coord grid
      else check_valid_ship_horiz len x_coord y_coord grid
    in
    if check_valid then
      if (* even length ships *)
         len mod 2 = 0 then
        let new_grid =
          place_ships_even len vertical x_coord y_coord updated_grid
        in
        new_grid
      else
        (* odd length ships *)
        let new_grid =
          place_ships_odd len vertical x_coord y_coord updated_grid
        in
        new_grid
    else place_ship_on_grid len grid

let rec place_ship_on_grid_debug len grid =
  if len = 0 then grid
  else
    let x_coord, y_coord = gen_random_coordinates 10 grid in
    print_endline
      ("Trying to place ship at coordinates: " ^ string_of_int x_coord ^ ", "
     ^ string_of_int y_coord);
    let vertical = random_bool () in
    let updated_grid = add_ship x_coord y_coord grid in

    let check_valid =
      (not (ships_overlap x_coord y_coord grid))
      &&
      if vertical then check_valid_ship_vert len x_coord y_coord grid
      else check_valid_ship_horiz len x_coord y_coord grid
    in
    if check_valid then (
      if len mod 2 = 0 then (
        let new_grid =
          place_ships_even len vertical x_coord y_coord updated_grid
        in
        print_endline "Successfully placed ship!";
        new_grid)
      else
        let new_grid =
          place_ships_odd len vertical x_coord y_coord updated_grid
        in
        print_endline "Successfully placed ship!";
        new_grid)
    else (
      print_endline "Failed to place ship. Trying again...";
      place_ship_on_grid_debug len grid)

let rec ai_place_ships_v3 ships_to_place_lst grid =
  match ships_to_place_lst with
  | [] -> grid
  | ship :: rest ->
      let ship_len = ship_len ship in
      let updated_grid = place_ship_on_grid ship_len grid in
      ai_place_ships_v3 rest updated_grid
