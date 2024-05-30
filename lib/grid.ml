open Cell
open Row

type grid_t = row_t list

let rec create_grid cols rows curr_col =
  if curr_col < cols then
    create_row rows 0 curr_col :: create_grid cols rows (curr_col + 1)
  else []

let print_column_headers num_columns =
  let headers = List.init num_columns string_of_int in
  let header_string = String.concat " " headers in
  print_endline ("  " ^ header_string);
  print_endline (" +" ^ String.make (2 * num_columns) '-' ^ "+")

let print_grid are_ships_hidden (grid : grid_t) =
  let num_columns = List.length (List.hd grid) in
  print_column_headers num_columns;
  List.iteri
    (fun i row -> Printf.printf "%d|%s|\n" i (print_row are_ships_hidden row))
    grid

let within_grid x y (grid : grid_t) =
  let max_row = List.length grid in
  if max_row = 0 then false
  else
    let max_col = List.length (List.hd grid) in
    x >= 0 && x < max_col && y >= 0 && y < max_row

let add_ship x y grid =
  if within_grid x y grid then
    List.map (List.map (fun cell -> update_occupied x y cell)) grid
  else grid

let update_hit x y grid =
  if x < 0 || y < 0 then
    raise (Invalid_argument "Coordinates must be non-negative")
  else List.map (List.map (fun cell -> update_hit x y cell)) grid

let rec get_cell x y cur_row (grid : grid_t) =
  if x < 0 || y < 0 then
    raise (Invalid_argument "Coordinates must be non-negative")
  else
    match grid with
    | [] -> failwith "empty grid"
    | row :: t ->
        if cur_row = y then get_cell_row row x else get_cell x y (cur_row + 1) t

let random_int min max = min + Random.int (max - min + 1)

let rec gen_random_coordinates grid_size grid =
  let x, y = (random_int 0 (grid_size - 1), random_int 0 (grid_size - 1)) in
  let cell = get_cell x y 0 grid in
  if not cell.hit then (x, y) else gen_random_coordinates grid_size grid

let rec gen_random_coords_with_ship grid_size grid =
  let x, y = gen_random_coordinates grid_size grid in
  let cell = get_cell x y 0 grid in
  if cell.occupied && not cell.hit then (x, y)
  else gen_random_coords_with_ship grid_size grid
