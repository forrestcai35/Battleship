type cell_t = {
  occupied : bool;
  hit : bool;
  x : int;
  y : int;
}

let update_occupied x y cell =
  if x < 0 || y < 0 then
    raise (Invalid_argument "Coordinates must be non-negative")
  else if cell.x = x && cell.y = y then { cell with occupied = true }
  else cell

let update_hit x y cell =
  if x < 0 || y < 0 then
    raise (Invalid_argument "Coordinates must be non-negative")
  else if cell.x = x && cell.y = y then { cell with hit = true }
  else cell

let print_cell_debug cell =
  print_endline
    ("cell coords : " ^ string_of_int cell.x ^ ", " ^ string_of_int cell.y
   ^ " | " ^ "occupied: "
    ^ string_of_bool cell.occupied
    ^ " | " ^ "hit : " ^ string_of_bool cell.hit)

let print_cell cell =
  if cell.occupied && cell.hit then "🟩" else if cell.occupied then "🟥" else "⬛️"

let print_cell_hidden cell =
  if cell.occupied && cell.hit then "🟩" else if cell.hit then "⬜️" else "⬛️"

let ship_hit_print_msg cell =
  if cell.occupied && cell.hit then
    print_endline
      (" hit a ship at " ^ string_of_int cell.x ^ ", " ^ string_of_int cell.y
     ^ "!")
  else print_endline " missed!"

let create_cell x_input y_input =
  if x_input < 0 || y_input < 0 then
    raise (Invalid_argument "Coordinates must be non-negative")
  else { occupied = false; hit = false; x = x_input; y = y_input }

let is_ship_hit_or_empty (cell : cell_t) : bool =
  (cell.occupied && cell.hit) || not cell.occupied

let is_cell_hit (cell : cell_t) : bool = cell.hit
let is_cell_occupied cell : bool = cell.occupied

let cells_overlap cell_1 grid_cell =
  if cell_1.x = grid_cell.x && cell_1.y = grid_cell.y then grid_cell.occupied
  else false
