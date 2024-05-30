open Cell

type row_t = cell_t list

let rec create_row size x y : row_t =
  if size > 0 then
    if x >= 0 && y >= 0 then create_cell x y :: create_row (size - 1) (x + 1) y
    else raise (Invalid_argument "Coordinates must be non-negative")
  else if size = 0 then
    if x >= 0 && y >= 0 then []
    else raise (Invalid_argument "Coordinates must be non-negative")
  else raise (Invalid_argument "Size must be non-negative")
(*Add main.ml to catch the Invalid_argument.*)

let rec print_row ships_hidden (row : cell_t list) =
  let func = if ships_hidden then print_cell_hidden else print_cell in
  match row with
  | [] -> ""
  | cell :: row_t -> func cell ^ print_row ships_hidden row_t

let rec get_cell_row row x =
  if x < 0 then raise (Invalid_argument "Coordinates must be non-negative")
  else
    match row with
    | [] -> failwith "empty row"
    | cell :: t -> if cell.x = x then cell else get_cell_row t x
