open Cell

type row_t = cell_t list
(** The type for a row of cells *)

val create_row : int -> int -> int -> row_t
(** [create_row] creates a row of cells *)

val print_row : bool -> cell_t list -> string
(* [[print_row] Prints a row of cells depending on whether or not a player wants
   to see their own ships or only see whether their shots hit a ship *)

val get_cell_row : cell_t list -> int -> cell_t
(* [get_cell_row] Returns the cell from a given row at position [x]*)
