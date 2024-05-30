type cell_t = {
  occupied : bool;
  hit : bool;
  x : int;
  y : int;
}
(* The type of a cell *)

val update_occupied : int -> int -> cell_t -> cell_t
(** [update_occupied] updates the [occupied] field from false to true. Default
    value of [occupied] is false *)

val update_hit : int -> int -> cell_t -> cell_t
(** [update_hit] updates [hit] field from false to true. Default value of [hit]
    is false *)

val print_cell_debug : cell_t -> unit
(** [print_cell_debug] prints out the representation of a cell_t *)

val print_cell : cell_t -> string
(** [print_cell] is the representation of a player's own grid. Prints out "🟩" if
    [occupied] is true and if [hit] is true, otherwise, if [occupied] then
    prints out "🟥" prints "⬛️" otherwise *)

val print_cell_hidden : cell_t -> string
(** [print_cell] is the representation of what a player would see of the enemy's
    grid. Prints out "🟩" if [occupied] is true and if [hit] is true, otherwise,
    if [hit] then prints out "⬜" prints "⬛️" otherwise *)

val ship_hit_print_msg : cell_t -> unit
(* [ship_hit_print_msg] prints out a short message of whether a ship was hit *)

val is_ship_hit_or_empty : cell_t -> bool
(* [is_ship_hit_or_empty] returns true/false based on if a ship is in the cell
   and hit or the cell is empty *)

val create_cell : int -> int -> cell_t
(** [create_cell] takes an x and y coordinate and returns a cell that is
    unoccupied and not hit *)

val cells_overlap : cell_t -> cell_t -> bool
(* [cells_overlap] returns true/false based on if 2 cells are at the same
   coordinates and if they are occupied *)

val is_cell_occupied : cell_t -> bool

(* [is_cell_occupied] returns the [occupied] field in a cell *)
val is_cell_hit : cell_t -> bool
(* [is_cell_hit] returns the [hit] field in a cell *)
