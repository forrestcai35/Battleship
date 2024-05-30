open Cell
open Row

type grid_t = row_t list
(** Row major representation of grid of cells *)

val create_grid : int -> int -> int -> row_t list
(** [create_grid] creates a grid of [row] rows and [cols] columns *)

val print_grid : bool -> grid_t -> unit
(** [print_grid] prints a grid to terminal depending on if a player wants to see
    the ships or just the hits *)

val within_grid : int -> int -> grid_t -> bool
(* [within_grid] returns true/false based on whether or not the coordinates
   (x,y) are within the grid *)

val add_ship : int -> int -> grid_t -> grid_t
(** [add_ship] updates [occupied] field in the cell at coordinates x,y *)

val update_hit : int -> int -> grid_t -> grid_t
(** [update_hit] updates the [hit] field in the cell at coordinates (x,y) *)

val get_cell : int -> int -> int -> grid_t -> cell_t
(* [get_cell] returns the cell at coordinates (x,y) *)

val gen_random_coordinates : int -> grid_t -> int * int
(** [gen_random_coordinates] generates random coordinates within a grid *)

val gen_random_coords_with_ship : int -> grid_t -> int * int
(** [gen_random_coords_with_ship] generates random coordinates of a ship that
    has not been hit *)
