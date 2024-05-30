(* val gen_random_coordinates : int -> int * int *)
(** [gen_random_coordinates grid_size] generates random coordinates within the
    grid bounds *)

val random_place_ships : int -> Grid.grid_t -> Grid.grid_t
(** [random_place_ships len grid] takes the length of the ship [len] and the
    current grid [grid] and places the ship at random coordinates. Whether the
    ship is vertical or horizontal is also randomized. *)

val generate_random_ship_list : unit -> Logic.ship_type list
(** [generate_random_ship_list n acc] returns a list of the 6 ship types [AC],
    [BS], [Cru], [Sub], [Dest] in a random order *)

val place_ship_on_grid : int -> Grid.grid_t -> Grid.grid_t
(** [place_ship_on_grid len grid] gets valid x and y coordinates and ensures
    that the ship is fully within the grid. If it is then it calls the functions
    depending on the length [place_ships_even/odd]. If not valid, prints a
    message and calls [place_ship_on_grid] again *)

val place_ship_on_grid_debug : int -> Grid.grid_t -> Grid.grid_t
(** [place_ship_on_grid len grid] gets valid x and y coordinates and ensures
    that the ship is fully within the grid. If it is then it calls the functions
    depending on the length [place_ships_even/odd]. If not valid, prints a
    message and calls [place_ship_on_grid] again *)

val ai_place_ships_v3 : Logic.ship_type list -> Grid.grid_t -> Grid.grid_t
(* [ai_place_ships lst grid] breaks down the type of ships the ai needs to place
   and does work until all ships have been placed. Not unfinished. Need to fix
   place_ship_on_grid for the ai to randomly choose spots *)
