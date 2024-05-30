open Grid

(* ship_type is the abstraction of ships to be used in logic *)
type ship_type =
  | Aircraft_carrier
  | Battleship
  | Cruiser
  | Submarine
  | Destroyer

(* [player_place_ships grid placed_ships] lets players choose the ship they want
   to place and ensures it is a valid ship and is finished when all ships have
   been placed

   The function takes in an accumlated list of ships that have been placed
   already and if all ships have been placed(length of [ships_placed] is 5),
   then it returns the grid, if not then it asks to continually place ships by
   calling [matching_inputted_ship] in a try-failure block. If it fails (the
   player does not input a correct ship), it recursively calls
   [player_place_ships] with a message saying it wasn't a valid ship. Otherwise,
   the ship that was matched was add to the grid and added to placed_ships. *)
val player_place_ships : grid_t -> ship_type list -> grid_t

(* [player_won grid] takes in a grid and returns true if all ships have been hit
   on the grid *)
val player_won : grid_t -> bool

(* [check_valid_ship_vert len x y grid] checks that both the top and bottom of
   the ship are within the grid and it does not overlap with any other ships. *)
val check_valid_ship_vert : int -> int -> int -> grid_t -> bool

(* [check_valid_ship_horiz len x y grid] checks that both the left and right of
   the ship are within the grid and it does not overlap with any other ships. *)
val check_valid_ship_horiz : int -> int -> int -> grid_t -> bool

(* [place_ships_even len vertical x y grid] takes an even length ship and sees
   if a ships is being placed vertically. If being placed vertically, then it
   adds ships vertically if not, horizontally

   There are only 2 cases here: length 2 and length 4, it always adds a ship to
   the left or above (depending on whether its placed vertically or
   horizontally). Then if its length 4 it adds 2 more ships to the right or
   below (again based on verticality) *)
val place_ships_even : int -> bool -> int -> int -> grid_t -> grid_t

(* [place_ships_odd len vertical x y grid] takes an odd length ship and sees if
   a ships is being placed vertically. If being placed vertically, then it adds
   ships vertically if not, horizontally

   There are only 2 cases here: length 3 and length 5, it always adds a ship to
   the left/above and a ship below/right (depending on whether its placed
   vertically or horizontally). Then if its length 5 it adds another ship to the
   left/above and below/rigt below (again based on verticality) *)
val place_ships_odd : int -> bool -> int -> int -> grid_t -> grid_t

(* [ships_overlap x y grid] takes in an xy coordinate of a grid, and checks if a
   ship is at that location, if so return true else false*)
val ships_overlap : int -> int -> grid_t -> bool

(* [ship_len ship_type] matches the abstract [ship_type] to its length *)
val ship_len : ship_type -> int
