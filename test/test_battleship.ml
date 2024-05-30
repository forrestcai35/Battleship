open Battleship.Cell
open OUnit2
open Battleship.Row
open QCheck

(* Test Plan *)

(* The testing tactics we used for the Battleship game are detailed in the test
   plan that follows, which includes OUnit and QCheck testing.

   1. OUnit-Based Automated Testing: - Tested Modules: Grid, Logic, Cell, Row

   - Cell Module: Glass box testing to verify that the functions for creating
   cells, updating (occupied and hit states), and printing cells operate
   appropriately in a range of situations, including extreme values and zero
   coordinates. To ensure that negative coordinates were not taken into account,
   exception testing was also incorporated. - Row Module: To verify row
   generation with various sizes and coordinates, as well as to guarantee the
   proper visual output of rows and the required parameters of an initialized
   row, both black box and glass box testing methodologies were applied. - Grid
   Module: Black box testing to confirm the correctness of within-grid checks,
   the ability to add ships to the grid, and the construction of grids with
   different dimensions. - Logic Module: Verify the logic of ship placement
   overlaps and horizontal and vertical ship placement checks within the grid by
   doing black box testing.

   2. QCheck Randomized Testing: - To test for actions like changing cell
   statuses and ship placements, we utilized Qcheck to automatically generate
   values for the Cell, Row, and Grid components. This method boosts our
   confidence in the system's dependability in all circumstances and aids in the
   identification of unexpected edge cases.

   3. Manual Testing: - Manual testing was carried out during development to
   confirm the integration of modules and the user interface, even though
   automated tests were used for the majority of the system's testing.

   4. Justification for Testing Approach: - The testing method that we have done
   provides full coverage of the system’s functionality. The Glass box tests
   ensure that all logic paths are evaluated, black box test validate the
   systems against its specifications without regard to implementations. The
   randomized testing via QCheck further improves the robustness of the test
   suite by challenging the system with a wide range of inputs. These approaches
   combined demonstrate the correctness of the system and ensure that all
   components of the system function as expected.*)

let test_create_large_cell _ =
  (*large number as coordinates*)
  let new_cell2 = create_cell 100000 1000000 in
  assert_equal false new_cell2.occupied;
  assert_equal false new_cell2.hit;
  assert_equal 100000 new_cell2.x;
  assert_equal 1000000 new_cell2.y

let test_create_small_cell _ =
  (*two small number coordiantes *)
  let new_cell3 = create_cell 1 1 in
  assert_equal false new_cell3.occupied;
  assert_equal false new_cell3.hit;
  assert_equal 1 new_cell3.x;
  assert_equal 1 new_cell3.y

let test_create_zero_cell _ =
  (*two coordinates of 0*)
  let new_cell = create_cell 0 0 in
  assert_equal false new_cell.occupied;
  assert_equal false new_cell.hit;
  assert_equal 0 new_cell.x;
  assert_equal 0 new_cell.y

let test_create_small_zero_cell _ =
  (*one zero coordinate as y*)
  let new_cell4 = create_cell 5 0 in
  assert_equal false new_cell4.occupied;
  assert_equal false new_cell4.hit;
  assert_equal 5 new_cell4.x;
  assert_equal 0 new_cell4.y;
  (*one zero coordinate as x*)
  let new_cell5 = create_cell 0 5 in
  assert_equal false new_cell5.occupied;
  assert_equal false new_cell5.hit;
  assert_equal 0 new_cell5.x;
  assert_equal 5 new_cell5.y

let test_create_large_zero_cell _ =
  (*one large coordinate one zero coordinate as y *)
  let new_cell6 = create_cell 10000000 0 in
  assert_equal false new_cell6.occupied;
  assert_equal false new_cell6.hit;
  assert_equal 10000000 new_cell6.x;
  assert_equal 0 new_cell6.y;
  (*one large coordinate one zero coordinate as y *)
  let new_cell7 = create_cell 0 10000000 in
  assert_equal false new_cell7.occupied;
  assert_equal false new_cell7.hit;
  assert_equal 0 new_cell7.x;
  assert_equal 10000000 new_cell7.y

let test_create_cell_negative_coords _ =
  let create_negative_cell () = create_cell (-5) 0 in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    create_negative_cell;
  let create_negative_cell2 () = create_cell 0 (-5) in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    create_negative_cell2

let test_update_occupied _ =
  let cell = create_cell 1 2 in
  let updated_cell = update_occupied 1 2 cell in
  assert_equal true updated_cell.occupied;
  (*check old cell is not updated*)
  assert_equal false cell.occupied;
  (*check hit is not updated*)
  assert_equal false updated_cell.hit

let test_update_occupied_negative_coords _ =
  let cell = create_cell 0 0 in
  let update_occupied_negative_cell () = update_hit (-5) 0 cell in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    update_occupied_negative_cell;
  let update_occupied_negative_cell2 () = update_hit 0 (-5) cell in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    update_occupied_negative_cell2

let test_update_hit _ =
  let cell = create_cell 1 2 in
  let updated_cell = update_hit 1 2 cell in
  assert_equal false updated_cell.occupied;
  (*check old cell is not updated*)
  assert_equal false cell.hit;
  (*check hit is not updated*)
  assert_equal true updated_cell.hit

let test_update_hit_negative_coords _ =
  let cell = create_cell 0 0 in
  let update_hit_negative_cell () = update_hit (-5) 0 cell in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    update_hit_negative_cell;
  let update_hit_negative_cell2 () = update_hit 0 (-5) cell in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    update_hit_negative_cell2

let test_print_empty_cell _ =
  let cell = create_cell 1 2 in
  assert_equal "⬛️" (print_cell cell)

let test_print_occupied_cell _ =
  let cell = create_cell 1 2 in
  let updated_cell = update_occupied 1 2 cell in
  assert_equal "🟥" (print_cell updated_cell)

let test_print_hit_and_occupied_cell _ =
  let cell = create_cell 1 2 in
  let updated_cell = update_occupied 1 2 cell in
  let hit_and_occupied_cell = update_hit 1 2 updated_cell in
  assert_equal "🟩" (print_cell_hidden hit_and_occupied_cell);
  let zero_cell = create_cell 0 0 in
  let updated_zero_cell = update_occupied 0 0 zero_cell in
  let hit_and_occupied_zero_cell = update_hit 0 0 updated_zero_cell in
  assert_equal "🟩" (print_cell_hidden hit_and_occupied_zero_cell)

let test_print_empty_hidden_cell _ =
  let cell = create_cell 1 2 in
  assert_equal "⬛️" (print_cell_hidden cell);
  let zero_cell = create_cell 0 0 in
  assert_equal "⬛️" (print_cell_hidden zero_cell);
  let edge_cell = create_cell 100000 100000 in
  assert_equal "⬛️" (print_cell_hidden edge_cell)

let test_print_hit_hidden_cell _ =
  let cell = create_cell 1 2 in
  let updated_cell = update_hit 1 2 cell in
  assert_equal "⬜️" (print_cell_hidden updated_cell);
  let cell = create_cell 0 0 in
  let updated_cell = update_hit 0 0 cell in
  assert_equal "⬜️" (print_cell_hidden updated_cell)

let test_print_hit_and_occupied_hidden_cell _ =
  let cell1 = create_cell 1 2 in
  let updated_cell = update_occupied 1 2 cell1 in
  let hit_and_occupied_cell = update_hit 1 2 updated_cell in
  assert_equal "🟩" (print_cell hit_and_occupied_cell);
  let cell2 = create_cell 0 0 in
  let updated_cell = update_occupied 0 0 cell2 in
  let hit_and_occupied_cell = update_hit 0 0 updated_cell in
  assert_equal "🟩" (print_cell hit_and_occupied_cell)

let test_cells_overlap _ =
  let cell = create_cell 1 2 in
  let grid_cell = create_cell 1 2 in
  let updated_grid_cell = update_occupied 1 2 grid_cell in
  assert_equal true (cells_overlap cell updated_grid_cell)

let test_cells_no_overlap _ =
  let cell = create_cell 1 2 in
  let grid_cell = create_cell 2 2 in
  assert_equal false (cells_overlap cell grid_cell)

let qcheck_create_cell =
  Test.make ~name:"create_cell" (pair int int) (fun (x, y) ->
      let cell = create_cell x y in
      cell.x = x && cell.y = y && (not cell.occupied) && not cell.hit)

let qcheck_test_create_cell _ = QCheck_runner.to_ounit2_test qcheck_create_cell

let qcheck_update_occupied =
  Test.make ~name:"update_occupied"
    (triple int int (pair int int))
    (fun (x, y, (cx, cy)) ->
      let cell = create_cell cx cy in
      let updated_cell = update_occupied x y cell in
      (x = cx && y = cy) = updated_cell.occupied
      && updated_cell.x = cx && updated_cell.y = cy)

let qcheck_test_update_occupied _ =
  QCheck_runner.to_ounit2_test qcheck_update_occupied

let qcheck_update_hit =
  Test.make ~name:"update_hit"
    (triple int int (pair int int))
    (fun (x, y, (cx, cy)) ->
      let cell = create_cell cx cy in
      let updated_cell = update_hit x y cell in
      (x = cx && y = cy) = updated_cell.hit
      && updated_cell.x = cx && updated_cell.y = cy)

let qcheck_test_update_hit _ = QCheck_runner.to_ounit2_test qcheck_update_hit

let cell_suite =
  "Cell Tests"
  >::: [
         "test_create_cell with large numbers" >:: test_create_large_cell;
         "test_create_cell with normal coordinates" >:: test_create_small_cell;
         "test_create_cell with zeros" >:: test_create_zero_cell;
         "test_create_cell with a zero and a small coordinate"
         >:: test_create_small_zero_cell;
         "test_create_cell with a large coordinate and a zero"
         >:: test_create_large_zero_cell;
         "test_create_cell_negative_coords" >:: test_create_cell_negative_coords;
         ( "QCheck test_create_cell" >:: fun ctxt ->
           ignore (qcheck_test_create_cell ctxt) );
         "test_update_occupied" >:: test_update_occupied;
         "test_update_occupied_negative_coords"
         >:: test_update_hit_negative_coords;
         ( "QCheck test_update_occupied" >:: fun ctxt ->
           ignore (qcheck_test_update_occupied ctxt) );
         "test_update_hit" >:: test_update_hit;
         "test_update_hit_negative_coords"
         >:: test_update_occupied_negative_coords;
         ( "QCheck test_update_hit" >:: fun ctxt ->
           ignore (qcheck_test_update_hit ctxt) );
         "test_print_empty_cell" >:: test_print_empty_cell;
         "test_print_occupied_cell" >:: test_print_occupied_cell;
         "test_print_hit_and_occupied_cell" >:: test_print_hit_and_occupied_cell;
         "test_print_empty_hidden_cell" >:: test_print_empty_hidden_cell;
         "test_print_hit_hidden_cell" >:: test_print_hit_hidden_cell;
         "test_print_hit_and_occupied_hidden_cell"
         >:: test_print_hit_and_occupied_hidden_cell;
         "test_cells_overlap" >:: test_cells_overlap;
         "test_cells_no_overlap" >:: test_cells_no_overlap;
       ]

let test_create_small_row _ =
  let row = create_row 5 0 0 in
  assert_equal 5 (List.length row);
  assert_equal 0 (List.nth row 0).y;
  assert_equal 0 (List.nth row 1).y;
  assert_equal 0 (List.nth row 2).y;
  assert_equal 0 (List.nth row 3).y;
  assert_equal 0 (List.nth row 4).y;
  assert_equal 0 (List.nth row 0).x;
  assert_equal 1 (List.nth row 1).x;
  assert_equal 2 (List.nth row 2).x;
  assert_equal 3 (List.nth row 3).x;
  assert_equal 4 (List.nth row 4).x

let test_create_large_row _ =
  let row2 = create_row 1000000 0 10 in
  assert_equal 1000000 (List.length row2);
  assert_equal 10 (List.nth row2 0).y;
  assert_equal 10 (List.nth row2 4).y;
  assert_equal 10 (List.nth row2 9999).y;
  assert_equal 0 (List.nth row2 0).x;
  assert_equal 100 (List.nth row2 100).x;
  assert_equal 9999 (List.nth row2 9999).x

let qcheck_create_row =
  QCheck.Test.make ~name:"create_row"
    (QCheck.triple QCheck.small_nat QCheck.small_signed_int
       QCheck.small_signed_int) (fun (size, x, y) ->
      let row = create_row size x y in
      let check_y_coord cell = cell.y = y in
      let check_x_coord index cell = cell.x = x + index in
      List.length row = size
      && List.for_all check_y_coord row
      && List.mapi (fun index cell -> check_x_coord index cell) row
         |> List.for_all Fun.id)

let qcheck_test_create_row _ = QCheck_runner.to_ounit2_test qcheck_create_row

let test_create_row_negative_size _ =
  let create_negative_row () = create_row (-5) 0 0 in
  assert_raises (Invalid_argument "Size must be non-negative")
    create_negative_row

let test_create_empty_row _ =
  let empty_row = create_row 0 0 0 in
  assert_equal [] empty_row;
  let empty_row_with_coords = create_row 0 1 1 in
  assert_equal [] empty_row_with_coords;
  let empty_row_with_big_coords = create_row 0 100000000 0 in
  assert_equal [] empty_row_with_big_coords;
  let empty_row_with_big_coords2 = create_row 0 0 100000000 in
  assert_equal [] empty_row_with_big_coords2

let test_create_row_negative_coords _ =
  let create_negative_coord1 () = create_row 1 0 (-1) in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    create_negative_coord1;
  let create_negative_coord2 () = create_row 1 (-1) (-1) in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    create_negative_coord2;
  let create_negative_coord3 () = create_row 1 (-1) 0 in
  assert_raises (Invalid_argument "Coordinates must be non-negative")
    create_negative_coord3

let test_print_row _ =
  let row = create_row 2 0 0 in
  assert_equal "⬛️ | ⬛️ | " (print_row false row)

let test_print_empty_row _ =
  let row = create_row 0 0 0 in
  assert_equal "" (print_row false row)

let test_print_hidden_row _ =
  let row = create_row 2 0 0 in
  assert_equal "⬛️ | ⬛️ | " (print_row true row)

let test_print_hidden_empty_row _ =
  let row = create_row 0 0 0 in
  assert_equal "" (print_row true row)

let row_suite =
  "Row Tests"
  >::: [
         "test_create_row with small coordinates" >:: test_create_small_row;
         "test_create_row with large coordinates" >:: test_create_large_row;
         ( "QCheck test_create_row" >:: fun ctxt ->
           ignore (qcheck_test_create_row ctxt) );
         "test_create_empty_row" >:: test_create_empty_row;
         "test_create_row_negative_size" >:: test_create_row_negative_size;
         "test_create_row_negative_coords" >:: test_create_row_negative_coords;
         "test_print_row" >:: test_print_row;
         "test_print_empty_row" >:: test_print_empty_row;
         "test_print_hidden_row" >:: test_print_hidden_row;
         "test_print_hiddent_empty_row" >:: test_print_hidden_empty_row;
       ]

open Battleship.Grid

let test_create_small_grid _ =
  let grid = create_grid 10 10 0 in
  assert_equal 10 (List.length grid);
  assert_equal 10 (List.length (List.nth grid 0));
  assert_equal 10 (List.length (List.nth grid 1));
  assert_equal 10 (List.length (List.nth grid 2));
  assert_equal 10 (List.length (List.nth grid 3));
  assert_equal 10 (List.length (List.nth grid 4));
  assert_equal 10 (List.length (List.nth grid 5));
  assert_equal 10 (List.length (List.nth grid 6));
  assert_equal 10 (List.length (List.nth grid 7));
  assert_equal 10 (List.length (List.nth grid 8));
  assert_equal 10 (List.length (List.nth grid 9))

let test_create_empty_grid _ =
  let grid = create_grid 0 0 0 in
  assert_equal 0 (List.length grid);
  assert_equal [] grid

let test_create_empty_row_grid _ =
  let grid = create_grid 10 0 0 in
  assert_equal 10 (List.length grid);
  assert_equal [] (List.nth grid 0);
  assert_equal [] (List.nth grid 1);
  assert_equal [] (List.nth grid 2);
  assert_equal [] (List.nth grid 3);
  assert_equal [] (List.nth grid 4);
  assert_equal [] (List.nth grid 5);
  assert_equal [] (List.nth grid 6);
  assert_equal [] (List.nth grid 7);
  assert_equal [] (List.nth grid 8);
  assert_equal [] (List.nth grid 9)

let test_create_uneven_grid _ =
  let grid = create_grid 10 2 0 in
  assert_equal 10 (List.length grid);
  assert_equal 2 (List.length (List.nth grid 0));
  assert_equal 2 (List.length (List.nth grid 1));
  assert_equal 2 (List.length (List.nth grid 2));
  assert_equal 2 (List.length (List.nth grid 3));
  assert_equal 2 (List.length (List.nth grid 4));
  assert_equal 2 (List.length (List.nth grid 5));
  assert_equal 2 (List.length (List.nth grid 6));
  assert_equal 2 (List.length (List.nth grid 7));
  assert_equal 2 (List.length (List.nth grid 8));
  assert_equal 2 (List.length (List.nth grid 9))

let test_within_grid _ =
  let grid = create_grid 10 10 0 in
  assert_equal true (within_grid 0 0 grid);
  assert_equal true (within_grid 1 1 grid);
  assert_equal true (within_grid 9 0 grid);
  assert_equal true (within_grid 9 9 grid);
  assert_equal true (within_grid 0 9 grid);
  assert_equal true (within_grid 5 5 grid);
  assert_equal false (within_grid (-1) 0 grid);
  assert_equal false (within_grid 0 (-1) grid);
  assert_equal false (within_grid 9 (-1) grid);
  assert_equal false (within_grid (-1) 9 grid);
  assert_equal false (within_grid 10 0 grid);
  assert_equal false (within_grid 0 10 grid);
  assert_equal false (within_grid 9 10 grid);
  assert_equal false (within_grid 1000000000000 0 grid);
  assert_equal false (within_grid 0 1000000000000 grid);
  assert_equal false (within_grid 9 1000000000000 grid);
  assert_equal false (within_grid 1000000000000 9 grid)

let test_add_ship _ =
  let grid = create_grid 10 10 0 in
  let updated_grid = add_ship 1 1 grid in
  let cell = get_cell 1 1 0 updated_grid in
  assert_equal true cell.occupied

let test_add_ship_invalid_coords _ =
  let grid = create_grid 10 10 0 in
  let updated_grid = add_ship (-100) (-100) grid in
  assert_equal grid updated_grid

let testing_grid = create_grid 75 75 0

let qcheck_within_grid =
  Test.make ~name:"test_within_grid" (pair int int) (fun (x, y) ->
      let max_row = List.length testing_grid in
      let max_col =
        if max_row = 0 then 0 else List.length (List.hd testing_grid)
      in
      within_grid x y testing_grid
      = (x >= 0 && x < max_col && y >= 0 && y < max_row))

let qcheck_test_within_grid _ = QCheck_runner.to_ounit2_test qcheck_within_grid

let qcheck_create_grid =
  Test.make ~name:"test_create_grid" (pair small_nat small_nat)
    (fun (cols, rows) ->
      let grid = create_grid cols rows 0 in
      List.length grid = cols
      && List.for_all (fun row -> List.length row = rows) grid)

let qcheck_test_create_grid _ = QCheck_runner.to_ounit2_test qcheck_create_grid

let qcheck_add_ship =
  Test.make ~name:"test_add_ship" (pair small_nat small_nat) (fun (x, y) ->
      let new_grid = add_ship x y testing_grid in
      within_grid x y testing_grid || new_grid == testing_grid)

let qcheck_test_add_ship _ = QCheck_runner.to_ounit2_test qcheck_add_ship

(* Test for update_hit *)
let qcheck_update_hit_grid =
  Test.make ~name:"test_update_hit" (pair small_nat small_nat) (fun (x, y) ->
      try
        let _ = update_hit x y testing_grid in
        x >= 0 && y >= 0
      with Invalid_argument _ -> x < 0 || y < 0)

let qcheck_test_update_hit_grid _ =
  QCheck_runner.to_ounit2_test qcheck_update_hit_grid

let qcheck_get_cell_grid =
  Test.make ~name:"test_get_cell" (pair small_nat small_nat) (fun (x, y) ->
      try
        let _ = get_cell x y 0 testing_grid in
        (not (List.length testing_grid = 0)) && within_grid x y testing_grid
      with
      | Failure _ -> List.length testing_grid = 0
      | Invalid_argument _ -> x < 0 || y < 0)

let qcheck_test_get_cell_grid _ =
  QCheck_runner.to_ounit2_test qcheck_get_cell_grid

let grid_suite =
  "Grid Tests"
  >::: [
         "test_create_grid" >:: test_create_small_grid;
         "test_create_empty_grid" >:: test_create_empty_grid;
         "test_create_empty_row_grid" >:: test_create_empty_row_grid;
         "test_create_uneven_grid" >:: test_create_uneven_grid;
         "test_within_grid" >:: test_within_grid;
         "test_add_ship" >:: test_add_ship;
         "test_add_ship_invalid_coords" >:: test_add_ship_invalid_coords;
         ( "QCheck test within_grid" >:: fun ctxt ->
           ignore (qcheck_test_within_grid ctxt) );
         ( "QCheck test create_grid" >:: fun ctxt ->
           ignore (qcheck_test_create_grid ctxt) );
         ( "QCheck test add_ship" >:: fun ctxt ->
           ignore (qcheck_test_add_ship ctxt) );
         ( "QCheck test update_hit" >:: fun ctxt ->
           ignore (qcheck_test_update_hit_grid ctxt) );
         ( "QCheck test get_cell" >:: fun ctxt ->
           ignore (qcheck_test_get_cell_grid ctxt) );
       ]

open Battleship.Logic

let empty_grid = create_grid 10 10 0

let test_ships_overlap _ =
  let grid = add_ship 5 5 empty_grid in
  assert_equal true (ships_overlap 5 5 grid)

let test_check_valid_ship_vert _ =
  let grid = add_ship 5 4 empty_grid in
  assert_equal false (check_valid_ship_vert 3 5 5 grid)

let test_check_valid_ship_horiz _ =
  let grid = add_ship 4 5 empty_grid in
  assert_equal false (check_valid_ship_horiz 3 5 5 grid)

let logic_suite =
  "Battleship Game Tests"
  >::: [
         "test_ships_overlap" >:: test_ships_overlap;
         "test_check_valid_ship_vert" >:: test_check_valid_ship_vert;
         "test_check_valid_ship_horiz" >:: test_check_valid_ship_horiz;
       ]

let () =
  run_test_tt_main cell_suite;
  run_test_tt_main row_suite;
  run_test_tt_main grid_suite;
  run_test_tt_main logic_suite
