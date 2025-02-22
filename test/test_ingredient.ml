open Recipe_backend

let test_make_ingredient_title () =
  let ingredient = Ingredient.make "1 tsp sugar" |> Result.get_ok in
  Alcotest.(check string) "has the correct title" ingredient.name "sugar"

let test_make_ingredient_amount () =
  let ingredient = Ingredient.make "1 tsp sugar" |> Result.get_ok in
  Alcotest.(check string) "has the correct amount" (Ingredient.to_string ingredient.amount) "1 tsp"

let test_make_ingredient_amounttype () =
  let ingredient = Ingredient.make "1 tsp sugar" |> Result.get_ok in
  match ingredient.amount with
  | Ingredient.Imperial i -> (
      match i with
      | Teaspoon y -> Alcotest.(check int) "has the correct amount" y 1
      | _ -> Alcotest.fail "Should not be anyting but teaspoon")
  | _ -> Alcotest.fail "Should not be metric"

let test_make_ingredient_amounttype () =
  let ingredient = Ingredient.make "1 tsp sugar" |> Result.get_ok in
  match ingredient.amount with
  | Ingredient.Imperial i -> (
      match i with
      | Teaspoon y -> Alcotest.(check int) "has the correct amount" y 1
      | _ -> Alcotest.fail "Should not be anyting but teaspoon")
  | _ -> Alcotest.fail "Should not be metric"

(* Run it *)
let () =
  let open Alcotest in
  run __FILE__
    [
      ( "ingredient make",
        [
          test_case "Ingredient title" `Quick test_make_ingredient_title;
          test_case "Ingredient amount" `Quick test_make_ingredient_amount;
          test_case "Check that the type is set correctly for the amount" `Quick test_make_ingredient_amounttype;
        ] );
    ]
