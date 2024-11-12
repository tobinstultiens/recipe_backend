type metric = Gram of int | Mililiter of int | KiloGram of int

type imperial =
  | Teaspoon of int
  | Tablespoon of int
  | Ounce of int
  | FluidOunce of int
  | Unit of int

type amount_of_ingredient = Metric of metric | Imperial of imperial
type ingredient = { name : string; amount : amount_of_ingredient }

let to_string_imperial = function
  | Teaspoon v -> Printf.sprintf "%d" v ^ " tsp"
  | Tablespoon v -> Printf.sprintf "%d" v ^ " tbsp"
  | Ounce v -> Printf.sprintf "%d" v ^ " oz"
  | FluidOunce v -> Printf.sprintf "%d" v ^ " fl. oz"
  | Unit v -> Printf.sprintf "%d" v

let to_string_metric = function
  | Gram v -> Printf.sprintf "%d" v ^ " gr"
  | Mililiter v -> Printf.sprintf "%d" v ^ " ml"
  | KiloGram v -> Printf.sprintf "%d" v ^ " kg"

let to_string = function
  | Metric m -> to_string_metric m
  | Imperial i -> to_string_imperial i

let make name ingredient = Some { name; amount = ingredient }
