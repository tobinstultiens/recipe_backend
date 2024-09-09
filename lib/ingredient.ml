open Re

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

(* Define the regex pattern *)
let pattern =
  Re.Pcre.regexp
    "\\(\\d+\\)\\s*\\(gr\\|gram\\|fl\\.\\s*oz\\|\\w*\\)\\s*\\(.*\\)"

(* Function to extract values *)
let extract_values str =
  match Re.Pcre.exec ~rex:pattern str with
  | exception Not_found -> ("", "", "")
  | result ->
      let value = Re.Pcre.get_substring result 1 in
      let unit = Re.Pcre.get_substring result 2 in
      let text = Re.Pcre.get_substring result 3 in
      (value, unit, text)

let process_ingredient x = extract_values x

let make ingredient =
  let value, unit, text = process_ingredient ingredient in
  Some { name = text; amount = value }
