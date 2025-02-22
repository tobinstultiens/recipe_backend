open Common.String_helper

type metric = Gram of int | Mililiter of int | KiloGram of int
type imperial = Teaspoon of int | Tablespoon of int | Ounce of int | FluidOunce of int | Unit of int
type amount_of_ingredient = Metric of metric | Imperial of imperial
type ingredient = { name : string; amount : amount_of_ingredient; calories : string option }

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

let pattern = Re.Perl.compile_pat "^([0-9¼½¾]*)\\s+(tsp|cups|cup|etc)?\\s?(.*)$"

let parse_imperial quantity unit ing =
  match unit with
  | t when contains t "tsp" -> Ok { name = ing; amount = Imperial (Teaspoon quantity); calories = None }
  | t when contains t "tbsp" -> Ok { name = ing; amount = Imperial (Teaspoon quantity); calories = None }
  | t when contains t "oz" -> Ok { name = ing; amount = Imperial (Teaspoon quantity); calories = None }
  | t when contains t "fl. oz" -> Ok { name = ing; amount = Imperial (Teaspoon quantity); calories = None }
  | _ -> Ok { name = ing; amount = Imperial (Unit quantity); calories = None }

let parse_metric quantity unit ing =
  match (quantity, unit) with
  | Ok t, uni when contains uni "gr" -> Ok { name = ing; amount = Metric (Gram t); calories = None }
  | Ok t, uni when contains uni "ml" -> Ok { name = ing; amount = Metric (Mililiter t); calories = None }
  | Ok t, runi when contains runi "kg" -> Ok { name = ing; amount = Metric (KiloGram t); calories = None }
  | Ok t, _ -> parse_imperial t unit ing
  | Error r, _ -> Error r

let parse_ingredient line =
  match Re.exec_opt pattern line with
  | Some groups ->
      let quantity =
        Re.Group.get groups 1 |> int_of_string_opt |> Option.to_result ~none:"Failed to retrieve quantity"
      in
      let unit = Re.Group.get groups 2 in
      let ingredient = Re.Group.get groups 3 in
      parse_metric quantity unit ingredient
  | None -> Error "Could not parse the ingredient"

(** Parse an ingredient text into the ingredient type *)
let make ingredient = parse_ingredient ingredient

(** Converts recipe values to string format *)
let to_string = function Metric m -> to_string_metric m | Imperial i -> to_string_imperial i
