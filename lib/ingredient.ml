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

(** This contains the regex format to retrieve the measurements and name values from an ingredient string *)
let regex_format =
  Re.compile
    (Re.Posix.re "(?P<measure>\\d+(?:\\.\\d+)?(?: [^ ]+)?)?(?P<name>.+)")

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let classify_measurement = function
  | s when contains s "oz" -> Ounce (int_of_string s)

(** Parse the ingredient text *)
let parse_ingredients text =
  let processed_groups = Re.all regex_format text in
  let group1 =
    processed_groups |> List.map (fun line -> Group.get_opt line 1) |> List.hd
  in
  let group2 =
    processed_groups |> List.map (fun line -> Group.get_opt line 2) |> List.hd
  in
  let ingredients = { name = group2 |> Option.get; amount = group1 } in
  ingredients

(* let parse_ingredients text = *)
(*   (* let substrings = Re.exec re text in *) *)
(*   let ingredients = *)
(*     Re.all re text *)
(*     |> List.filter_map (fun line -> *)
(*            match line.(1) with *)
(*            | Some (_, _, measure, name) -> *)
(*                Some *)
(*                  { *)
(*                    measurement = *)
(*                      (if String.is_empty measure then None else Some measure); *)
(*                    name = *)
(*                      String.trim (Str.cuts (Str.regexp_delim "▢" "") name).hd; *)
(*                  } *)
(*            | None -> None) *)
(*   in *)
(*   ingredients *)
(**)
let process_ingredient x = parse_ingredients x

let make ingredient =
  let value = ingredient |> List.map (fun t -> process_ingredient t) in
  Some { name = text; amount = value }
