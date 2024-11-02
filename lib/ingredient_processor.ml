open Re

(** This contains the regex format to retrieve the measurements and name values from an ingredient string *)
let regex_format =
  Re.compile
    (Re.Posix.re "(?P<measure>\\d+(?:\\.\\d+)?(?: [^ ]+)?)?(?P<name>.+)")

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let classify_measurement_imperial = function
  | s when contains s "oz" -> Ounce (int_of_string s)
  | s when contains s "tbsp" -> Tablespoon (int_of_string s)
  | s when contains s "tsp" -> Teaspoon (int_of_string s)
  | s when contains s "fl. oz" -> FluidOunce (int_of_string s)
  | s when contains s "unit" -> Unit (int_of_string s)
  | _ -> raise Exit

let classify_measurement_metric = function
  | s when contains s "gr" -> Gram (int_of_string s)
  | s when contains s "kg" -> KiloGram (int_of_string s)
  | s when contains s "ml" -> Mililiter (int_of_string s)
  | _ -> raise Exit

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

let process_ingredient x = parse_ingredients x
