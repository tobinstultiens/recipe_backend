type metric = Gram of int | Mililiter of int | KiloGram of int
type imperial = Teaspoon of int | Tablespoon of int | Ounce of int | FluidOunce of int | Unit of int
type amount_of_ingredient = Metric of metric | Imperial of imperial
type ingredient = { name : string; amount : amount_of_ingredient; calories : string option }

val make : string -> (ingredient, string) result
val to_string : amount_of_ingredient -> string
