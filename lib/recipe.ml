type recipe = {
  name : string;
  ingredients : Ingredient.ingredient list;
  instructions : string list;
}

let make name ingredients instructions =
  Some { name; ingredients; instructions }
