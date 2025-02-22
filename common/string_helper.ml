let contains s1 s2 =
  let re = Re.Str.regexp_string s2 in
  try
    ignore (Re.Str.search_forward re s1 0);
    true
  with Not_found -> false
