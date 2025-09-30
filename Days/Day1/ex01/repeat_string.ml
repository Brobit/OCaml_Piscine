let rec repeat_string ?(str = "x") (n: int) : string =
  if n < 0 then "Error"
  else if n = 0 then ""
  else str ^ repeat_string ~str (n - 1)