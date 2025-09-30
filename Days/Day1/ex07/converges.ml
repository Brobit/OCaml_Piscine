let rec converges (f: int -> int) (x: int) (n: int) : bool =
  if n < 0 then false
  else if f x = x then true
  else converges f (f x) (n - 1)