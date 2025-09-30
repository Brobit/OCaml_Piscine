let rec iter (f : int -> int) (x : int) (n : int) : int =
  if n < 0 then -1
  else if n = 0 then x
  else iter f (f x) (n - 1)
