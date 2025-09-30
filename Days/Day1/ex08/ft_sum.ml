let rec ft_sum (f: int -> float) (x: int) (y: int) : float =
  if x = y then f x
  else if x > y then nan
  else f x +. ft_sum f (x + 1) y