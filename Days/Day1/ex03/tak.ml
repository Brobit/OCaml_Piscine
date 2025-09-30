let rec tak (x: int) (y: int) (z: int) : int =
  if y < x then tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  else z