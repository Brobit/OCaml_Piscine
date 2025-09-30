let rec fibonacci (n: int) : int =
  if n < 0 then -1
  else if n = 0 || n = 1 then n
  else fibonacci (n - 2) + fibonacci (n - 1)