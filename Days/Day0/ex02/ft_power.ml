let rec ft_power (x: int) (y: int) : int = 
  if y = 0 then 1 else x * ft_power x (y - 1)