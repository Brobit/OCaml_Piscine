let rec crossover lst1 lst2 =
  let rec contains elem lst =
    match lst with
    | [] -> false
    | h :: t -> if h = elem then true else contains elem t
  in
  match lst1 with
  | [] -> []
  | h :: t ->
    if contains h lst2 then h :: crossover t lst2
    else crossover t lst2