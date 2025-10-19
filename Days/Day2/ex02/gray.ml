let rec gray n =
  let reverse lst =
    let rec aux acc lst =
      match lst with
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in
    aux [] lst
  in
  let rec append lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | h :: t -> h :: append t lst2
  in
  let rec put_prefix c lst =
    match lst with
    | [] -> []
    | h :: t -> (c ^ h) :: put_prefix c t
  in
  let rec gen_lst n =
    if n = 0 then [""]
    else if n = 1 then ["0"; "1"]
    else
      let prev_node = gen_lst (n - 1) in
      let zero_added = put_prefix "0" prev_node in
      let one_added = put_prefix "1" (reverse prev_node) in
      append zero_added one_added
  in
  let rec print_lst lst =
    match lst with
    | [] -> ()
    | h :: t -> print_endline h; print_lst t
  in
  if n <= 0 then print_endline ""
  else
    let final_lst = gen_lst n in
    print_lst final_lst