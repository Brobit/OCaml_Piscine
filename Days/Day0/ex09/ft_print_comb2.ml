let ft_print_comb2 unit : unit =
  let print i j =
      if i < 10 then print_char '0';
      print_int i; print_char ' '; 
      if j < 10 then print_char '0';
      print_int j;
      print_string (if i = 98 && j = 99 then "\n" else ", " )
  in
  let rec loop i j =
    print i j;
    if not (i = 98 && j = 99) then (
      if j < 99 then loop i (j + 1)
      else if i < 98 then loop (i + 1) (i + 2)
    )
  in
  loop 0 1