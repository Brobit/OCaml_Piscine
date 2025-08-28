let ft_print_comb unit : unit =
  let print i j k =
      print_int i; print_int j; print_int k;
      print_string (if i = 7 && j = 8 && k = 9 then "\n" else ", " )
  in
  let rec loop i j k =
    print i j k;
    if not (i = 7 && j = 8 && k = 9) then (
      if k < 9 then loop i j (k + 1)
      else if j < 8 then loop i (j + 1) (k + 1)
      else loop (i + 1) (j + 2) (k + 3)
    )
  in
  loop 0 1 2