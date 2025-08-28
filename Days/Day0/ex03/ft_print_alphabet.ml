let ft_print_alphabet unit : unit =
  let rec print_loop c =
    if c <= 'z' then (
      print_char c;
      print_loop (Char.chr (Char.code c + 1))
    )
  in
  print_loop 'a';
  print_char '\n'