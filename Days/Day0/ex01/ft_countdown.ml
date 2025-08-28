let rec ft_countdown (number: int) : unit =
  print_int (if number > 0 then number else 0);
  print_char('\n');
  if number > 0 then ft_countdown (number - 1)