let ft_rot_n (n : int) (s : string) : string =
  let shift c =
    if c >= 'a' && c <= 'z' then 
      char_of_int (((int_of_char c - int_of_char 'a' + n) mod 26) + int_of_char 'a')
    else if c >= 'A' && c <= 'Z' then 
      char_of_int (((int_of_char c - int_of_char 'A' + n) mod 26) + int_of_char 'A')
    else c
  in
  String.map shift s