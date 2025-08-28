let ft_is_palindrome (s : string) : bool =
  let rec loop i j =
    if i = j || i = j - 1 then true
    else if (String.get s i) <> (String.get s j) then false
    else loop (i + 1) (j - 1)
  in
  loop 0 (if String.length s - 1 > 0 then String.length s - 1 else 0)