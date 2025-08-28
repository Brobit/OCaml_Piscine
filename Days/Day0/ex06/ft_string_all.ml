let ft_string_all (f : char -> bool) (s : string) : bool =
    let rec loop i =
        if i >= String.length s then true
        else if f (String.get s i) then loop (i + 1)
        else false
    in
    loop 0