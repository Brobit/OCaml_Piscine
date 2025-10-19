let sequence (n : int) : string =
  let rec int_to_string num =
    if num = 0 then "0"
    else if num < 10 then 
      let c = char_of_int (48 + num) in
      let rec char_to_str ch = 
        match ch with
        | '0' -> "0" | '1' -> "1" | '2' -> "2" | '3' -> "3" | '4' -> "4"
        | '5' -> "5" | '6' -> "6" | '7' -> "7" | '8' -> "8" | '9' -> "9"
        | _ -> ""
      in
      char_to_str c
    else int_to_string (num / 10) ^ int_to_string (num mod 10)
  in
  
  let char_to_string c = 
    match c with
    | '0' -> "0" | '1' -> "1" | '2' -> "2" | '3' -> "3" | '4' -> "4"
    | '5' -> "5" | '6' -> "6" | '7' -> "7" | '8' -> "8" | '9' -> "9"
    | _ -> ""
  in
  
  let rec string_length s =
    try
      ignore (s.[0]);
      1 + string_length (let len = ref 0 in
                        try while true do ignore (s.[!len]); incr len done; ""
                        with Invalid_argument _ -> 
                          if !len <= 1 then "" 
                          else (let rec build_sub i acc =
                                  if i >= !len then acc else s.[i] :: build_sub (i+1) acc
                                in
                                let chars = build_sub 1 [] in
                                let rec chars_to_string lst =
                                  match lst with
                                  | [] -> ""
                                  | h :: t -> char_to_string h ^ chars_to_string t
                                in
                                chars_to_string (List.rev chars)))
    with Invalid_argument _ -> 0
  in
  
  let convert_to_str s =
    if s = "" then "1"
    else
      let str_len = string_length s in
      let rec count_consecutive first_char str pos =
        if pos >= str_len then 0
        else
          try
            if s.[pos] = first_char then 1 + count_consecutive first_char str (pos + 1)
            else 0
          with Invalid_argument _ -> 0
      in
      
      let rec get_substring str start len =
        if len <= 0 then ""
        else
          let rec build_chars i acc =
            if i >= start + len then acc
            else
              try s.[i] :: build_chars (i + 1) acc
              with Invalid_argument _ -> acc
          in
          let chars = List.rev (build_chars start []) in
          let rec chars_to_string lst =
            match lst with
            | [] -> ""
            | h :: t -> char_to_string h ^ chars_to_string t
          in
          chars_to_string chars
      in
      
      let rec count_and_build remaining result =
        if remaining = "" then result
        else
          let first_char = remaining.[0] in
          let count = count_consecutive first_char remaining 0 in
          let count_str = int_to_string count in
          let char_str = char_to_string first_char in
          let new_result = result ^ count_str ^ char_str in
          let remaining_length = string_length remaining in
          let new_remaining = 
            if count >= remaining_length then ""
            else get_substring remaining count (remaining_length - count)
          in
          count_and_build new_remaining new_result
      in
      count_and_build s ""
  in
  
  let rec calc n =
    if n = 1 then "1"
    else 
      let res = calc (n - 1) in
      convert_to_str res
  in
  
  if n <= 0 then ""
  else calc n




(*let sequence (n : int) : string =
  let convert_to_str s =
    if s = "" then "1"
    else
      let rec count_and_build remaining result =
        if remaining = "" then result
        else
          let first_char = remaining.[0] in
          let count = count_consecutive first_char remaining in
          let new result
      in
      count_and_build s ""
  in
  let rec calc n =
    if n = 1 then "1"
    else 
      let res = calc (n - 1) in
      convert_to_str res
  in
  if n <= 0 then ""
  else calc n *)