let rec encode list =
  let rec count_consecutive elem list count =
    match list with
    | [] -> (count, elem, [])
    | head :: tail when head = elem -> count_consecutive elem tail (count + 1)
    | _ -> (count, elem, list)
  in
  match list with
  | [] -> []
  | head :: tail ->
    let (count, element, remaining) = count_consecutive head tail 1 in
    (count, element) :: encode remaining