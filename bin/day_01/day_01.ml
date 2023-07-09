let part_one input = 
  let rec calculate input accum =
    match input with
    | [] -> accum
    | x::xs when x = "" -> 
        let next_accum = calculate xs 0 in
        if accum > next_accum then accum else next_accum
    | x::xs -> 
        let value = (int_of_string x) in 
        calculate xs (value + accum) in 
  calculate input 0

let part_two input number_of_elves = 
  let rec sums input accum =
    match input with
    | [] -> [accum]
    | x::xs when x = "" -> 
        let next_accums = sums xs 0 in
        accum :: next_accums 
    | x::xs -> 
        let value = (int_of_string x) in 
        sums xs (value + accum)
  in 
  let rec take n l =
    match l with
    | x::xs when n > 0 -> x:: take(n-1) xs
    | _ -> []
  in
  let all_sums = sums input 0 in
  all_sums |> List.fast_sort compare |> List.rev |> (take number_of_elves) |> (List.fold_left (+) 0)

let () = 
  assert (part_one (Advent.get_test_input 1) = 24000);
  print_endline (string_of_int (part_one (Advent.get_input 1)));
  print_endline (string_of_int (part_two (Advent.get_input 1) 3))
