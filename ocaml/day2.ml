(* part 1 *)
let sum_invalid (lb, ub) =
  let rec aux acc curr =
    if curr > ub then acc
    else
      let str_curr = string_of_int curr in
      let curr_len = String.length str_curr in
      if curr_len mod 2 = 1 then aux acc (curr + 1)
      else
        let first_part = String.sub str_curr 0 (curr_len / 2)
        and second_part = String.sub str_curr (curr_len / 2) (curr_len / 2) in
        (* Printf.printf "first: %s, second: %s\n" first_part second_part; *)
        if first_part = second_part then aux (acc + curr) (curr + 1)
        else aux acc (curr + 1)
  in
  aux 0 lb

(* part 2 *)
let is_repeated s =
  let len = String.length s in
  if len <= 1 then false
  else
    let ss = s ^ s in
    let rec search i =
      if i >= len then false
      else if String.sub ss i len = s then true
      else search (i + 1)
    in
    search 1

let sum_invalid2 (lb, ub) =
  let rec aux acc curr =
    if curr > ub then acc
    else
      let str_curr = string_of_int curr in
      if is_repeated str_curr then aux (acc + curr) (curr + 1)
      else aux acc (curr + 1)
  in
  aux 0 lb

let () =
  (* let file_name = "../input/day2/sample" in *)
  let file_name = "../input/day2/real" in
  let ranges =
    In_channel.with_open_text file_name (fun ic ->
        match In_channel.input_line ic with
        | None -> failwith "what"
        | Some line ->
            let range_list = String.split_on_char ',' line in
            List.map
              (fun str_range ->
                match String.split_on_char '-' str_range with
                | [ lb; ub ] -> (int_of_string lb, int_of_string ub)
                | _ -> failwith "what")
              range_list)
  in
  let sums = List.map sum_invalid2 ranges in
  let total_sum = List.fold_left ( + ) 0 sums in
  Printf.printf "part 2: %d\n" total_sum
