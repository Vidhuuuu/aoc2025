(* part 1 *)
let count_available_ids ids ranges =
  let rec in_range id = function
    | [] -> false
    | (lb, ub) :: tl -> if id >= lb && id <= ub then true else in_range id tl
  in
  let rec aux count = function
    | [] -> count
    | hd :: tl ->
        if in_range hd ranges then aux (count + 1) tl else aux count tl
  in
  aux 0 ids

(* part 2 *)
let count_fresh_ids ranges =
  let sorted_ranges =
    List.sort (fun (lb1, _) (lb2, _) -> Int.compare lb1 lb2) ranges
  in
  let rec aux curr_lb curr_ub acc = function
    | [] -> (curr_lb, curr_ub) :: acc
    | (lb, ub) :: tl ->
        if lb <= curr_ub then aux curr_lb (max curr_ub ub) acc tl
        else aux lb ub ((curr_lb, curr_ub) :: acc) tl
  in
  let first_lb, first_ub = List.hd sorted_ranges in
  aux first_lb first_ub [] (List.tl sorted_ranges)
  |> List.fold_left (fun acc (lb, ub) -> acc + (ub - lb + 1)) 0

let () =
  (* let file_name = "./input/day5/sample" in *)
  let file_name = "./input/day5/real" in
  let ranges, ids =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop ranges ids =
          match In_channel.input_line ic with
          | None -> (ranges, ids)
          | Some line ->
              if String.contains line '-' then
                match String.split_on_char '-' line with
                | [ lb_str; ub_str ] ->
                    loop
                      ((int_of_string lb_str, int_of_string ub_str) :: ranges)
                      ids
                | _ -> failwith "what"
              else if line = "" then loop ranges ids
              else loop ranges (int_of_string line :: ids)
        in
        loop [] [])
  in
  let available_ids = count_available_ids ids ranges in
  Printf.printf "part 1: %d\n" available_ids;
  Printf.printf "part 2: %d\n" (count_fresh_ids ranges)
