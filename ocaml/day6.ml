(* part 1 *)
let solve_problems nums ops =
  let nrows = Array.length nums in
  let ncols = Array.length nums.(0) in
  let rec iter_cols i total =
    if i = ncols then total
    else
      let rec iter_rows j acc =
        if j = nrows then acc
        else iter_rows (j + 1) (int_of_string nums.(j).(i) :: acc)
      in
      let args = iter_rows 0 [] in
      let curr =
        match ops.(i) with
        | "+" -> List.fold_left ( + ) 0 args
        | "*" -> List.fold_left ( * ) 1 args
        | _ -> failwith "what"
      in
      iter_cols (i + 1) (total + curr)
  in
  iter_cols 0 0

(* part 2 *)
let split_by_width lines widths =
  let rec go_over_widths lines acc = function
    | [] -> List.rev acc
    | w :: ws ->
        let rec get_padded_args padded_args = function
          | [] -> List.rev padded_args
          | l :: ls -> get_padded_args (String.sub l 0 w :: padded_args) ls
        in
        let padded_args = get_padded_args [] lines in
        let chopped_lines =
          List.map
            (fun l ->
              let start = w + 1 in
              if String.length l <= start then ""
              else String.sub l start (String.length l - start))
            lines
        in
        go_over_widths chopped_lines (padded_args :: acc) ws
  in
  go_over_widths lines [] widths

let solve_problems2 problems ops =
  let rec aux total ops = function
    | [] -> total
    | p :: ps -> (
        let n = String.length (List.hd p) in
        let rec get_args args i =
          if i = n then args
          else
            let rec form_arg curr = function
              | [] -> curr
              | x :: xs ->
                  let ch = x.[i] in
                  if ch = ' ' then form_arg curr xs
                  else form_arg (curr ^ String.make 1 ch) xs
            in
            get_args (form_arg "" p :: args) (i + 1)
        in
        let args = List.map int_of_string (get_args [] 0) in
        match List.hd ops with
        | "+" ->
            let sol = List.fold_left ( + ) 0 args in
            aux (total + sol) (List.tl ops) ps
        | "*" ->
            let sol = List.fold_left ( * ) 1 args in
            aux (total + sol) (List.tl ops) ps
        | _ -> failwith "what")
  in
  aux 0 ops problems

let () =
  (* let file_name = "../input/day6/sample" in *)
  let file_name = "../input/day6/real" in
  let lines, nums_as_list, ops_as_list =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop lines nums ops =
          match In_channel.input_line ic with
          | None -> (List.rev lines, List.rev nums, ops)
          | Some line ->
              let parts =
                String.split_on_char ' ' line |> List.filter (fun p -> p <> "")
              in
              if List.exists (fun p -> p = "+") parts then loop lines nums parts
              (* else loop (List.map int_of_string parts :: nums) ops *)
                else loop (line :: lines) (parts :: nums) ops
        in
        loop [] [] [])
  in
  let nums = Array.of_list (List.map Array.of_list nums_as_list) in
  let ops = Array.of_list ops_as_list in
  Printf.printf "part 1: %d\n" (solve_problems nums ops);

  let rec iter_cols i acc =
    if i = Array.length nums.(0) then List.rev acc
    else
      let rec iter_rows j curr_max =
        if j = Array.length nums then curr_max
        else if String.length nums.(j).(i) > curr_max then
          iter_rows (j + 1) (String.length nums.(j).(i))
        else iter_rows (j + 1) curr_max
      in
      let curr_max = iter_rows 0 (-1) in
      iter_cols (i + 1) (curr_max :: acc)
  in
  let max_widths = iter_cols 0 [] in
  let args = split_by_width lines max_widths in
  Printf.printf "part 2: %d\n" (solve_problems2 args ops_as_list)
