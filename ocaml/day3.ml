(* part 1 *)
let get_largest_joltage = function
  | [] | [ _ ] -> 0
  | hd :: tl ->
      let rec aux max_left curr = function
        | [] -> curr
        | x :: xs ->
            let new_curr = max curr ((10 * max_left) + x) in
            let max_left = max max_left x in
            aux max_left new_curr xs
      in
      aux hd 0 tl

(* part 2 *)
(* TIL: monotonic stacks *)
let get_largest_joltage2 bank =
  let to_remove = List.length bank - 12 in
  let rec aux stack removed = function
    | [] -> (stack, removed)
    | x :: xs ->
        let rec pop stack removed =
          match stack with
          | hd :: tl when removed > 0 && hd < x -> pop tl (removed - 1)
          | _ -> (stack, removed)
        in
        let popped_stack, new_removed = pop stack removed in
        aux (x :: popped_stack) new_removed xs
  in
  let stack, _ = aux [] to_remove bank in
  let rec take k lst =
    match (lst, k) with
    | _, 0 -> []
    | [], _ -> []
    | x :: xs, _ -> x :: take (k - 1) xs
  in
  take 12 (List.rev stack)

let make_number digits =
  let rec aux acc = function [] -> acc | d :: ds -> aux ((10 * acc) + d) ds in
  aux 0 digits

let () =
  (* let file_name = "../input/day3/sample" in *)
  let file_name = "../input/day3/real" in
  let banks =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          (* | Some bank -> loop (bank :: acc) *)
          | Some bank ->
              let joltages =
                List.of_seq (String.to_seq bank)
                |> List.map (fun c -> Char.code c - Char.code '0')
              in
              loop (joltages :: acc)
        in
        loop [])
  in
  Printf.printf "part 1: %d\npart 2: %d\n"
    (List.fold_left ( + ) 0 (List.map get_largest_joltage banks))
    (List.fold_left ( + ) 0
       (List.map make_number (List.map get_largest_joltage2 banks)))
