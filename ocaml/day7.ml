let get_start grid =
  let start_y =
    match Array.find_index (fun c -> c = 'S') grid.(0) with
    | None -> failwith "what"
    | Some y -> y
  in
  (0, start_y)

let print_grid grid =
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      Printf.printf "%c " grid.(i).(j)
    done;
    print_newline ()
  done

let copy_grid grid =
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let new_grid = Array.make_matrix nrows ncols '$' in
  for i = 0 to nrows - 1 do
    for j = 0 to ncols - 1 do
      new_grid.(i).(j) <- grid.(i).(j)
    done
  done;
  new_grid

(* part 1 *)
let count_splits grid =
  let start_x, start_y = get_start grid in
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let hit = Array.make_matrix nrows ncols false in
  let rec aux x y =
    if x + 1 >= nrows then 0
    else
      match grid.(x + 1).(y) with
      | '.' ->
          grid.(x + 1).(y) <- '|';
          aux (x + 1) y
      | '^' ->
          let count = ref 0 in
          if not hit.(x + 1).(y) then (
            hit.(x + 1).(y) <- true;
            count := !count + 1);

          if y - 1 >= 0 then count := !count + aux (x + 1) (y - 1);
          if y + 1 < ncols then count := !count + aux (x + 1) (y + 1);

          !count
      | _ -> 0
  in
  aux start_x start_y

(* part 2 *)
let count_timelines grid =
  let start_x, start_y = get_start grid in
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let memo = Array.make_matrix nrows ncols (-1) in
  let rec aux x y =
    if x + 1 >= nrows then 1
    else if memo.(x).(y) <> -1 then memo.(x).(y)
    else
      let total =
        match grid.(x + 1).(y) with
        | '.' ->
            grid.(x + 1).(y) <- '|';
            aux (x + 1) y
        | '^' ->
            let left = if y - 1 >= 0 then aux (x + 1) (y - 1) else 0 in
            let right = if y + 1 < ncols then aux (x + 1) (y + 1) else 0 in
            left + right
        | _ -> 0
      in
      memo.(x).(y) <- total;
      total
  in
  aux start_x start_y

let () =
  (* let file_name = "../input/day7/sample" in *)
  let file_name = "../input/day7/real" in
  let grid_as_lists =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some line ->
              let chars = List.init (String.length line) (String.get line) in
              loop (chars :: acc)
        in
        loop [])
  in
  let grid = Array.of_list (List.map Array.of_list grid_as_lists) in
  let copy = copy_grid grid in
  Printf.printf "part 1: %d\n" (count_splits grid);
  (* print_grid grid; *)
  Printf.printf "part 2: %d\n" (count_timelines copy)
