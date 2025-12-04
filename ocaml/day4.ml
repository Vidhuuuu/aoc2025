(* part 1 *)
let is_accessible grid x y =
  let directions =
    [ (-1, 0); (-1, 1); (0, 1); (1, 1); (1, 0); (1, -1); (0, -1); (-1, -1) ]
  and dim = Array.length grid in
  let rec aux acc = function
    | [] -> acc
    | (dx, dy) :: tl ->
        let nx = x + dx and ny = y + dy in
        if
          nx >= 0 && nx < dim && ny >= 0 && ny < dim
          && Char.equal grid.(nx).(ny) '@'
        then aux (acc + 1) tl
        else aux acc tl
  in
  aux 0 directions < 4

let count_rolls_of_paper grid =
  let count = ref 0 in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid - 1 do
      if Char.equal grid.(i).(j) '@' && is_accessible grid i j then
        (* Printf.printf "valid: %d, %d\n" i j; *)
        count := !count + 1
    done
  done;
  count

(* part 2 *)
let count_rolls_of_paper2 grid =
  let total_count = ref 0 in
  let flag = ref true in
  while !flag do
    let count = ref 0 in
    for i = 0 to Array.length grid - 1 do
      for j = 0 to Array.length grid - 1 do
        if Char.equal grid.(i).(j) '@' && is_accessible grid i j then (
          grid.(i).(j) <- '.';
          count := !count + 1)
      done
    done;
    if !count = 0 then flag := false;
    total_count := !total_count + !count
  done;
  total_count

let print_grid grid =
  let dim = Array.length grid in
  for i = 0 to dim - 1 do
    for j = 0 to dim - 1 do
      Printf.printf "%c " grid.(i).(j)
    done;
    print_newline ()
  done

let () =
  (* let is_sample = true in *)
  let is_sample = false in
  let file_name, dim =
    if is_sample then ("./input/day4/sample", 10) else ("./input/day4/real", 136)
  in
  let grid = Array.make_matrix dim dim '$' in
  In_channel.with_open_text file_name (fun ic ->
      let rec loop row_idx =
        match In_channel.input_line ic with
        | None -> ()
        | Some line ->
            let chars = Array.init (String.length line) (String.get line) in
            let rec set_cols col_idx =
              if col_idx >= dim then ()
              else (
                grid.(row_idx).(col_idx) <- chars.(col_idx);
                set_cols (col_idx + 1))
            in
            set_cols 0;
            loop (row_idx + 1)
      in
      loop 0);
  Printf.printf "part 1: %d\n" !(count_rolls_of_paper grid);
  Printf.printf "part 2: %d\n" !(count_rolls_of_paper2 grid)
