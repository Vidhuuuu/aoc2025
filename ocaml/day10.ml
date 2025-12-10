type machine = {
  indicator_lights : int array;
  buttons : int list list;
  joltages : int array;
}
[@@deriving show]

let parse_line l =
  let rbrack_idx = String.index l ']' in
  let lbrace_idx = String.index l '{' in
  let indicator_lights =
    let s = String.sub l 1 (rbrack_idx - 1) in
    Array.init (String.length s) (String.get s)
    |> Array.map (fun c -> if c = '.' then 0 else 1)
  in
  let buttons =
    String.sub l (rbrack_idx + 2) (lbrace_idx - (rbrack_idx + 3))
    |> String.split_on_char ' '
    |> List.map (fun p ->
        let p' = String.sub p 1 (String.length p - 2) in
        let nums = String.split_on_char ',' p' in
        List.map int_of_string nums)
  in
  let joltages =
    String.sub l (lbrace_idx + 1) (String.length l - (lbrace_idx + 2))
    |> String.split_on_char ',' |> List.map int_of_string |> Array.of_list
  in
  { indicator_lights; buttons; joltages }

(* part 1 *)
module StateSet = Set.Make (struct
  type t = int array

  let compare = compare
end)

let configure_indicator_lights machine =
  let press_button state button =
    let new_state = Array.copy state in
    List.iter
      (fun idx ->
        if new_state.(idx) = 0 then new_state.(idx) <- 1
        else new_state.(idx) <- 0)
      button;
    new_state
  in
  let rec bfs queue visited =
    match queue with
    | [] -> -1
    | (state, steps) :: rest ->
        if state = machine.indicator_lights then steps
        else
          let next_states =
            machine.buttons
            |> List.map (press_button state)
            |> List.filter (fun s -> not (StateSet.mem s !visited))
          in
          List.iter (fun s -> visited := StateSet.add s !visited) next_states;
          let new_queue =
            rest @ List.map (fun s -> (s, steps + 1)) next_states
          in
          bfs new_queue visited
  in
  let start = Array.make (Array.length machine.indicator_lights) 0 in
  let visited = ref (StateSet.singleton start) in
  bfs [ (start, 0) ] visited

(* part 2 *)
let configure_joltages machine =
  let n = Array.length machine.joltages in
  let k = List.length machine.buttons in
  let button_vectors = Array.make_matrix n k 0 in
  let rec fill col_idx = function
    | [] -> ()
    | b :: bs ->
        let rec go_over_indices = function
          | [] -> ()
          | i :: is ->
              button_vectors.(i).(col_idx) <- 1;
              go_over_indices is
        in
        go_over_indices b;
        fill (col_idx + 1) bs
  in
  fill 0 machine.buttons;
  let target = Array.map float_of_int machine.joltages in

  let prob = Glpk.new_problem () in
  Glpk.set_direction prob Minimize;
  Glpk.set_message_level prob 0;

  Glpk.add_rows prob n;
  for i = 0 to n - 1 do
    Glpk.set_row_bounds prob i Glpk.Fixed_var target.(i) target.(i)
  done;

  Glpk.add_columns prob k;
  for j = 0 to k - 1 do
    Glpk.set_col_kind prob j Glpk.Integer_var;
    Glpk.set_col_bounds prob j Glpk.Lower_bounded_var 0.0 0.0;
    Glpk.set_obj_coef prob j 1.0
  done;

  let entries = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to k - 1 do
      let coef = float_of_int button_vectors.(i).(j) in
      if coef <> 0.0 then entries := ((i, j), coef) :: !entries
    done
  done;

  let entries_arr = Array.of_list (List.rev !entries) in
  Glpk.load_sparse_matrix prob entries_arr;

  Glpk.use_presolver prob true;

  Glpk.simplex prob;
  Glpk.branch_and_bound prob;

  int_of_float (Glpk.get_obj_val prob)

let () =
  (* let file_name = "../input/day10/sample" in *)
  let file_name = "../input/day10/real" in
  let machines =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some line -> loop (parse_line line :: acc)
        in
        loop [])
  in
  let total_min_steps =
    List.map configure_indicator_lights machines |> List.fold_left ( + ) 0
  in
  Printf.printf "part 1: %d\n" total_min_steps;
  let total_min_steps2 =
    List.map configure_joltages machines |> List.fold_left ( + ) 0
  in
  Printf.printf "part 2: %d\n" total_min_steps2
