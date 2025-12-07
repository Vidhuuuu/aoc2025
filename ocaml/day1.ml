let parse_rotation (rotation : string) : char * int =
  ( rotation.[0],
    int_of_string (String.sub rotation 1 (String.length rotation - 1)) )

(* part 1 *)
let rotate (curr : int) (rot_type : char) (dist : int) : int =
  match rot_type with
  | 'L' -> (curr - (dist mod 100) + 100) mod 100
  | 'R' -> (curr + dist) mod 100
  | _ -> failwith "what"

(* part2 *)
let count_zeros (curr : int) (rot_type : char) (dist : int) : int =
  let first_zero =
    match rot_type with
    | 'L' -> curr mod 100
    | 'R' -> (100 - curr) mod 100
    | _ -> failwith "what"
  in
  let first_zero = if first_zero = 0 then 100 else first_zero in
  if dist < first_zero then 0 else 1 + ((dist - first_zero) / 100)

let () =
  (* let file_name = "../input/day1/sample" in *)
  let file_name = "../input/day1/real" in
  let count =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop curr count =
          match In_channel.input_line ic with
          | None -> count
          | Some rotation ->
              let rot_type, dist = parse_rotation rotation in
              let zeros = count_zeros curr rot_type dist in
              let new_dial = rotate curr rot_type dist in
              (* if new_dial = 0 then loop new_dial (count + 1) *)
              (* else loop new_dial count *)
              loop new_dial (count + zeros)
        in
        loop 50 0)
  in
  (* Printf.printf "part 1: %d\n" count *)
  Printf.printf "part 2: %d\n" count
