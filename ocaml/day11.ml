(* part 1 *)
module StringSet = Set.Make (String)

let count_paths graph src dst =
  let neighbors graph v =
    match List.assoc_opt v graph with None -> [] | Some ns -> ns
  in
  let visited = ref StringSet.empty in
  let rec dfs v =
    if v = dst then 1
    else (
      visited := StringSet.add v !visited;
      let total =
        neighbors graph v
        |> List.filter (fun x -> not (StringSet.mem x !visited))
        |> List.fold_left (fun acc n -> acc + dfs n) 0
      in
      visited := StringSet.remove v !visited;
      total)
  in
  dfs src

(* part 2 *)
module Memo = Map.Make (struct
  type t = string * bool * bool

  let compare = compare
end)

let count_paths2 graph src dst =
  let neighbors graph v =
    match List.assoc_opt v graph with None -> [] | Some ns -> ns
  in
  let memo = ref Memo.empty in
  let rec dfs v visited_dac visited_fft =
    let visited_dac = visited_dac || v = "dac" in
    let visited_fft = visited_fft || v = "fft" in
    if v = dst then if visited_dac && visited_fft then 1 else 0
    else
      match Memo.find_opt (v, visited_dac, visited_fft) !memo with
      | Some r -> r
      | None ->
          let total =
            neighbors graph v
            |> List.fold_left
                 (fun acc n -> acc + dfs n visited_dac visited_fft)
                 0
          in
          memo := Memo.add (v, visited_dac, visited_fft) total !memo;
          total
  in
  dfs src false false

let () =
  (* let file_name = "../input/day11/sample" in *)
  (* let file_name = "../input/day11/sample2" in *)
  let file_name = "../input/day11/real" in
  let graph =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some line -> (
              match String.split_on_char ':' line with
              | [ node; rest ] ->
                  let edges =
                    String.split_on_char ' ' rest
                    |> List.filter (fun x -> x <> "")
                  in
                  loop ((node, edges) :: acc)
              | _ -> failwith "what")
        in
        loop [])
  in
  let count = count_paths graph "you" "out" in
  Printf.printf "part 1: %d\n" count;
  let count2 = count_paths2 graph "svr" "out" in
  Printf.printf "part 2: %d\n" count2
