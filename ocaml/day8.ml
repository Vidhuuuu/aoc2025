type box = { x : float; y : float; z : float }

module DSU = struct
  type t = { parent : int array; size : int array }

  let make n = { parent = Array.init n (fun x -> x); size = Array.make n 1 }

  let rec find dsu x =
    let p = dsu.parent.(x) in
    if p = x then x
    else
      let root = find dsu p in
      dsu.parent.(x) <- root;
      root

  let union dsu a b =
    let root_a = find dsu a in
    let root_b = find dsu b in
    if root_a = root_b then false
    else (
      if dsu.size.(root_a) < dsu.size.(root_b) then (
        dsu.parent.(root_a) <- root_b;
        dsu.size.(root_b) <- dsu.size.(root_b) + dsu.size.(root_a))
      else (
        dsu.parent.(root_b) <- root_a;
        dsu.size.(root_a) <- dsu.size.(root_a) + dsu.size.(root_b));
      true)
end

let distance b1 b2 =
  let diff_squared =
    ((b1.x -. b2.x) ** 2.) +. ((b1.y -. b2.y) ** 2.) +. ((b1.z -. b2.z) ** 2.)
  in
  sqrt diff_squared

let get_distances points =
  let n = Array.length points in
  let distances = ref [] in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let d = distance points.(i) points.(j) in
      (* distances := (points.(i), points.(j), d) :: !distances *)
      distances := (i, j, d) :: !distances
    done
  done;
  Array.of_list !distances

(* part 1 *)
let get_largest_circuits boxes distances =
  let dsu = DSU.make (Array.length boxes) in
  let rec aux i =
    (* if i > 10 then () *)
    if i > 1000 then ()
    else
      let f, s, _ = distances.(i) in
      DSU.union dsu f s |> ignore;
      aux (i + 1)
  in
  aux 1;
  Array.sort (fun a b -> compare b a) dsu.size;
  let rec take arr acc i =
    if i = 3 then acc else take arr (arr.(i) :: acc) (i + 1)
  in
  take dsu.size [] 0

(* part 2 *)
let get_final_connection boxes distances =
  let distances = Array.to_list distances in
  let n = Array.length boxes in
  let dsu = DSU.make n in
  let rec aux ncomponents = function
    | [] -> (-1., -1.)
    | (f, s, _) :: tl ->
        if DSU.union dsu f s then
          let after_ncomponents = ncomponents - 1 in
          if after_ncomponents = 1 then (boxes.(f).x, boxes.(s).x)
          else aux after_ncomponents tl
        else aux ncomponents tl
  in
  aux n distances

let () =
  (* let file_name = "../input/day8/sample" in *)
  let file_name = "../input/day8/real" in
  let boxes_as_list =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some line -> (
              match String.split_on_char ',' line with
              | [ x; y; z ] ->
                  let p =
                    {
                      x = float_of_string x;
                      y = float_of_string y;
                      z = float_of_string z;
                    }
                  in
                  loop (p :: acc)
              | _ -> failwith "what")
        in
        loop [])
  in
  let boxes = Array.of_list boxes_as_list in
  let distances = get_distances boxes in
  Array.sort (fun (_, _, d1) (_, _, d2) -> compare d1 d2) distances;
  let largest_prod =
    List.fold_left ( * ) 1 (get_largest_circuits boxes distances)
  in
  Printf.printf "part 1: %d\n" largest_prod;
  let x1, x2 = get_final_connection boxes distances in
  Printf.printf "part 2: %d\n" (int_of_float (x1 *. x2))
