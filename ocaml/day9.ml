type point = { x : int; y : int }
type rectangle = { p1 : point; p2 : point; area : int }

(* part 2 *)
let form_rectangles points =
  let rec outer acc = function
    | [] | [ _ ] -> List.rev acc
    | c :: cs ->
        let rec inner acc = function
          | [] -> List.rev acc
          | p :: rest ->
              let area = (abs (p.x - c.x) + 1) * (abs (p.y - c.y) + 1) in
              inner
                ({ p1 = { x = p.x; y = p.y }; p2 = { x = c.x; y = c.y }; area } :: acc)
                rest
        in
        let pairs = inner [] cs in
        outer (acc @ pairs) cs
  in
  outer [] points

let get_edges points =
  let get_edge_middle_points a b =
    let rec aux v i limit is_x acc =
      if i = limit then List.rev acc
      else if is_x then aux v (i + 1) limit is_x ({ x = v; y = i } :: acc)
      else aux v (i + 1) limit is_x ({ x = i; y = v } :: acc)
    in
    if a.x = b.x then aux a.x (min a.y b.y + 1) (max a.y b.y) true []
    else aux a.y (min a.x b.x + 1) (max a.x b.x) false []
  in

  let points_as_array = Array.of_list points in
  let n = Array.length points_as_array in
  let rec form_middle_point i acc =
    if i = n then List.rev acc
    else
      let a = points_as_array.(i) in
      let b = points_as_array.((i + 1) mod n) in
      let edge_middle_points = get_edge_middle_points a b in
      form_middle_point (i + 1) (edge_middle_points @ acc)
  in
  let middle_points = form_middle_point 0 [] in
  points @ middle_points

let largest_red_green rectangles edges =
  let edge_in_rectangle r e =
    e.x > min r.p1.x r.p2.x
    && e.x < max r.p1.x r.p2.x
    && e.y > min r.p1.y r.p2.y
    && e.y < max r.p1.y r.p2.y
  in
  let rec go_over_rectangles = function
    | [] -> failwith "what"
    | r :: rs ->
        let rec check_edges = function
          | [] -> true
          | e :: es -> if edge_in_rectangle r e then false else check_edges es
        in
        if check_edges edges then r.area else go_over_rectangles rs
  in
  go_over_rectangles rectangles

let () =
  (* let file_name = "../input/day9/sample" in *)
  let file_name = "../input/day9/real" in
  let points =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop acc =
          match In_channel.input_line ic with
          | None -> List.rev acc
          | Some line -> (
              match String.split_on_char ',' line with
              | [ x; y ] ->
                  let x_int = int_of_string x and y_int = int_of_string y in
                  loop ({ x = x_int; y = y_int } :: acc)
              | _ -> failwith "what")
        in
        loop [])
  in
  let rectangles = form_rectangles points in
  let sorted_rectangles =
    List.sort (fun a b -> compare b.area a.area) rectangles
  in
  (* part 1 *)
  let largest_rectangle = List.hd sorted_rectangles in
  Printf.printf "part 1: %d\n" largest_rectangle.area;
  let edges = get_edges points in
  Printf.printf "part 2: %d\n" (largest_red_green sorted_rectangles edges)
