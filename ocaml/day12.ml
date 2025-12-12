type region = { width : int; height : int; requirements : int list }
[@@deriving show]

let count_fit_regions regions presents =
  let rec aux acc = function
    | [] -> acc
    | r :: rs ->
        let region_area = r.width * r.height in
        let rec sum_present_areas i acc = function
          | [] -> acc
          | x :: xs -> sum_present_areas (i + 1) (acc + (x * presents.(i))) xs
        in
        let total_required_area = sum_present_areas 0 0 r.requirements in
        if total_required_area <= region_area then aux (acc + 1) rs
        else aux acc rs
  in
  aux 0 regions

let () =
  (* let file_name = "../input/day12/sample" in *)
  let file_name = "../input/day12/real" in
  let presents = Array.make 6 0 in
  let regions =
    In_channel.with_open_text file_name (fun ic ->
        let rec loop regions =
          match In_channel.input_line ic with
          | None -> List.rev regions
          | Some line when String.contains line 'x' ->
              let x_idx = String.index line 'x' in
              let dims, rest =
                match String.split_on_char ':' line with
                | [ a; b ] -> (a, b)
                | _ -> failwith "what"
              in
              let width, height =
                ( int_of_string @@ String.sub dims 0 x_idx,
                  int_of_string
                  @@ String.sub dims (x_idx + 1)
                       (String.length dims - (x_idx + 1)) )
              in
              let requirements =
                rest |> String.split_on_char ' '
                |> List.filter (fun x -> x <> "")
                |> List.map int_of_string
              in
              loop ({ width; height; requirements } :: regions)
          | Some line when String.contains line ':' ->
              let index =
                int_of_string @@ String.sub line 0 (String.index line ':')
              in
              let rec read_present acc =
                match In_channel.input_line ic with
                | None | Some "" -> List.rev acc
                | Some l -> read_present (l :: acc)
              in
              let present = read_present [] in
              let size =
                present
                |> List.concat_map (fun row ->
                    row |> String.to_seq
                    |> Seq.filter (fun x -> x = '#')
                    |> List.of_seq)
                |> List.length
              in
              presents.(index) <- size;
              loop regions
          | Some _ -> loop regions
        in
        loop [])
  in
  (* List.iter (fun r -> Printf.printf "%s\n\n" (show_region r)) regions; *)
  (* Array.iteri (fun i p -> Printf.printf "index: %d, size: %d\n\n" i p) presents; *)
  Printf.printf "part 1: %d\n" (count_fit_regions regions presents)
