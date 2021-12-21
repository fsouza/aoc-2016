open Core

let line_re = Str.regexp {| +|}

let parse line =
  match Str.split line_re line with
  | [ x; y; z ] -> Some (int_of_string x, int_of_string y, int_of_string z)
  | _ -> None

let is_valid_triangle (x, y, z) = x + y > z && x + z > y && y + z > x

let count_valid_triangles list =
  list |> List.filter ~f:is_valid_triangle |> List.length

let part1 lines =
  lines
  |> List.filter_map ~f:parse
  |> count_valid_triangles
  |> Printf.printf "Part 1: %d\n"

let part2 lines =
  lines
  |> List.fold_left
       ~init:(([], [], []), [])
       ~f:(fun (wip, triangles) line ->
         match Str.split line_re line with
         | [ v1; v2; v3 ] -> (
             let v1 = int_of_string v1 in
             let v2 = int_of_string v2 in
             let v3 = int_of_string v3 in
             match wip with
             | [ t1_y; t1_z ], [ t2_y; t2_z ], [ t3_y; t3_z ] ->
                 ( ([], [], []),
                   (v1, t1_y, t1_z)
                   :: (v2, t2_y, t2_z)
                   :: (v3, t3_y, t3_z)
                   :: triangles )
             | t1, t2, t3 -> ((v1 :: t1, v2 :: t2, v3 :: t3), triangles))
         | _ -> (wip, triangles))
  |> snd
  |> count_valid_triangles
  |> Printf.printf "Part 2: %d\n"

let () =
  let lines = In_channel.input_lines In_channel.stdin in
  part1 lines;
  part2 lines
