open Core
module CharMap = Map.Make (Char)

let idx_of_direction = function
  | 'U' -> 0
  | 'R' -> 1
  | 'D' -> 2
  | 'L' -> 3
  | _ -> invalid_arg "unknown direction"

let allowed_moves_part1 =
  [
    ('1', [| None; Some '2'; Some '4'; None |]);
    ('2', [| None; Some '3'; Some '5'; Some '1' |]);
    ('3', [| None; None; Some '6'; Some '2' |]);
    ('4', [| Some '1'; Some '5'; Some '7'; None |]);
    ('5', [| Some '2'; Some '6'; Some '8'; Some '4' |]);
    ('6', [| Some '3'; None; Some '9'; Some '5' |]);
    ('7', [| Some '4'; Some '8'; None; None |]);
    ('8', [| Some '5'; Some '9'; None; Some '7' |]);
    ('9', [| Some '6'; None; None; Some '8' |]);
  ]
  |> CharMap.of_alist_exn

let allowed_moves_part2 =
  [
    ('1', [| None; None; Some '3'; None |]);
    ('2', [| None; Some '3'; Some '6'; None |]);
    ('3', [| Some '1'; Some '4'; Some '7'; Some '2' |]);
    ('4', [| None; None; Some '8'; Some '3' |]);
    ('5', [| None; Some '6'; None; None |]);
    ('6', [| Some '2'; Some '7'; Some 'A'; Some '5' |]);
    ('7', [| Some '3'; Some '8'; Some 'B'; Some '6' |]);
    ('8', [| Some '4'; Some '9'; Some 'C'; Some '7' |]);
    ('9', [| None; None; None; Some '8' |]);
    ('A', [| Some '6'; Some 'B'; None; None |]);
    ('B', [| Some '7'; Some 'C'; Some 'D'; Some 'A' |]);
    ('C', [| Some '8'; None; None; Some 'B' |]);
    ('D', [| Some 'B'; None; None; None |]);
  ]
  |> CharMap.of_alist_exn

let rec move allowed_moves pos = function
  | [] -> pos
  | inst :: tl ->
      let moves = CharMap.find_exn allowed_moves pos in
      let idx = idx_of_direction inst in
      let pos = moves.(idx) |> Option.value ~default:pos in
      move allowed_moves pos tl

let run allowed_map prefix lines =
  lines
  |> List.fold_left ~init:('5', []) ~f:(fun (pos, digits) line ->
         let line = String.to_list line in
         let pos = move allowed_map pos line in
         (pos, pos :: digits))
  |> snd
  |> List.rev
  |> String.of_char_list
  |> Printf.printf "%s: %s\n" prefix

let part1 = run allowed_moves_part1 "Part 1"
let part2 = run allowed_moves_part2 "Part 2"

let () =
  let lines = In_channel.stdin |> In_channel.input_lines in
  part1 lines;
  part2 lines
