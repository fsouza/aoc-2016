open Core

let delta = function
  | 'U' -> -3
  | 'R' -> 1
  | 'D' -> 3
  | 'L' -> -1
  | _ -> 0

let can_move pos = function
  | 'R' -> pos <> 3 && pos <> 6
  | 'L' -> pos <> 4 && pos <> 7
  | _ -> true

let rec move pos = function
  | [] -> pos
  | inst :: tl ->
      let delta = delta inst in
      let new_pos = pos + delta in
      if new_pos > 0 && new_pos < 10 && can_move pos inst then move new_pos tl
      else move pos tl

let () =
  In_channel.stdin
  |> In_channel.fold_lines ~init:(5, []) ~f:(fun (pos, digits) line ->
         let line = String.to_list line in
         let pos = move pos line in
         (pos, pos :: digits))
  |> snd
  |> List.rev
  |> List.iter ~f:(Printf.printf "%d");
  Out_channel.newline stdout
