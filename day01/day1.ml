open Core

type direction = N | E | S | W
type state = { pos : int * int; heading : direction }

module PosSet = Set.Make (struct
  type t = int * int [@@deriving sexp]

  let compare (x1, y1) (x2, y2) =
    let x_cmp = Int.compare x1 x2 in
    if x_cmp = 0 then Int.compare y1 y2 else x_cmp
end)

let initial_state = { pos = (0, 0); heading = N }

let turn_right = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N

let turn_left = function
  | N -> W
  | W -> S
  | S -> E
  | E -> N

let delta = function
  | N -> (0, 1)
  | E -> (1, 0)
  | S -> (0, -1)
  | W -> (-1, 0)

let parse instruction =
  ( instruction.[0],
    String.sub ~pos:1 ~len:(String.length instruction - 1) instruction
    |> Int.of_string )

let execute_instruction { pos = x, y; heading } (turn, walk) =
  let heading =
    match turn with
    | 'R' -> turn_right heading
    | 'L' -> turn_left heading
    | _ -> heading
  in
  let delta_x, delta_y = delta heading in
  let pos = (x + (walk * delta_x), y + (walk * delta_y)) in
  { pos; heading }

let distance_from_origin (x, y) = abs x + abs y

let generate_visited_positions (old_x, old_y) (x, y) =
  let generate_range start end_ =
    let original_start = start in
    let start, end_ = if start < end_ then (start, end_) else (end_, start) in
    Sequence.unfold_step ~init:start ~f:(fun v ->
        Sequence.Step.(
          if v > end_ then Done
          else if v = original_start then Skip (v + 1)
          else Yield (v, v + 1)))
  in
  if old_x = x then generate_range old_y y |> Sequence.map ~f:(fun y -> (x, y))
  else generate_range old_x x |> Sequence.map ~f:(fun x -> (x, y))

let find_first_repeat =
  let rec find_first_repeat' visited ({ pos; _ } as state) = function
    | [] -> None
    | instruction :: tl ->
        let ({ pos = new_pos; _ } as state) =
          execute_instruction state instruction
        in
        visit_all pos new_pos visited state tl
  and visit_all old_pos new_pos visited state remaining_instructions =
    let rec visit_all' visited = function
      | [] -> find_first_repeat' visited state remaining_instructions
      | pos :: _ when PosSet.mem visited pos -> Some pos
      | pos :: tl -> visit_all' (PosSet.add visited pos) tl
    in
    let to_visit = generate_visited_positions old_pos new_pos in
    visit_all' visited @@ Sequence.to_list to_visit
  in
  find_first_repeat' PosSet.empty initial_state

let part1 instructions =
  instructions
  |> List.fold_left ~init:initial_state ~f:execute_instruction
  |> fun { pos; _ } ->
  pos |> distance_from_origin |> Printf.printf "Part 1: %d\n"

let part2 instructions =
  instructions
  |> find_first_repeat
  |> Option.map ~f:distance_from_origin
  |> Option.iter ~f:(Printf.printf "Part 2: %d\n")

let () =
  let instructions =
    In_channel.stdin
    |> In_channel.input_line_exn
    |> String.split ~on:','
    |> Sequence.of_list
    |> Sequence.map ~f:String.strip
    |> Sequence.map ~f:parse
    |> Sequence.to_list
  in
  part1 instructions;
  part2 instructions
