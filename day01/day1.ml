open Core

type direction = N | E | S | W

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

type state = { pos : int * int; heading : direction }

let initial_state = { pos = (0, 0); heading = N }

let execute_instruction { pos = x, y; heading } (turn, walk) =
  let heading =
    match turn with
    | 'R' -> turn_right heading
    | 'L' -> turn_left heading
    | _ -> heading
  in
  let delta_x, delta_y = delta heading in
  { pos = (x + (walk * delta_x), y + (walk * delta_y)); heading }

let distance_from_origin { pos = x, y; _ } = abs x + abs y

let () =
  In_channel.stdin
  |> In_channel.input_line_exn
  |> String.split ~on:','
  |> Sequence.of_list
  |> Sequence.map ~f:String.strip
  |> Sequence.map ~f:parse
  |> Sequence.fold ~init:initial_state ~f:execute_instruction
  |> distance_from_origin
  |> Printf.printf "%d\n"
