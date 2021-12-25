open Core

module PosSet = Set.Make (struct
  type t = int * int [@@deriving sexp]

  let compare (x1, y1) (x2, y2) =
    let x_cmp = Int.compare x1 x2 in
    if x_cmp <> 0 then x_cmp else Int.compare y1 y2
end)

type col = Col of int
type row = Row of int

type instruction =
  | Rect of int * int
  | Shift_down of col * int
  | Shift_right of row * int

let parse_rect dimensions =
  match String.split ~on:'x' dimensions with
  | [ cols; rows ] -> Some (Rect (Int.of_string cols, Int.of_string rows))
  | _ -> None

let parse_rotate_pos pos =
  match String.split ~on:'=' pos with
  | [ "y"; v ] | [ "x"; v ] -> Some (Int.of_string v)
  | _ -> None

let parse line =
  match String.split ~on:' ' line with
  | [ "rect"; dimensions ] -> parse_rect dimensions
  | [ "rotate"; "row"; pos; "by"; amount ] ->
      let amount = Int.of_string amount in
      pos
      |> parse_rotate_pos
      |> Option.map ~f:(fun row -> Shift_right (Row row, amount))
  | [ "rotate"; "column"; pos; "by"; amount ] ->
      let amount = Int.of_string amount in
      pos
      |> parse_rotate_pos
      |> Option.map ~f:(fun col -> Shift_down (Col col, amount))
  | _ -> None

let add_positions s (cols, rows) =
  let rec gen_positions s x y =
    if x = cols then s
    else if y = rows then gen_positions s (x + 1) 0
    else gen_positions (PosSet.add s (x, y)) x (y + 1)
  in
  gen_positions s 0 0

let shift_down s col amount =
  let height = 6 in
  s
  |> PosSet.map ~f:(fun (x, y) ->
         let y = if x = col then y + amount else y in
         (x, y mod height))

let shift_right s row amount =
  let width = 50 in
  s
  |> PosSet.map ~f:(fun (x, y) ->
         let x = if y = row then x + amount else x in
         (x mod width, y))

let execute s = function
  | Rect (cols, rows) -> add_positions s (cols, rows)
  | Shift_down (Col col, amount) -> shift_down s col amount
  | Shift_right (Row row, amount) -> shift_right s row amount

let print_grid grid =
  let width = 50 in
  let height = 6 in
  let rec print' x y =
    if y = height then ()
    else if x = width then (
      Out_channel.newline stdout;
      print' 0 (y + 1))
    else
      let ch = if PosSet.mem grid (x, y) then '#' else '.' in
      Out_channel.output_char stdout ch;
      print' (x + 1) y
  in
  print' 0 0

let () =
  let grid =
    In_channel.stdin
    |> In_channel.input_lines
    |> List.filter_map ~f:parse
    |> List.fold_left ~init:PosSet.empty ~f:execute
  in
  grid |> PosSet.length |> Printf.printf "Part 1: %d\n";
  Printf.printf "Part 2:\n";
  print_grid grid
