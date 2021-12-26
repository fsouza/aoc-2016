open Core

let parse_marker m =
  let m = String.sub ~pos:1 ~len:(String.length m - 2) m in
  match String.split ~on:'x' m with
  | [ v1; v2 ] -> (Int.of_string v1, Int.of_string v2)
  | _ -> (0, 0)

let decompressed_length input =
  let rec decompressed_length acc idx =
    if idx = String.length input then acc
    else
      let ch = input.[idx] in
      if Char.equal ch '(' then consume_marker acc idx
      else decompressed_length (acc + 1) (idx + 1)
  and consume_marker acc idx =
    let closing_pos = String.index_from_exn input idx ')' in
    let marker = String.sub ~pos:idx ~len:(closing_pos + 1 - idx) input in
    let len, times = parse_marker marker in
    let idx = closing_pos + 1 + len in
    let acc = acc + (len * times) in
    decompressed_length acc idx
  in
  decompressed_length 0 0

let () =
  In_channel.stdin
  |> In_channel.input_line_exn
  |> decompressed_length
  |> Printf.printf "%d\n"
