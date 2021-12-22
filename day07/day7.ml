open Core

type ip_addr = { hypernet_seqs : string list; main_parts : string list }

let has_abba s =
  let rec has_abba' idx =
    if idx > String.length s - 4 then false
    else if
      Char.(
        s.[idx + 1] = s.[idx + 2]
        && s.[idx] = s.[idx + 3]
        && s.[idx] <> s.[idx + 1])
    then true
    else has_abba' (idx + 1)
  in
  has_abba' 0

let supports_tls { hypernet_seqs; main_parts } =
  List.exists ~f:has_abba main_parts
  && List.for_all ~f:(Fn.non has_abba) hypernet_seqs

let parse_ipv7 line =
  match String.split_on_chars ~on:[ '['; ']' ] line with
  | parts when List.length parts mod 2 = 1 ->
      parts
      |> List.foldi ~init:{ hypernet_seqs = []; main_parts = [] }
           ~f:(fun i { hypernet_seqs; main_parts } part ->
             if i mod 2 = 0 then
               { hypernet_seqs; main_parts = part :: main_parts }
             else { hypernet_seqs = part :: hypernet_seqs; main_parts })
      |> Option.some
  | _ -> None

let () =
  In_channel.stdin
  |> In_channel.input_lines
  |> List.filter_map ~f:parse_ipv7
  |> List.filter ~f:supports_tls
  |> List.length
  |> Printf.printf "%d\n"
