open Core
module StringSet = Set.Make (String)

type ip_addr = { hypernet_seqs : string list; supernet_seqs : string list }

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

let supports_tls { hypernet_seqs; supernet_seqs } =
  List.exists ~f:has_abba supernet_seqs
  && List.for_all ~f:(Fn.non has_abba) hypernet_seqs

let bab_of_aba aba = [ aba.[1]; aba.[0]; aba.[1] ] |> String.of_char_list

let find_abas s =
  let rec find_abas' acc idx =
    if idx > String.length s - 3 then acc
    else if Char.(s.[idx] = s.[idx + 2] && s.[idx] <> s.[idx + 1]) then
      find_abas' (String.sub ~pos:idx ~len:3 s :: acc) (idx + 1)
    else find_abas' acc (idx + 1)
  in
  find_abas' [] 0

let supports_ssl { hypernet_seqs; supernet_seqs } =
  let all_abas = supernet_seqs |> List.bind ~f:find_abas |> StringSet.of_list in
  let all_babs = hypernet_seqs |> List.bind ~f:find_abas |> StringSet.of_list in
  if StringSet.length all_abas > 0 then
    let babs = StringSet.map ~f:bab_of_aba all_abas in
    StringSet.inter babs all_babs |> StringSet.length > 0
  else false

let parse_ipv7 line =
  match String.split_on_chars ~on:[ '['; ']' ] line with
  | parts when List.length parts mod 2 = 1 ->
      parts
      |> List.foldi ~init:{ hypernet_seqs = []; supernet_seqs = [] }
           ~f:(fun i { hypernet_seqs; supernet_seqs } part ->
             if i mod 2 = 0 then
               { hypernet_seqs; supernet_seqs = part :: supernet_seqs }
             else { hypernet_seqs = part :: hypernet_seqs; supernet_seqs })
      |> Option.some
  | _ -> None

let part1 ips =
  ips
  |> List.filter ~f:supports_tls
  |> List.length
  |> Printf.printf "Part 1: %d\n"

let part2 ips =
  ips
  |> List.filter ~f:supports_ssl
  |> List.length
  |> Printf.printf "Part 2: %d\n"

let () =
  let ips =
    In_channel.stdin |> In_channel.input_lines |> List.filter_map ~f:parse_ipv7
  in
  part1 ips;
  part2 ips
