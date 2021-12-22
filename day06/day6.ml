open Core

let string_iteri ~f str =
  let rec aux idx =
    if idx = String.length str then ()
    else (
      f idx str.[idx];
      aux (idx + 1))
  in
  aux 0

let make_freqs lines =
  let freqs =
    match lines with
    | [] -> [||]
    | hd :: _ ->
        Array.init ~f:(fun _ -> Hashtbl.create (module Char)) (String.length hd)
  in
  lines
  |> List.iter ~f:(fun line ->
         line
         |> string_iteri ~f:(fun i ch ->
                let curr =
                  Hashtbl.find freqs.(i) ch |> Option.value ~default:0
                in
                Hashtbl.set ~key:ch ~data:(curr + 1) freqs.(i)));
  freqs

let find_ecm_max_freq freqs =
  let ecm =
    freqs
    |> Array.map
         ~f:
           (Hashtbl.fold ~init:('\x00', 0)
              ~f:(fun ~key ~data (most_common, most_common_amount) ->
                if data > most_common_amount then (key, data)
                else (most_common, most_common_amount)))
  in
  ecm |> Array.to_list |> List.map ~f:fst |> String.of_char_list

let find_ecm_min_freq freqs =
  let ecm =
    freqs
    |> Array.map
         ~f:
           (Hashtbl.fold ~init:('\x00', Int.max_value)
              ~f:(fun ~key ~data (least_common, least_common_amount) ->
                if data < least_common_amount then (key, data)
                else (least_common, least_common_amount)))
  in
  ecm |> Array.to_list |> List.map ~f:fst |> String.of_char_list

let () =
  let lines = In_channel.stdin |> In_channel.input_lines in
  let freqs = make_freqs lines in
  Printf.printf "Part 1: %s\n" @@ find_ecm_max_freq freqs;
  Printf.printf "Part 2: %s\n" @@ find_ecm_min_freq freqs
