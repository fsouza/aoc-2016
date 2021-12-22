open Core

let string_iteri ~f str =
  let rec aux idx =
    if idx = String.length str then ()
    else (
      f idx str.[idx];
      aux (idx + 1))
  in
  aux 0

let find_ecm lines =
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

let () =
  In_channel.stdin |> In_channel.input_lines |> find_ecm |> Printf.printf "%s\n"
