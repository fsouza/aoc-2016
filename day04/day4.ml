open Core
module CharMap = Map.Make (Char)

type room = { encrypted_name : string; checksum : string; sector_id : int }
type decrypted_room = { name : string; sector_id : int }

let line_re = Re.compile (Re.Pcre.re {|^(.+)-(\d+)\[([a-z]{5})\]$|})

let parse_room line =
  line
  |> Re.exec_opt line_re
  |> Option.map ~f:Re.Group.all
  |> Option.bind ~f:(function
       | [| _; encrypted_name; sector_id; checksum |] ->
           Some
             { encrypted_name; checksum; sector_id = Int.of_string sector_id }
       | _ -> None)

let string_of_room { encrypted_name; sector_id; checksum } =
  Printf.sprintf "%s-%d[%s]" encrypted_name sector_id checksum

let calc_checksum { encrypted_name; _ } =
  let char_counts =
    encrypted_name
    |> String.fold ~init:CharMap.empty ~f:(fun map ch ->
           if Char.is_alpha ch then
             let curr = CharMap.find map ch |> Option.value ~default:0 in
             CharMap.set ~key:ch ~data:(curr + 1) map
           else map)
    |> CharMap.to_alist ~key_order:`Increasing
    |> List.sort ~compare:(fun (ch1, count1) (ch2, count2) ->
           let count_cmp = Int.compare count2 count1 in
           if count_cmp = 0 then Char.compare ch1 ch2 else count_cmp)
  in
  List.take char_counts 5 |> List.map ~f:fst |> String.of_char_list

let is_valid ({ checksum; _ } as room) =
  String.equal (calc_checksum room) checksum

let a_code = Char.to_int 'a'
let int_of_ch ch = Char.to_int ch - a_code
let ch_of_int v = Char.of_int_exn (v + a_code)

let decrypt n ch =
  if Char.is_alpha ch then
    let v = int_of_ch ch in
    let v = (v + n) mod 26 in
    ch_of_int v
  else ' '

let decrypt { encrypted_name; sector_id; _ } =
  let name = String.map ~f:(decrypt sector_id) encrypted_name in
  { name; sector_id }

let part1 rooms =
  rooms
  |> List.fold_left ~init:0 ~f:(fun acc ({ sector_id; _ } : room) ->
         sector_id + acc)
  |> Printf.printf "Part 1: %d\n"

let part2 rooms =
  Printf.printf "Part 2:\n";
  rooms
  |> List.map ~f:decrypt
  |> List.iter ~f:(fun { name; sector_id } ->
         Printf.printf "%s (%d)\n" name sector_id)

let () =
  let valid_rooms =
    In_channel.stdin
    |> In_channel.fold_lines ~init:[] ~f:(fun rooms line ->
           match parse_room line with
           | None -> rooms
           | Some room -> room :: rooms)
    |> List.filter ~f:is_valid
  in
  part1 valid_rooms;
  part2 valid_rooms
