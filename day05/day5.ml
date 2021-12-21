open Core

let md5 v = v |> Md5.digest_string |> Md5.to_hex

let rec find_char door_id idx =
  let digest = Printf.sprintf "%s%d" door_id idx |> md5 in
  if String.is_prefix ~prefix:"00000" digest then (digest.[5], idx + 1)
  else find_char door_id (idx + 1)

let make_password door_id =
  let password_length = 8 in
  let password = Array.init ~f:(Fn.const 'a') password_length in
  let rec make_password gen_idx password_idx =
    if password_idx = password_length then
      password |> Array.to_list |> String.of_char_list
    else
      let ch, gen_idx = find_char door_id gen_idx in
      password.(password_idx) <- ch;
      make_password gen_idx (password_idx + 1)
  in
  make_password 0 0

let () =
  In_channel.stdin
  |> In_channel.input_line_exn
  |> make_password
  |> Printf.printf "%s\n"
