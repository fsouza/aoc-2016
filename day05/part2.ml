open Core

let md5 v = v |> Md5.digest_string |> Md5.to_hex

let rec find_char door_id idx =
  let digest = Printf.sprintf "%s%d" door_id idx |> md5 in
  if String.is_prefix ~prefix:"00000" digest then
    let password_idx =
      digest.[5] |> Char.get_digit |> Option.value ~default:8
    in
    (password_idx, digest.[6], idx + 1)
  else find_char door_id (idx + 1)

let string_of_array arr = arr |> Array.to_list |> String.of_char_list

let make_password door_id =
  let password_length = 8 in
  let placeholder = '_' in
  let password = Array.init ~f:(Fn.const placeholder) password_length in
  let rec make_password gen_idx remaining_chars =
    if remaining_chars = 0 then string_of_array password
    else
      let password_idx, ch, gen_idx = find_char door_id gen_idx in
      if password_idx >= password_length then
        make_password gen_idx remaining_chars
      else
        let is_placeholder = Char.equal password.(password_idx) placeholder in
        let remaining_chars =
          if is_placeholder then remaining_chars - 1 else remaining_chars
        in
        if is_placeholder then password.(password_idx) <- ch;
        make_password gen_idx remaining_chars
  in
  make_password 0 password_length

let () =
  In_channel.stdin
  |> In_channel.input_line_exn
  |> make_password
  |> Printf.printf "%s\n"
