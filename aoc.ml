open Core

let stdin =
  Sequence.unfold ~init:() ~f:(fun () ->
      In_channel.input_line In_channel.stdin
      |> Option.map ~f:(fun line -> (line, ())))

let nat = Sequence.unfold ~init:0 ~f:(fun v -> Some (v, v + 1))
