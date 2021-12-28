open Core
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type bot_id = int
type target = Bot of bot_id | Output of int

let parse_target bot_or_output v =
  let v = int_of_string v in
  match bot_or_output with
  | "bot" -> Some (Bot v)
  | "output" -> Some (Output v)
  | _ -> None

type instruction =
  | Value of int * bot_id
  | Give of { bot : bot_id; low : target; high : target }

let parse line =
  match String.split ~on:' ' line with
  | [ "value"; v; "goes"; "to"; "bot"; bot_id ] ->
      Some (Value (int_of_string v, int_of_string bot_id))
  | [
   "bot";
   giver;
   "gives";
   "low";
   "to";
   bot_or_output_low;
   bot_id_low;
   "and";
   "high";
   "to";
   bot_or_output_high;
   bot_id_high;
  ] -> (
      match
        ( parse_target bot_or_output_low bot_id_low,
          parse_target bot_or_output_high bot_id_high )
      with
      | Some low, Some high ->
          Some (Give { bot = int_of_string giver; low; high })
      | _ -> None)
  | _ -> None

type dependency = Low_dep of bot_id | High_dep of bot_id

let string_of_dep = function
  | Low_dep bot_id -> Printf.sprintf "Low_dep %d" bot_id
  | High_dep bot_id -> Printf.sprintf "High_dep %d" bot_id

type value = Resolved of int | Unresolved of dependency

let string_of_value = function
  | Resolved v -> Printf.sprintf "Resolved %d" v
  | Unresolved (Low_dep bot_id) ->
      Printf.sprintf "Unresolved (Low_dep %d)" bot_id
  | Unresolved (High_dep bot_id) ->
      Printf.sprintf "Unresolved (High_dep %d)" bot_id

let add_value ~bot_id ~value values =
  let curr = IntMap.find values bot_id |> Option.value ~default:[] in
  IntMap.set ~key:bot_id ~data:(value :: curr) values

let add_dependency ~bot_id ~target graph =
  let curr = IntMap.find graph bot_id |> Option.value ~default:[] in
  IntMap.set ~key:bot_id ~data:(target :: curr) graph

let process_bot (graph, values) = function
  | Value (v, bot_id) -> (graph, add_value ~bot_id ~value:(Resolved v) values)
  | Give { bot; low = Bot bot_low; high = Bot bot_high } ->
      ( graph
        |> add_dependency ~bot_id:bot ~target:bot_low
        |> add_dependency ~bot_id:bot ~target:bot_high,
        values
        |> add_value ~bot_id:bot_low ~value:(Unresolved (Low_dep bot))
        |> add_value ~bot_id:bot_high ~value:(Unresolved (High_dep bot)) )
  | Give { bot; low = Bot bot_low; _ } ->
      ( graph |> add_dependency ~bot_id:bot ~target:bot_low,
        values |> add_value ~bot_id:bot_low ~value:(Unresolved (Low_dep bot)) )
  | Give { bot; high = Bot bot_high; _ } ->
      ( graph |> add_dependency ~bot_id:bot ~target:bot_high,
        values |> add_value ~bot_id:bot_high ~value:(Unresolved (High_dep bot))
      )
  | Give _ -> (graph, values)

type chip = V of int | Ref of bot_id

let string_of_chip = function
  | V v -> Printf.sprintf "V %d" v
  | Ref bot_id -> Printf.sprintf "Ref %d" bot_id

let chip_of_dependency = function
  | Low_dep bot_id | High_dep bot_id -> Ref bot_id

type robot = { id : bot_id; values : value list }

let build_resolution_list (graph, values) =
  let rec visit visited acc = function
    | [] -> (acc, visited)
    | bot_id :: to_visit when IntSet.mem visited bot_id ->
        visit visited acc to_visit
    | bot_id :: to_visit ->
        let neighbors = IntMap.find graph bot_id |> Option.value ~default:[] in
        let visited = IntSet.add visited bot_id in
        let result, visited = visit (IntSet.add visited bot_id) acc neighbors in
        let values = IntMap.find values bot_id |> Option.value ~default:[] in
        visit visited ({ id = bot_id; values } :: result) to_visit
  in
  ( graph
    |> IntMap.fold ~init:([], IntSet.empty)
         ~f:(fun ~key:bot_id ~data:_ (r, visited) -> visit visited r [ bot_id ])
    |> fst,
    values )

let resolve_value m = function
  | Resolved v -> v
  | Unresolved (Low_dep bot_id) -> IntMap.find_exn m bot_id |> fst
  | Unresolved (High_dep bot_id) -> IntMap.find_exn m bot_id |> snd

let get_values m values =
  assert (List.length values = 2);
  match List.map ~f:(resolve_value m) values with
  | [ v1; v2 ] when v1 > v2 -> (v2, v1)
  | [ v1; v2 ] -> (v1, v2)
  | _ -> assert false

let resolve =
  let rec resolve' m = function
    | [] -> m
    | { id; values } :: tl ->
        let values = get_values m values in
        resolve' (IntMap.set ~key:id ~data:values m) tl
  in
  resolve' IntMap.empty

let find_value low high m =
  m
  |> IntMap.to_sequence
  |> Sequence.find ~f:(fun (_, (l, h)) -> low = l && high = h)
  |> Option.map ~f:fst

let () =
  In_channel.stdin
  |> In_channel.input_lines
  |> List.filter_map ~f:parse
  |> List.fold_left ~init:(IntMap.empty, IntMap.empty) ~f:process_bot
  |> build_resolution_list
  |> fst
  |> resolve
  |> find_value 17 61
  |> Option.iter ~f:(Printf.printf "%d\n")
