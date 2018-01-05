module IntT = struct
  type t = int
  let compare = compare
end

module StringT = struct
  type t = string
  let compare = compare
end

module IntMap = Map.Make(IntT)
module StringMap = Map.Make(StringT)

module MapLens(Key : Map.OrderedType) = struct
  let for_key key =
    let module M = Map.Make(Key) in
    let open Lens in
    { get = M.find key
    ; set = M.add key
    }
end

module IntMapLens = MapLens(IntT)
module StringMapLens = MapLens(StringT)
(*
begin try
    let games =
      List.assoc "games" list
      |> Js.String.split " " |> Array.to_list in
    let local_games =
      List.fold_left
        (fun acc v ->
           IntMap.add
             (int_of_string v)
             (List.assoc v list |> Game.game_of_pgn)
             acc
        ) IntMap.empty games in
    let game =
      match model.route with
      | Local id -> local_lens |-- IntMapLens.for_key id
      | _ -> model.game in        
    {model with local_games; game}, Cmd.none
  with e -> Js.log e; model, Cmd.none
end *)

let load_game key =
  Tea.Ex.LocalStorage.getItem key |> Tea_task.map (fun value -> key, value)

let load_scratch_game =
  load_game "scratch"
  |> Tea_task.map (fun (_, value) -> Game.game_of_pgn value)

let load_games =
  Tea.Ex.LocalStorage.length
  |> Tea_task.andThen (fun length ->
      let rec loop i acc =
        if i >= 0 then
          loop (i - 1) (Tea.Ex.LocalStorage.key i::acc)
        else acc in
      loop length [] |> Tea_task.sequence
    )
  |> Tea_task.andThen (fun keys ->
      List.map load_game keys
      |> Tea_task.sequence)
  |> Tea_task.map (fun list ->
      begin try Some (List.assoc "scratch" list |> Game.game_of_pgn)
        with Not_found -> None
      end,
      List.fold_left (fun acc (key, value) ->
          try
            IntMap.add (int_of_string key)
              (Game.game_of_pgn value)
              acc
          with _ -> acc)
        IntMap.empty
        list
    )

(* returns a task *)
let save_game key game =
  Game.pgn_of_game game |> Tea.Ex.LocalStorage.setItem key

let save_games games =
  IntMap.fold (fun key value acc ->
      save_game (string_of_int key) value::acc)
    games
    []
