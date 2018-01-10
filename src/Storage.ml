open Util

let load_game key =
  Tea.Ex.LocalStorage.getItem key |> Tea_task.map (fun value -> key, value)


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
      List.fold_left (fun acc (key, value) ->
          try begin match Game.game_of_pgn value with
            | Some game -> IntMap.add (int_of_string key) game acc
            | None -> acc
          end with _ -> acc)
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
