open Tea
open! Util

type msg =
  | Load_tournament
  | Tournament_data of (string, string Http.error) Result.t
  | Load_game of string
  | Game_data of string * (string, string Http.error) Result.t
[@@bs.deriving {accessors}]

type 'a transfer =
  | Idle
  | Loading
  | Failed
  | Received of 'a

type player = { name : string
              ; rating : int
              }
type game = { id : string
            ; white : player
            ; black : player
            ; result : Pgn.result
            }
type model = game list transfer

let init = Idle

let tournament_id = "1JebhZhW"


let make_request url headers action =
  let request = 
    { Http.method' = "GET"
    ; headers = headers
    ; url = url
    ; body = Web.XMLHttpRequest.EmptyBody
    ; expect = Http.expectString
    ; timeout = None
    ; withCredentials = false
    }
  in
  Http.request request
  |> Http.send action


let get_game msg game_id =
  let url = (Printf.sprintf
               "https://lichess.org/game/export/%s.pgn" game_id) in
  make_request url [] msg


let update model = function
  | Load_tournament ->
    begin match model with
      | Loading | Received _ -> model, Cmd.none
      | Idle | Failed ->
        let url = Printf.sprintf "https://lichess.org/api/tournament/%s/games?moves=false" tournament_id in
        model,
        make_request
          url
          [Http.Header ("Accept", "application/x-ndjson")]
          tournament_data
    end
  | Tournament_data (Error _e) ->
    Js.log (Http.string_of_error(_e));
    Failed, Cmd.none
  | Tournament_data (Ok data) ->
    let open Json.Decoder in
    let id_decoder = field "id" string in
    let player_decoder color =
      field "players"
        (field color
           (map2 (fun name rating -> {name; rating})
              (field "user" (field "name" string))
              (field "rating" int))) in
    (* https://github.com/ornicar/scalachess/blob/master/src/main/scala/Status.scala *)
    let result_decoder = map2 (fun status winner ->
        match status with
        | "draw" | "stalemate" -> Some Chess.Draw
        | "created" | "started" -> None
        | "aborted" -> None
        | "mate" | "resign" | "timeout"
        | "outoftime" | "cheat" | "noStart" -> begin
            match winner with
            | Some "white" -> Some (Chess.Win White)
            | Some "black" -> Some (Chess.Win Black)
            | _ -> None
          end
        | _ -> None
      )
        (field "status" string)
        (maybe (field "winner" string)) in

    let game_decoder =
      map4 (fun id white black result -> { id ; white ; black ; result })
        id_decoder
        (player_decoder "white")
        (player_decoder "black")
        result_decoder in
    let games = String.split_on_char '\n' data in

    let games_decoded = List.map (decodeString game_decoder) games |> Result.to_list in
    Received games_decoded, Cmd.none

  | Load_game game_id ->
    model, get_game (game_data game_id) game_id
  | Game_data _ -> model, Cmd.none


let view model =
  let open Html in
  let game_view game =
    td [] [ a [ Printf.sprintf "#/lichess/%s" game.id |> href ]
              [ text game.id ]
          ]::
    td [] [ Game.string_of_result game.result |> text ]::
    List.map (fun player -> td [] [ text player ])
      [ game.white.name ; game.white.rating |> string_of_int
      ; game.black.name ; game.white.rating |> string_of_int ]
    |> tr [] in

  match model with
  | Idle -> p [] [ button
                     [ onClick Load_tournament ]
                     [ text "load Lichess tournament" ]
                 ]
  | Loading -> p [] [ text "Loading tournament..." ]
  | Received tournament ->
    List.map game_view tournament
    |> table []
  | Failed -> p [] [ text "Tournament could not be loaded."
                   ; button
                       [ onClick Load_tournament ]
                       [ text "retry" ]
                   ]
