open Tea
open! Util

type msg =
  | Load_games
  | Games_data of (string, string Http.error) Result.t
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
              ; title : string option
              }
type game = { id : string
            ; white : player
            ; black : player
            ; result : Pgn.result
            }
type model = game list transfer

let init = Idle

let tournament_id = "1JebhZhW"
let favorite_player = "alireza2003"


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
  | Load_games ->
    begin match model with
      | Loading | Received _ -> model, Cmd.none
      | Idle | Failed ->
        let url = Printf.sprintf "https://lichess.org/api/games/user/%s?moves=false&max=100" favorite_player in
        Loading,
        make_request
          url
          [Http.Header ("Accept", "application/x-ndjson")]
          games_data
    end
  | Games_data (Error _e) ->
    Js.log (Http.string_of_error(_e));
    Failed, Cmd.none
  | Games_data (Ok data) ->
    let open Json.Decoder in
    let id_decoder = field "id" string in
    let player_decoder color =
      field "players"
        (field color
           (map3 (fun name rating title -> {name; rating; title})
              (field "user" (field "name" string))
              (field "rating" int)
              (field "user" (field "title" string) |> maybe))) in
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
        (field "winner" string |> maybe) in

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


let player_title player =
  match player.title with
  | Some title -> Printf.sprintf "%s %s" title player.name
  | None -> player.name


let view model =
  let open Html in
  let game_view game =
    td [] [ a [ Printf.sprintf "#/lichess/%s" game.id |> href ]
              [ text game.id ]
          ]::
    td [] [ Game.string_of_result game.result |> text ]::
    List.map (fun player -> td [] [ text player ])
      [ player_title game.white ; game.white.rating |> string_of_int
      ; player_title game.black ; game.black.rating |> string_of_int ]
    |> tr [] in

  match model with
  | Idle -> p [] [ button
                     [ onClick Load_games ]
                     [ text "load Lichess games" ]
                 ]
  | Loading -> Elements.spinner ()
  | Received tournament ->
    List.map game_view tournament
    |> table []
  | Failed -> p [] [ text "Games could not be loaded."
                   ; button
                       [ onClick Load_games ]
                       [ text "retry" ]
                   ]
