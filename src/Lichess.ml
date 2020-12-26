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

type model = (string * string list) list transfer

let init = Idle

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
        let url = "https://lichess.org/api/tournament/a3lZhGdA/games?moves=false" in
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
    let player_decoder = (field "user" (field "name" string)) in
    let players_decoder = field "players"
        (map2 (fun x y -> [x; y])
           (field "white" player_decoder)
           (field "black" player_decoder)) in

    let game_decoder = map2 (fun x y -> x, y)
        id_decoder
        players_decoder in
    let games = String.split_on_char '\n' data in

    let games_decoded = List.map (decodeString game_decoder) games |> Result.filter in
    Received games_decoded, Cmd.none

  | Load_game game_id ->
    model, get_game (game_data game_id) game_id
  | Game_data _ -> model, Cmd.none


let view model =
  let open Html in
  let game_view (id, players) =
    td [] [ a [ Printf.sprintf "#/lichess/%s" id |> href ]
              [ text id ]
          ]::
    List.map (fun player -> td [] [ text player ]) players
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
