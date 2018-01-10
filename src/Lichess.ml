open Tea

type msg =
  | Load_tournament
  | Tournament_data of (string, string Http.error) Result.t
[@@bs.deriving {accessors}]

type 'a transfer =
  | Idle
  | Loading
  | Failed
  | Received of 'a

type model = (string * string list) list transfer

let init = Idle


let update model = function
  | Load_tournament ->
    begin match model with
      | Loading | Received _ -> model, Cmd.none
      | Idle | Failed ->
        let url = "https://lichess.org/api/tournament/GToVqkC9" in
        model,
        Http.getString url |> Http.send tournament_data
    end
  | Tournament_data (Error _e) ->
    Failed, Cmd.none
  | Tournament_data (Ok data) -> 
    let open Json.Decoder in
    let players_decoder = list string in
    let pairing_decoder = map2 (fun x y -> x, y)
        (field "id" string)
        (field "u" players_decoder) in
    let list_decoder = list pairing_decoder in
    let pairings_decoder = field "pairings" list_decoder in
    begin match decodeString pairings_decoder data with
      | Ok tournament -> Received tournament
      | Error _ -> Failed
    end, Cmd.none


let view model =
  let open Html in
  let game_view (id, players) =
    td [] [ text id ]::
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
