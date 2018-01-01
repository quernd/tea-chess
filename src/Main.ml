open Tea
open Tea.Html
open Tea.App

type 'a transfer =
  | Loading
  | Failed
  | Received of 'a

type route =
  | Game
  | Tournament
  | Pgn of string

type model =
  { game : Game.model
  ; board : Board.model
  ; tournament : (string * string list) list transfer
  ; pgn : (string * string transfer) list (* association list *)
  ; route : route
  }

type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Key_pressed of Keyboard.key_event
  | Tournament_data of (string, string Http.error) Result.t
  | Location_change of Web.Location.location  
  | Pgn_requested of string
  | Pgn_data of string * (string, string Http.error) Result.t
  | Close_tab of string
  | Validate_pgn of string
[@@bs.deriving {accessors}]

let proxy = "http://localhost:3000/fetch/"

let route_of_location location =
  let open Web.Location in
  let route = Chess.split_on_char '/' location.hash in
  match route with
  | ["#"; "game"] -> Game, Cmd.none
  | ["#"; "tournament"] -> Tournament, Cmd.none
  | ["#"; "pgn"; id] ->  Pgn id, Cmd.msg (Pgn_requested id)
  | _ -> Game, Navigation.modifyUrl "#/game"  (* default route *)

let init () location =
  let route, cmd = route_of_location location in
  let url = "https://lichess.org/api/tournament/GToVqkC9" in
  let init_cmd =
    Http.getString url |> Http.send tournament_data in
  { game = Game.init ()
  ; board = Board.init ()
  ; tournament = Loading
  ; route
  ; pgn = []
  }, Cmd.batch [init_cmd; cmd]


let update model = function
  | Board_msg (Move move) ->
    let game, cmd = Game.update model.game (Game.Make_move move) in
    {model with game}, Cmd.map game_msg cmd
  | Board_msg msg ->
    let board, cmd = Board.update model.board msg in
    {model with board}, Cmd.map board_msg cmd
  | Game_msg msg ->
    let game, cmd = Game.update model.game msg in
    {model with game}, Cmd.map game_msg cmd
  | Key_pressed key_event ->
    model,
    begin match key_event.ctrl, key_event.key_code with
      | _, 37 (* left *) | true, 66 (* Ctrl-b *) ->
        Cmd.msg (Game_msg Back_button)
      | _, 39 (* right *) | true, 70 (* Ctrl-f *) ->
        Cmd.msg (Game_msg Fwd_button)
      | true, 82 (* Ctrl-r *) -> Cmd.msg (Game_msg Random_button)
      | true, 84 (* Ctrl-t *) -> Cmd.msg (Game_msg Back_button)
      | _ -> Cmd.none
    end
  | Tournament_data (Result.Error e) -> Js.log e;
    {model with tournament = Failed}, Cmd.none
  | Tournament_data (Result.Ok data) -> 
    let open Json.Decoder in
    let players_decoder = list string in
    let pairing_decoder = map2 (fun x y -> x, y)
        (field "id" string)
        (field "u" players_decoder) in
    let list_decoder = list pairing_decoder in
    let pairings_decoder = field "pairings" list_decoder in
    {model with
     tournament = match decodeString pairings_decoder data with
       | Ok tournament -> Received tournament
       | Error _ -> Failed
    }, Cmd.none
  | Location_change location ->
    let route, cmd = route_of_location location in
    {model with route}, cmd
  | Pgn_requested id ->
    begin
      try
        match List.assoc id model.pgn with
        | Received pgn -> model, Cmd.msg (Validate_pgn pgn)
        | _ -> model, Cmd.none
      with Not_found -> 
        let url =
          Printf.sprintf "%shttps://lichess.org/game/export/%s.pgn" proxy id in
        let cmd = Http.getString url |> Http.send (pgn_data id) in
        { model with
          route = Pgn id
        ; pgn = (id, Loading)::model.pgn
        }, cmd
    end
  | Pgn_data (id, Result.Error _e) ->
    { model with
      pgn = (id, Failed)::List.remove_assoc id model.pgn
    }, Cmd.none
  | Pgn_data (id, Result.Ok data) ->
    { model with
      pgn = (id, Received data)::List.remove_assoc id model.pgn
    }, Cmd.msg (Validate_pgn data)
  | Close_tab id ->
    let pgn = List.remove_assoc id model.pgn in
    let route = begin match pgn with
      | (id, _)::_ -> Pgn id
      | [] -> Tournament
    end in
    {model with pgn; route}, Cmd.none
  | Validate_pgn pgn ->
    try let position, ply, moves = Pgn.game_of_string pgn in
      {model with game = {position; ply; moves}}, Cmd.none
    with _e -> Js.log _e ; model, Cmd.none



let header_nav_view =
  nav [class' "top"]
    [ ul []
        [ li [class' "home"] [text "TEA-Chess"]
        ; li []
            [ a [href "https://quernd.github.io/tutorials/tea-chess"]
                [text "Tutorial"] ]
        ; li []
            [ a [href "https://github.com/quernd/tea-chess"]
                [text "Code"] ]
        ]
    ]

let tournament_view tournament =
  match tournament with
  | Loading -> text "Loading tournament..."
  | Received tournament' ->
    List.map
      (fun (id, players) ->
         td [] [ a [href (Printf.sprintf "#/pgn/%s" id)] [text id]]::
         (List.map (fun player -> td [] [text player]) players) |> tr [])
      tournament'
    |> table []
  | Failed -> text "Tournament could not be loaded."

let pgn_view id model =
  try match List.assoc id model.pgn with
    | Loading -> text "Loading PGN game..."
    | Received _ -> Game.view model.game |> map game_msg
    | Failed -> text "Game could not be loaded."             
  with Not_found -> text ""

let game_nav_view model =
  let pgn_nav_item id =
    li [ if model.route = Pgn id then class' "current" else noProp ]
      [ if model.route = Pgn id
        then text id
        else a [href (Printf.sprintf "#/pgn/%s" id)] [text id]
      ; span [] [text " "]
      ; span [ style "cursor" "pointer"
             ; onClick (Close_tab id)
             ] [text "(x)"]
      ] in

  let game_nav_item current link label =
    li [ if current then class' "current" else noProp ]
      [ if current then text label else a [href link] [text label] ] in

  nav [class' "top tabbed"]
    [ ul []
        ([ game_nav_item (model.route = Game) "#/game" "Game"
         ; game_nav_item (model.route = Tournament) "#/tournament" "Tournament"
         ] @ (List.rev_map (fun (id, _) -> pgn_nav_item id) model.pgn))
    ]

let view model =
  let game_status = Chess.game_status model.game.position in
  let interactable =
    match game_status with
    | Play move_list -> Board.Interactable (model.game.position.turn, move_list)
    | _ -> Board.Not_interactable
  in
  main []
    [ section [id "board"]
        [ header_nav_view
        ; Board.view interactable model.game.position.ar model.board
          |> map board_msg
          ;
          List.map (map board_msg) Board.buttons_view @
          List.map (map game_msg) Game.buttons_view
          |> nav [id "buttons"]
          (* ; Board.result_view game_status *)
        ]
    ; section [id "game"]
        [ game_nav_view model
        ; section [class' "scroll"]
            [ match model.route with
              | Game -> Game.view model.game |> map game_msg
              | Tournament -> tournament_view model.tournament
              | Pgn id -> pgn_view id model
            ]
        ]
    ]


let subscriptions model =
  Sub.batch [ Board.subscriptions model.board |> Sub.map board_msg
            ; Keyboard.downs key_pressed
            ]


let main =
  Navigation.navigationProgram location_change
    { init
    ; update
    ; view
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none)
    }
