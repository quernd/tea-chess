open Tea
open! Util

open Lens
open Infix

type route =
  | Game of int
  | Games
  | Lichess of string


type model =
  { games : Game.model IntMap.t
  ; game : (model, Game.model) Lens.t
  ; route : route
  ; board : Board.model
  ; lichess : Lichess.model
  ; lichess_games : Game.model StringMap.t
  ; stockfish : Stockfish.model option
  }

type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Lichess_msg of Lichess.msg
  | Random_button
  | Random_move of Chess.move
  | Key_pressed of Keyboard.key_event
  | Reset_game
  | Switch_game of int
  | New_game
  | Location_changed of Web.Location.location
  | Save_games
  | Clear_games
  | Games_loaded of
      Web.Location.location * (Game.model IntMap.t, string) Result.t
  | Games_saved of (unit list, string) Result.t
  | Load_stockfish
  | Stockfish_msg of Stockfish.msg
[@@bs.deriving {accessors}]

external alert : (string -> unit) = "alert" [@@bs.val]

external stockfish_loader :
  (string -> msg) -> Stockfish.stockfish Js.nullable =
  "stockfish_loader" [@@bs.val]


let games_lens =
  { get = (fun r -> r.games)
  ; set = (fun v r -> { r with games = v })
  }

let add_game_update_lens game model =
  let key = (IntMap.max_binding model.games |> fst) + 1 in
  let games = IntMap.add key game model.games in
  { model with games
             ; game = games_lens |-- IntMapLens.for_key key
  }, key


let route_of_location (location:Web.Location.location) =
  let route = Js.String.split "/" location.hash |> Array.to_list in
  match route with
  | ["#"; ""] -> Game 1
  | ["#"; "games"] -> Games
  | ["#"; "game"; id] -> Game (int_of_string id)
  | ["#"; "lichess"; id] -> Lichess id
  | _ -> Game 1  (* default route *)

let location_of_route = function
  | Game id -> Printf.sprintf "#/game/%d" id
  | Lichess id -> Printf.sprintf "#/lichess/%s" id
  | Games -> "#/games"


let update_route model = function
  | route when model.route = route -> model, Cmd.none
  | Game id as route when IntMap.mem id model.games ->
    let game = games_lens |-- IntMapLens.for_key id in
    { model with route; game }, Cmd.none
  | Game _ -> { model with route = Game 1 },
              location_of_route (Game 1) |> Navigation.modifyUrl
  | Games ->
    { model with route = Games }, Cmd.none
  | Lichess game_id ->
    begin try
        let game = StringMap.find game_id model.lichess_games in
        let model, key = add_game_update_lens game model in
        model,
        location_of_route (Game key) |> Navigation.modifyUrl
      with Not_found ->
        model, Cmd.msg (Lichess_msg (Load_game game_id))
    end


let init_model =
  { games = IntMap.empty |> IntMap.add 1 Game.init
  ; game = games_lens |-- IntMapLens.for_key 1
  ; route = Game 1
  ; board = Board.init
  ; lichess = Lichess.init
  ; lichess_games = StringMap.empty
  ; stockfish = None
  }

let init () location =
  (* wait for games to be restored before the route is updated *)
  init_model, Tea_task.attempt (games_loaded location) Storage.load_games

let update model = function
  | Board_msg (Move move) | Random_move move ->
    let game, cmd = Game.update (model |.. model.game) (Game.Move move) in
    let autoplay_cmd =
      match model.stockfish, Chess.game_status game.position with
      | Some stockfish, Play _ when stockfish.autoplay ->
        game.position
        |> Chess.FEN.fen_of_position
        |> Stockfish.make_move
        |> stockfish_msg
        |> Cmd.msg
      | _ -> Cmd.none in
    model |> model.game ^= game, Cmd.batch [Cmd.map game_msg cmd; autoplay_cmd]
  | Board_msg msg ->
    let board, cmd = Board.update model.board msg in
    { model with board }, Cmd.map board_msg cmd
  | Game_msg msg ->
    let game, cmd = Game.update (model |.. model.game) msg in
    model |> model.game ^= game, Cmd.map game_msg cmd
  | Lichess_msg (Game_data (game_id, Error _)) ->
    Printf.sprintf "Game %s could not be loaded!" game_id |> alert;
    model, Cmd.none
  | Lichess_msg (Game_data (game_id, Ok data)) ->
    begin match Game.game_of_pgn data with
      | Some game ->
        let lichess_games = StringMap.add game_id game model.lichess_games in
        let model, key =
          { model with lichess_games }
          |> add_game_update_lens game in
        model, location_of_route (Game key) |> Navigation.modifyUrl
      | None -> alert "Game could not be parsed!";
        model, Cmd.none
    end
  | Lichess_msg msg ->
    let lichess, cmd = Lichess.update model.lichess msg in
    { model with lichess }, Cmd.map lichess_msg cmd
  | Random_button ->
    model,
    begin match Chess.game_status (model |.. model.game).position with
      | Play move_list ->
        move_list
        |> List.length
        |> Random.int 0
        |> Random.generate
          (fun random_number ->
             List.nth move_list random_number |> random_move)
      | _ -> Cmd.none
    end
  | Key_pressed key_event ->
    model,
    begin match key_event.ctrl, key_event.key_code with
      | _, 37 (* left *) | true, 66 (* Ctrl-b *) ->
        Cmd.msg (Game_msg Take_back)
      | _, 39 (* right *) | true, 70 (* Ctrl-f *) ->
        Cmd.msg (Game_msg Forward)
      | true, 78 (* Ctrl-n *) -> Cmd.msg New_game
      | true, 82 (* Ctrl-r *) -> Cmd.msg Random_button
      | _ -> Cmd.none
    end
  | Reset_game ->
    model |> model.game ^= Game.init, Cmd.none
  | New_game -> 
    let model, key = add_game_update_lens Game.init model in
    model, location_of_route (Game key) |> Navigation.newUrl
  | Switch_game 0 -> model,
                     location_of_route Games |> Navigation.newUrl
  | Switch_game i -> model,
                     location_of_route (Game i) |> Navigation.newUrl
  | Location_changed location ->
    route_of_location location |> update_route model
  | Save_games -> model,
                  Storage.save_games model.games
                  |> Tea_task.sequence
                  |> Tea_task.attempt games_saved
  | Games_saved (Ok _) -> alert "games successfully saved"; model, Cmd.none
  | Games_saved (Error e) ->
    Printf.sprintf "games couldn't be saved because of error %s" e |> alert;
    model, Cmd.none
  | Clear_games -> model, Ex.LocalStorage.clearCmd ()
  | Games_loaded (location, Ok games) ->
    route_of_location location
    |> update_route
      (if games = IntMap.empty then model else { model with games })
  | Games_loaded (location, Error e) ->
    Printf.sprintf "games couldn't be restored because of error %s" e |> alert;
    route_of_location location
    |> update_route model
  | Load_stockfish ->
    let f = (fun string -> Stockfish.data string |> stockfish_msg) in
    begin match stockfish_loader f |> Js.Nullable.toOption
      with
      | None -> alert "Stockfish failed to load"; model, Cmd.none
      | Some stockfish ->
        { model with stockfish = Some (Stockfish.init stockfish) }, Cmd.none
    end
  | Stockfish_msg (Move move) ->
    begin try
        let move' = Pgn.move_of_pgn_move
            ((model |.. model.game).position) move.move in
        let game, cmd =
          Game.update (model |.. model.game) (Game.Move move') in
        model |> model.game ^= game, Cmd.map game_msg cmd
      with Chess.Illegal_move -> model, Cmd.none
      (* Stockfish rarely makes illegal moves, but just in case ;-) *)
    end
  | Stockfish_msg msg ->
    begin match model.stockfish with
      | None -> model, Cmd.none
      | Some stockfish' ->
        let stockfish, cmd = Stockfish.update stockfish' msg in
        { model with stockfish = Some stockfish }, Cmd.map stockfish_msg cmd
    end


let header_nav_view =
  let open! Html in
  let link ?(home=false) link description =
    li [ if home then class' "home" else noProp ]
      [ a [ href link ] [ text description ] ] in

  nav [ id "main-nav" ]
    [ ul [] [ link ~home:true "#/" "TEA-Chess"
            ; link "https://quernd.github.io/tutorials/tea-chess" "Tutorial"
            ; link "https://github.com/quernd/tea-chess" "Code"
            ]
    ]


let games_picker model =
  let open Html in
  let selected = match model.route with
    | Game id -> Some id
    | _ -> None in
  let option_view k _v acc =
    option' [ string_of_int k |> value
            ; Attributes.selected (selected = Some k) ]
      [ Printf.sprintf "Game %d" k |> text ]::acc in

  let options = IntMap.fold option_view model.games []
                |> List.rev in
  option' [ value "0"
          ; Attributes.selected (model.route = Games) ]
    [ text "Lichess games" ]::options
  |> select [ int_of_string >>> switch_game |> onChange ?key:None ]




let view model =
  let open! Html in
  let game = model |.. model.game in
  let position = game.position in
  let interactable =
    match Chess.game_status position with
    | Play move_list ->
      Board.Interactable (position.turn, move_list)
    | _ -> Board.Not_interactable in

  let stockfish_view =
    match model.stockfish with
    | None -> p [] [ button' Load_stockfish "load Stockfish" ]
    | Some stockfish ->
      match stockfish.status, interactable with
      | Idle, Not_interactable -> Game.status_view position
      | _ -> game.position
             |> Chess.FEN.fen_of_position
             |> Stockfish.view stockfish
             |> map stockfish_msg in

  main []
    [ section [ id "board" ]
        [ header_nav_view
        ; Board.view interactable position.ar model.board
          |> map board_msg
        ; p [] [ map board_msg Board.flip_button_view
               ; button' Random_button "Random move!"
               ; button' (Game_msg Take_back) "Take back"
               ]
        ; stockfish_view
        ]
    ; section [ id "game" ]
        [ nav [] [ ul [] [ li []
                             [ games_picker model
                             ; button' Reset_game "reset game"
                             ; button' New_game "new game"
                             ; button' Save_games "save games"
                             ; button' Clear_games "clear storage"
                             ]
                         ]
                 ]
        ; div [ class' "scroll" ]
            [ match model.route with
              | Game _ -> Game.view game |> map game_msg
              | Games -> Lichess.view model.lichess |> map lichess_msg
              | _ -> noNode
            ]
        ]
    ]


let subscriptions model =
  [ Board.subscriptions model.board |> Sub.map board_msg
  ; Keyboard.downs key_pressed
  ] |> Sub.batch

let main =
  Navigation.navigationProgram location_changed
    { init
    ; update
    ; view
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none)
    }
