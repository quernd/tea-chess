open Tea
open Tea.Html
open Tea.App

open Lens.Infix

open Storage

type stockfish = <
  postMessage : string -> unit [@bs.meth];
> Js.t

external stockfish : stockfish = "stockfish" [@@bs.val]




type 'a transfer =
  | Idle
  | Loading
  | Failed
  | Received of 'a

type ready =
  | Ready
  | Thinking
  | Loading

type model =
  { game : game_lens
  ; board : Board.model
  ; scratch_game : Game.model
  ; local_games : Game.model IntMap.t
  ; tournament : (string * string list) list transfer
  ; lichess_games : int StringMap.t
  ; route : route
  ; stockfish : ready
  }
and game_lens = (model, Game.model) Lens.t
and route =
  | Local of int
  | Lichess of string
  | Scratch
  | Tournament

let local_lens = let open Lens in
  { get = (fun x -> x.local_games)
  ; set = (fun v x -> { x with local_games = v })
  }
let lichess_lens = let open Lens in
  { get = (fun x -> x.lichess_games)
  ; set = (fun v x -> { x with lichess_games = v })
  }
let scratch_lens = let open Lens in
  { get = (fun x -> x.scratch_game)
  ; set = (fun v x -> { x with scratch_game = v })
  }


type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Key_pressed of Keyboard.key_event
  | New_tab
  | Reset_game
  | Save_games
  | Clear_games
  | Games_loaded of ((Game.model option * Game.model IntMap.t), string) Result.t
  | Games_saved of (unit list, string) Result.t
  | Load_tournament
  | Tournament_data of (string, string Http.error) Result.t
  | Location_change of Web.Location.location  
  | Pgn_requested of string
  | Pgn_data of game_lens * (string, string Http.error) Result.t
  | Close_tab of string
  | Validate_pgn of string
  | Stockfish_data of string
  | Stockfish_request_move
[@@bs.deriving {accessors}]

let proxy = "http://localhost:3000/fetch/"

let route_of_location location =
  let open Web.Location in
  let route = Chess.split_on_char '/' location.hash in
  match route with
  | ["#"; ""] -> Scratch
  | ["#"; "tournament"] -> Tournament
  | ["#"; "local"; id] -> Local (int_of_string id)
  | ["#"; "lichess"; id] -> Lichess id
  | _ -> Scratch

let location_of_route = function
  | Local id -> Printf.sprintf "#/local/%d" id
  | Lichess id -> Printf.sprintf "#/lichess/%s" id
  | Scratch -> "#/"
  | Tournament -> "#/tournament"

let update_route model route =
  if model.route = route then model, Cmd.none
  else
    match route with
    | Local id when IntMap.mem id model.local_games ->
      let game = local_lens |-- IntMapLens.for_key id in
      {model with route; game}, Cmd.none
    | Local _ -> {model with route = Scratch}, Cmd.none
    | Tournament ->
      let url = "https://lichess.org/api/tournament/GToVqkC9" in
      {model with route = Tournament; tournament = Loading},
      Http.getString url |> Http.send tournament_data
    | Lichess game_id ->
      begin try
          let id = StringMap.find game_id model.lichess_games in
          let game = local_lens |-- IntMapLens.for_key id in
          let route = Local id in
          {model with route; game}, Cmd.none
        with Not_found ->
          let key = begin try (IntMap.max_binding model.local_games |> fst) + 1
            with Not_found -> 1 end in
          let game = local_lens |-- IntMapLens.for_key key in
          let route = Local key in
          { model with
            route
          ; local_games = IntMap.add key (Game.init ()) model.local_games
          ; game
          ; lichess_games = StringMap.add game_id key model.lichess_games },
          Cmd.batch
            [ location_of_route route |> Navigation.modifyUrl
            ; Printf.sprintf
                "%shttps://lichess.org/game/export/%s.pgn" proxy game_id
              |> Http.getString
              |> Http.send (pgn_data game) ]
      end
    | _ -> {model with route = Scratch; game = scratch_lens}, Cmd.none




let init_model =
  { scratch_game = Game.init ()
  ; game = scratch_lens
  ; board = Board.init ()
  ; tournament = Idle
  ; local_games = IntMap.empty
  ; lichess_games = StringMap.empty
  ; route = Scratch
  ; stockfish = Loading
  }



let load_games = Tea_task.attempt games_loaded Storage.load_games

let init () location =
  let model, cmd =
    route_of_location location |> update_route init_model in
  model, Cmd.batch [cmd; load_games]


let update model = function
  | Board_msg (Move move) -> 
    let game, cmd = Game.update (model |. model.game) (Game.Make_move move) in
    model |> model.game ^= game, Cmd.map game_msg cmd
  | Board_msg msg ->
    let board, cmd = Board.update model.board msg in
    {model with board}, Cmd.map board_msg cmd
  | Game_msg msg ->
    let game, cmd = Game.update (model |. model.game) msg in
    model |> model.game ^= game, Cmd.map game_msg cmd
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
  | New_tab ->
    let key = try (IntMap.max_binding model.local_games |> fst) + 1
      with Not_found -> 1 in
    { model with
      local_games = IntMap.add key (Game.init ()) model.local_games
    ; scratch_game = Game.init ()
    }, location_of_route (Local key) |> Navigation.modifyUrl
  | Reset_game ->
    model |> model.game ^= Game.init (), Cmd.none
  | Save_games ->
    model,
    Storage.save_game "scratch" model.scratch_game::
    Storage.save_games model.local_games
    |> Tea_task.sequence
    |> Tea_task.attempt games_saved
  | Games_saved Ok _ ->
    Js.log "all games saved" ; model, Cmd.none
  | Games_saved Error e -> Js.log e ; model, Cmd.none
  | Games_loaded result ->
    begin match result with
      | Ok ((Some scratch_game), local_games) ->
        {model with local_games; scratch_game}, Cmd.none
      | Ok (None, local_games) ->
        {model with local_games}, Cmd.none
      | Error e -> Js.log e ; model, Cmd.none
    end
  | Clear_games ->
    model, Ex.LocalStorage.clearCmd ()
  | Load_tournament ->
    model,
    location_of_route Tournament |> Navigation.modifyUrl
  | Tournament_data (Error _e) ->
    {model with tournament = Failed}, Cmd.none
  | Tournament_data (Ok data) -> 
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
  | Pgn_data (lens, (Ok data)) ->
    begin try
        let game = Game.game_of_pgn data in
        model |> lens ^= game, Cmd.none
      with _e -> model, Cmd.none end
  | Location_change location ->
    route_of_location location |> update_route model
  | Stockfish_data s ->
    begin try match Stockfish.parse s with
      | Stockfish.Ready -> {model with stockfish = Ready}, Cmd.none
      | Stockfish.Irrelevant -> model, Cmd.none
      | Stockfish.Move move ->
        let move' = Pgn.move_of_pgn_move
            ((model |. model.game).position) move.move in
        let game, cmd = Game.update (model |. model.game) (Game.Make_move move') in
        {model with stockfish = Ready} |> model.game ^= game, Cmd.map game_msg cmd
      with Chess.Illegal_move -> model, Cmd.none
    end
  | Stockfish_request_move ->
    begin match model.stockfish with
      | Ready ->
        stockfish##postMessage
          ("position fen " ^
           Chess.fen_of_position (model |. model.game).position) ;
        stockfish##postMessage "go depth 12" ;
        {model with stockfish = Thinking}, Cmd.none
      | _ -> model, Cmd.none end
  | _ -> model, Cmd.none


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
  | Idle -> text ""
  | Loading -> text "Loading tournament..."
  | Received tournament' ->
    List.map
      (fun (id, players) ->
         td [] [ a [href (Printf.sprintf "#/lichess/%s" id)] [text id]]::
         (List.map (fun player -> td [] [text player]) players) |> tr [])
      tournament'
    |> table []
  | Failed -> text "Tournament could not be loaded."



let game_nav_view model =
  let game_nav_item current link label =
    li [ if current then class' "current" else noProp ]
      [ if current then text label else a [href link] [text label] ] in

  let g id _game k =
    let item = game_nav_item 
        (model.route = Local id)
        (Printf.sprintf "#/local/%d" id)
        (Printf.sprintf "Game %d" id)
    in fun tl -> k (item::tl) in

  (* http://blog.wakatta.jp/blog/2011/11/11/haskell-foldr-as-foldl/ *)

(*
  let f hd tl = game_nav_item 
      (model.route = Local hd)
      (Printf.sprintf "#/local/%d" hd)
      (Printf.sprintf "Game %d" hd)::tl in

  let g id _game k acc =
    k (f id acc) in
*)

  nav [class' "top tabbed"]
    [ ul []
        (game_nav_item (model.route = Scratch) "#/" "*scratch*"
         ::(IntMap.fold g model.local_games (fun x -> x)) [])
    ]

let buttons_view =
  nav []
    [ button [onClick New_tab] [text "new tab"]
    ; button [onClick Reset_game] [text "reset game"]
    ; button [onClick Save_games] [text "save all games in local storage"]
    ; button [onClick Clear_games] [text "clear local storage"]
    ; button [onClick Load_tournament] [text "load a game from Lichess"]
    ]

let scratch_view =
  div []
    [ buttons_view
    ; p []
        [ text "This is a scratch buffer.  Click \"new tab\" to open a new game in a separate tab."]
    ]


let view model =
  let game = model |. model.game in
  let game_view = Game.view game |> map game_msg in
  let game_status = Chess.game_status game.position in
  let interactable =
    match game_status with
    | Play move_list ->
      Board.Interactable (game.position.turn, move_list)
    | _ -> Board.Not_interactable in
  main []
    [ section [id "board"]
        [ header_nav_view
        ; Board.view interactable game.position.ar model.board
          |> map board_msg
        ; List.map (map board_msg) Board.buttons_view @
          List.map (map game_msg) Game.buttons_view
          |> nav [id "buttons"]
        (* ; Board.result_view game_status *)
        ; p [style "font-size" "small"] [Chess.fen_of_position (model |. model.game).position |> text]
        ; p []
            [ begin match model.stockfish with
                | Ready ->
                  begin match interactable with
                    | Board.Not_interactable -> Board.result_view game_status
                    | _ -> button [onClick Stockfish_request_move]
                             [text "computer move"]
                  end
                | Loading -> text "loading Stockfish"
                | Thinking -> text "Stockfish is thinking"
              end
            ]
        ]
    ; section [id "game"]
        [ game_nav_view model
        ; section [class' "scroll"]
            begin match model.route with
              | Scratch -> scratch_view::[game_view]
              | Tournament -> [tournament_view model.tournament]
              | _ -> buttons_view::[game_view]
            end
        ]
    ]


let subscriptions model =
  Sub.batch [ Board.subscriptions model.board |> Sub.map board_msg
            ; Keyboard.downs key_pressed
            ; Stockfish.stockfish stockfish_data
            ]


let main =
  Navigation.navigationProgram location_change
    { init
    ; update
    ; view
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none)
    }
