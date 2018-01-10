open Tea
open App
open Util

open Lens
open Infix

type model =
  { games : Game.model IntMap.t
  ; game : (model, Game.model) Lens.t
  ; selected : int
  ; board : Board.model
  ; lichess : Lichess.model
  ; lichess_games : Game.model StringMap.t
  }

type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Lichess_msg of Lichess.msg
  | Random_button
  | Random_move of Chess.move
  | Key_pressed of Keyboard.key_event
  | Reset_game
  | New_game
  | Switch_game of int
[@@bs.deriving {accessors}]

external alert : (string -> unit) = "alert" [@@bs.val]

let games_lens =
  { get = (fun r -> r.games)
  ; set = (fun v r -> { r with games = v })
  }

let init () =
  { games = IntMap.empty |> IntMap.add 1 Game.init
  ; game = games_lens |-- IntMapLens.for_key 1
  ; selected = 1
  ; board = Board.init
  ; lichess = Lichess.init
  ; lichess_games = StringMap.empty
  }, Cmd.none


let add_game_update_lens game model =
  let key = (IntMap.max_binding model.games |> fst) + 1 in
  let games = IntMap.add key game model.games in
  { model with games
             ; selected = key
             ; game = games_lens |-- IntMapLens.for_key key
  }


let update model = function
  | Board_msg (Move move) | Random_move move ->
    model, Game_msg (Move move) |> Cmd.msg
  | Board_msg msg ->
    let board, cmd = Board.update model.board msg in
    { model with board }, Cmd.map board_msg cmd
  | Game_msg msg ->
    let game, cmd = Game.update (model |. model.game) msg in
    model |> model.game ^= game, Cmd.map game_msg cmd
  | Lichess_msg (Load_game id) ->
    begin try let game = StringMap.find id model.lichess_games in
        add_game_update_lens game model, Cmd.none
      with Not_found ->
        let lichess, cmd = Lichess.update model.lichess (Load_game id) in
        { model with lichess }, Cmd.map lichess_msg cmd
    end
  | Lichess_msg (Game_data (game_id, Error _)) ->
    Printf.sprintf "Game %s could not be loaded!" game_id |> alert;
    model, Cmd.none
  | Lichess_msg (Game_data (game_id, Ok data)) ->
    begin match Game.game_of_pgn data with
      | Some game ->
        let lichess_games = StringMap.add game_id game model.lichess_games in
        { model with lichess_games }
        |> add_game_update_lens game, Cmd.none
      | None -> alert "Game could not be parsed!";
        model, Cmd.none
    end
  | Lichess_msg msg ->
    let lichess, cmd = Lichess.update model.lichess msg in
    { model with lichess }, Cmd.map lichess_msg cmd
  | Random_button ->
    model,
    begin match Chess.game_status (model |. model.game).position with
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
    add_game_update_lens Game.init model, Cmd.none
  | Switch_game i ->
    { model with game = games_lens |-- IntMapLens.for_key i }, Cmd.none


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


let games_picker selected games =
  let open Html in
  let option_view k _v acc =
    option' [ string_of_int k |> value
            ; Attributes.selected (selected = k) ]
      [ Printf.sprintf "Game %d" k |> text ]::acc in

  IntMap.fold option_view games []
  |> List.rev
  |> select [ int_of_string >> switch_game |> onChange ]


let view model =
  let open! Html in
  let game = model |. model.game in
  let position = game.position in
  let interactable =
    match Chess.game_status position with
    | Play move_list ->
      Board.Interactable (position.turn, move_list)
    | _ -> Board.Not_interactable in
  main []
    [ section [ id "board" ]
        [ header_nav_view
        ; Board.view interactable position.ar model.board
          |> map board_msg
        ; Game.status_view position
        ; p [] [ map board_msg Board.flip_button_view
               ; button
                   [ onClick Random_button ]
                   [ text "Random move!" ]
               ; button
                   [ onClick (Game_msg Take_back) ]
                   [ text "Take back" ]
               ]
        ]
    ; section [ id "game" ]
        [ nav [] [ ul [] [ li []
                             [ games_picker model.selected model.games ]
                         ; button
                             [ onClick Reset_game ]
                             [ text "reset game" ]
                         ; button
                             [ onClick New_game ]
                             [ text "new game" ]
                         ]
                 ]
        ; div [ class' "scroll" ]
            [ Game.view game |> map game_msg
            ; Lichess.view model.lichess |> map lichess_msg
            ]
        ]
    ]


let subscriptions model =
  [ Board.subscriptions model.board |> Sub.map board_msg
  ; Keyboard.downs key_pressed
  ] |> Sub.batch

let main =
  standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
