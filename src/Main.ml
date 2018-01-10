open Tea
open App

type model =
  { game : Game.model
  ; board : Board.model
  ; lichess : Lichess.model
  }

type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Lichess_msg of Lichess.msg
  | Random_button
  | Random_move of Chess.move
  | Key_pressed of Keyboard.key_event
[@@bs.deriving {accessors}]

external alert : (string -> unit) = "alert" [@@bs.val]

let init () =
  { game = Game.init
  ; board = Board.init
  ; lichess = Lichess.init
  }, Cmd.none


let update model = function
  | Board_msg (Move move) | Random_move move ->
    model, Game_msg (Move move) |> Cmd.msg
  | Board_msg msg ->
    let board, cmd = Board.update model.board msg in
    { model with board }, Cmd.map board_msg cmd
  | Game_msg msg ->
    let game, cmd = Game.update model.game msg in
    { model with game }, Cmd.map game_msg cmd
  | Lichess_msg (Game_data (Error _)) ->
    alert "Game could not be loaded!";
    model, Cmd.none
  | Lichess_msg (Game_data (Ok data)) ->
    begin match Game.game_of_pgn data with
      | Some game -> { model with game }, Cmd.none
      | None -> alert "Game could not be parsed!";
        model, Cmd.none
    end
  | Lichess_msg msg ->
    let lichess, cmd = Lichess.update model.lichess msg in
    { model with lichess }, Cmd.map lichess_msg cmd
  | Random_button ->
    model,
    begin match Chess.game_status model.game.position with
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
      | true, 82 (* Ctrl-r *) -> Cmd.msg Random_button
      | _ -> Cmd.none
    end





let header_nav_view =
  let open Html in
  let link ?(home=false) link description =
    li [ if home then class' "home" else noProp ]
      [ a [ href link ] [ text description ] ] in

  nav [ id "main-nav" ]
    [ ul [] [ link ~home:true "#/" "TEA-Chess"
            ; link "https://quernd.github.io/tutorials/tea-chess" "Tutorial"
            ; link "https://github.com/quernd/tea-chess" "Code"
            ]
    ]


let view model =
  let open Html in
  let interactable =
    match Chess.game_status model.game.position with
    | Play move_list ->
      Board.Interactable (model.game.position.turn, move_list)
    | _ -> Board.Not_interactable in
  main []
    [ section [ id "board" ]
        [ header_nav_view
        ; Board.view interactable model.game.position.ar model.board
          |> map board_msg
        ; Game.status_view model.game.position
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
        [ div [ class' "scroll" ]
            [ Game.view model.game |> map game_msg
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
