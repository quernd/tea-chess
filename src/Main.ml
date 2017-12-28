open Tea
open Tea.Html
open Tea.App

type model =
  { position : Chess.position
  ; board : Board.model
  ; moves : (Chess.move * string) list
  }

type msg =
  | Board_msg of Board.msg
  | Random_button
  | Takeback_button
  | Random_move of Chess.move
  | Key_pressed of Keyboard.key_event
[@@bs.deriving {accessors}]


let init () =
  { position = Chess.init_position
  ; board = Board.init ()
  ; moves = []
  }, Cmd.none


let update model = function
  | Board_msg (Internal_msg msg) ->
    let board', cmd = Board.update model.board (Internal_msg msg) in
    { model with
      board = board'
    }, Cmd.map board_msg cmd
  | Random_button ->
    model,
    begin match Chess.game_status model.position with
      | Play move_list ->
        List.length move_list
        |> Random.int 0
        |> Random.generate
          (fun random_number ->
             List.nth move_list random_number |> random_move)
      | _ -> Cmd.none
    end
  | Random_move move | Board_msg (Move move) ->
    let san = Chess.san_of_move model.position move in
    { model with
      position = Chess.make_move model.position move
    ; moves = (move, san)::model.moves
    }, Cmd.none
  | Takeback_button ->
    begin match model.moves, model.position.prev with
      | _::moves, Some position -> {model with moves; position}
      | _ -> model
    end, Cmd.none
  | Key_pressed key_event -> Js.log key_event;
    model,
    begin match key_event.ctrl, key_event.key_code with
      | true, 82 (* Ctrl-r *) -> Cmd.msg Random_button
      | true, 84 (* Ctrl-t *) -> Cmd.msg Takeback_button
      | _ -> Cmd.none
    end

let move_view ply (_move, san) =
  let number = ply / 2 + 1
  and w_move = ply mod 2 = 0 in
  li [ classList [ "move", true
                 ; "numbered", w_move
                 ] ]
    [ span [class' "number"] [string_of_int number |> text]
    ; span [class' "move"] [text san]
    ]

let move_list_view moves =
  List.rev moves |> List.mapi move_view |> ul [class' "moves"]

let view model =
  let game_status = Chess.game_status model.position in
  let interactable =
    match game_status with
    | Play move_list -> Board.Interactable (model.position.turn, move_list)
    | _ -> Board.Not_interactable
  in
  div []
    [ Board.view interactable model.position.ar model.board
      |> map board_msg
    ; List.map (map board_msg) Board.buttons_view @
      [ button [onClick Random_button] [text "random move"]
      ; button [onClick Takeback_button] [text "take back"]
      ]
      |> p []
    ; Board.result_view game_status
    ; move_list_view model.moves
    ]


let subscriptions model =
  Sub.batch
    [ Board.subscriptions model.board |> Sub.map board_msg
    ; Keyboard.downs key_pressed ]


let main =
  standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
