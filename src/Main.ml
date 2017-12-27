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
    let san = Chess.legal_moves_with_san model.position |> List.assoc move in
    { model with
      position = Chess.make_move model.position move
    ; moves = (move, san)::model.moves
    }, Cmd.none
  | Takeback_button ->
    begin match model.moves, model.position.prev with
      | _::moves, Some position -> {model with moves; position}
      | _ -> model
    end, Cmd.none


let move_view (_move, san) =
  li [class' "move"] [text san]

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
    ; List.rev_map move_view model.moves |> ul [class' "moves"]
    ]


let subscriptions model =
  Board.subscriptions model.board |> Sub.map board_msg


let main =
  standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
