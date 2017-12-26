open Tea
open Tea.Html
open Tea.App

type model =
  { position : Chess.position
  ; board : Board.model
  }

type msg =
  | Board_msg of Board.msg
  | Random_button
  | Random_move of Chess.move
[@@bs.deriving {accessors}]


let init () =
  { position = Chess.init_position
  ; board = Board.init () 
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
    { model with
      position = Chess.make_move model.position move 0 }, Cmd.none


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
      ]
      |> p []
    ; Board.result_view game_status
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
