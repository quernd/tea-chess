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
  | Board_msg msg ->
    let board', cmd = Board.update model.board msg in
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
  | Random_move move ->
    { model with
      position = Chess.make_move model.position move 0 }, Cmd.none


let view model =
  div []
    [ Board.view model.position.ar model.board
      |> map board_msg
    ; List.map (map board_msg) Board.buttons_view @
      [ button [onClick Random_button] [text "random move"]
      ]
      |> p []
    ; Chess.game_status model.position |> Board.result_view 
    ]


let subscriptions _model =
  Sub.none


let main =
  standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
