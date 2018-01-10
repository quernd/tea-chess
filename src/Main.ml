open Tea
open App

type model =
  { game : Game.model
  ; board : Board.model
  }

type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Random_button
  | Random_move of Chess.move
[@@bs.deriving {accessors}]


let init () =
  { game = Game.init
  ; board = Board.init
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


let view model =
  let open Html in
  let interactable =
    match Chess.game_status model.game.position with
    | Play move_list ->
      Board.Interactable (model.game.position.turn, move_list)
    | _ -> Board.Not_interactable in
  div []
    [ Board.view interactable model.game.position.ar model.board |> map board_msg
    ; p [] [ map board_msg Board.flip_button_view
           ; button
               [ onClick Random_button ]
               [ text "Make a random move!" ]
           ; button
               [ onClick (Game_msg Take_back) ]
               [ text "Take back" ]
           ]
    ; Game.view model.game
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
