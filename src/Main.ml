open Tea
open Tea.Html
open Tea.App

type model =
  { position : Chess.position
  ; board : Board.model
  ; moves : (Chess.move * string) Zipper.zipper
  ; ply : int
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
  ; moves = Zipper.init ()
  ; ply = 0
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
    ; moves = Zipper.fwd' (move, san) model.moves
    ; ply = model.ply + 1
    }, Cmd.none
  | Takeback_button ->
    begin match model.position.prev with
      | Some position ->
        { model with
          moves = Zipper.back model.moves
        ; position
        ; ply = model.ply - 1}
      | _ -> model
    end, Cmd.none
  | Key_pressed key_event ->
    model,
    begin match key_event.ctrl, key_event.key_code with
      | true, 82 (* Ctrl-r *) -> Cmd.msg Random_button
      | true, 84 (* Ctrl-t *) -> Cmd.msg Takeback_button
      | _ -> Cmd.none
    end

let move_view ?(highlight=false) ply (_move, san) =
  let number = ply / 2 + 1
  and w_move = ply mod 2 = 0 in
  li [ classList [ "move", true
                 ; "numbered", w_move
                 ; "highlight", highlight
                 ] ]
    [ span [class' "number"] [string_of_int number |> text]
    ; span [class' "move"] [text san]
    ]


let move_list_future_view ply future =
  let rec loop offset cont = function
    | [] -> cont []
    | hd::tl ->
      loop (offset + 1)
        (fun acc -> move_view (ply + offset) hd::acc
                    |> cont) tl
  in loop 0 (fun x -> x) future

let move_list_view ply (past, future) =
  let rec loop offset acc = function
    | [] -> acc
    | hd::tl -> loop (offset + 1)
                  (move_view ~highlight:(offset = 1) (ply - offset) hd::acc) tl
  in loop 1 (move_list_future_view ply future) past
     |> ul [class' "moves"]


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
    ; move_list_view model.ply model.moves
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
