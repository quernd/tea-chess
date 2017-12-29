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
  | Back_button
  | Fwd_button
  | Random_move of Chess.move
  | Key_pressed of Keyboard.key_event
  | Jump of int
  | Validate_pgn of string
[@@bs.deriving {accessors}]


let init () =
  let url = "http://localhost:3000/fetch/https://lichess.org/api/tournament/GToVqkC9" in
  let http_cmd =
    Http.getString url
    |> Http.send
      (function
        | Result.Error _e -> Jump 0
        | Result.Ok output ->
          let system = Json.Decoder.decodeString (Json.Decoder.field "system" Json.Decoder.string) output in
          Js.log system; Validate_pgn output) in
  { position = Chess.init_position
  ; board = Board.init ()
  ; moves = Zipper.init ()
  ; ply = 0
  }, http_cmd


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
  | Back_button ->
    begin match model.position.prev with
      | Some position ->
        { model with
          moves = Zipper.back model.moves
        ; position
        ; ply = model.ply - 1}
      | _ -> model
    end, Cmd.none
  | Fwd_button ->
    begin try let (move, _san), moves = Zipper.fwd model.moves in
        { model with
          position = Chess.make_move model.position move
        ; moves
        ; ply = model.ply + 1
        }, Cmd.none
      with Zipper.End_of_list -> model, Cmd.none
    end
  | Key_pressed key_event ->
    model,
    begin match key_event.ctrl, key_event.key_code with
      | true, 66 (* Ctrl-b *) -> Cmd.msg Back_button
      | true, 70 (* Ctrl-f *) -> Cmd.msg Fwd_button
      | true, 82 (* Ctrl-r *) -> Cmd.msg Random_button
      | true, 84 (* Ctrl-t *) -> Cmd.msg Back_button
      | _ -> Cmd.none
    end
  | Jump how_many ->
    let rec jump_fwd position zipper n =
      if n <= 0 then position, zipper
      else let (move, _san), zipper' = Zipper.fwd zipper in
        jump_fwd (Chess.make_move position move) zipper' (n - 1) in
    let rec jump_back (position:Chess.position) zipper n =
      match position.prev, n with
      | Some position', n when n < 0 ->
        jump_back position' (Zipper.back zipper) (n + 1)
      | _ -> position, zipper in
    begin match how_many with
      | 0 -> model, Cmd.none
      | n -> let position, moves =
               if n > 0 then jump_fwd model.position model.moves n
               else jump_back model.position model.moves n in
        {model with position; moves; ply = model.ply + n}, Cmd.none
    end
  | Validate_pgn s ->
    let pgn = Pgn.Opal.LazyStream.of_string s |> Pgn.Opal.parse (Pgn.pgn_game) in
    let advance (position, ply, acc) (move:Pgn.pgn_move) =
      let move' = Pgn.move_of_pgn_move position move.move in
      let position' = Chess.make_move position move' in
      let san = Chess.san_of_move position move' in
      let acc' = (move', san)::acc in
      (position', ply + 1, acc')
    in
    begin match pgn with
      | Some (_, pgn', _) -> begin try
            let position, ply, past = List.fold_left advance
                (Chess.init_position, 0, []) pgn' in
            { model with position; ply; moves = (past, []) }, Cmd.none
          with e -> (Js.log e; model, Cmd.none) end
      | None -> model, Cmd.none
    end



let move_view ?(highlight=false) current_ply offset (_move, san) =
  let ply = current_ply + offset + 1 in
  let number = ply / 2
  and w_move = ply mod 2 = 0 in
  li [ classList [ "move", true
                 ; "numbered", w_move
                 ; "highlight", highlight
                 ] ]
    [ span [class' "number"] [string_of_int number |> text]
    ; span
        [ class' "move"
        ; if offset <> 0 then onClick (Jump offset) else noProp
        ] [text san]
    ]


let home_view ~highlight current_ply =
  li [ classList
         [ "move", true
         ; "highlight", highlight ] ]
    [ span
        [ class' "move"
        ; onClick (Jump (-current_ply))
        ] [text {js|\u2302|js}]
    ]


let move_list_future_view ply future =
  let rec loop offset cont = function
    | [] -> cont []
    | hd::tl ->
      loop (offset + 1)
        (fun acc -> move_view ply offset hd::acc
                    |> cont) tl
  in loop 1 (fun x -> x) future

let move_list_view ply (past, future) =
  let rec loop offset acc = function
    | [] -> acc
    | hd::tl ->
      loop (offset - 1)
        (move_view ~highlight:(offset = 0) ply offset hd::acc) tl
  in home_view ~highlight:(ply = 0) ply::
     loop 0 (move_list_future_view ply future) past
     |> ul [class' "moves"]

let view model =
  let game_status = Chess.game_status model.position in
  let interactable =
    match game_status with
    | Play move_list -> Board.Interactable (model.position.turn, move_list)
    | _ -> Board.Not_interactable
  in
  div [id "main"]
    [ div [id "board"]
        [
          [ Board.view interactable model.position.ar model.board
            |> map board_msg
          ; List.map (map board_msg) Board.buttons_view @
            [ button [onClick Random_button] [text "random move"]
            ; button [onClick Back_button] [text "back"]
            ; button [onClick Fwd_button] [text "forward"]
            ]
            |> p []
          ; Board.result_view game_status
          ] |> div [class' "fixed"]
        ]
    ; div [id "flexible"] [move_list_view model.ply model.moves]
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
