open Tea
open Tea.Html
open Tea.App

type 'a transfer =
  | Loading
  | Failed
  | Received of 'a

type model =
  { position : Chess.position
  ; board : Board.model
  ; moves : (Chess.move * string) Zipper.zipper
  ; ply : int
  ; tournament : (string * string list) list transfer
  }

type msg =
  | Board_msg of Board.msg
  | Random_button
  | Back_button
  | Fwd_button
  | Random_move of Chess.move
  | Key_pressed of Keyboard.key_event
  | Jump of int
  | Tournament_data of (string, string Http.error) Result.t 
[@@bs.deriving {accessors}]


let init () =
  let url = "https://lichess.org/api/tournament/GToVqkC9" in
  let init_cmd =
    Http.getString url |> Http.send tournament_data in
  { position = Chess.init_position
  ; board = Board.init ()
  ; moves = Zipper.init ()
  ; ply = 0
  ; tournament = Loading
  }, init_cmd


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
      | _, 37 (* left *) | true, 66 (* Ctrl-b *) -> Cmd.msg Back_button
      | _, 39 (* right *) | true, 70 (* Ctrl-f *) -> Cmd.msg Fwd_button
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
  | Tournament_data (Result.Error e) -> Js.log e;
    {model with tournament = Failed}, Cmd.none
  | Tournament_data (Result.Ok data) -> 
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


let buttons_view =
  List.map (map board_msg) Board.buttons_view @
  [ button [onClick Random_button] [text "random move"]
  ; button [onClick Back_button] [text "back"]
  ; button [onClick Fwd_button] [text "forward"]
  ]
  |> nav [id "buttons"]

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
  | Loading -> text "Loading tournament..."
  | Received tournament' ->
    List.map
      (fun (id, players) ->
         td [] [ a [href (Printf.sprintf "#/pgn/%s" id)] [text id]]::
         (List.map (fun player -> td [] [text player]) players) |> tr [])
      tournament'
    |> table []
  | Failed -> text "Tournament could not be loaded."

let game_nav_view _model =
  let game_nav_item current link label =
    li [ if current then class' "current" else noProp ]
      [ if current then text label else a [href link] [text label] ] in
  nav [class' "top tabbed"]
    [ ul []
        [ game_nav_item true "#/game" "Game"
        ; game_nav_item false "#/tournament" "Tournament"
        ]
    ]

let view model =
  let game_status = Chess.game_status model.position in
  let interactable =
    match game_status with
    | Play move_list -> Board.Interactable (model.position.turn, move_list)
    | _ -> Board.Not_interactable
  in
  main []
    [ section [id "board"]
        [ header_nav_view
        ; Board.view interactable model.position.ar model.board
          |> map board_msg
        ; buttons_view
          (* ; Board.result_view game_status *)
        ]
    ; section [id "game"]
        [ game_nav_view model
        ; section [class' "scroll"] (* [move_list_view model.ply model.moves] *)
            [tournament_view model.tournament]
        ]
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
