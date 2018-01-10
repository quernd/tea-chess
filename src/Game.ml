open Tea

type san = string

type move =
  { move : Chess.move
  ; san : san
  }

type model =
  { position : Chess.position
  ; moves : move Zipper.zipper
  ; header : (string * string) list
  ; result : Pgn.result
  }

type msg =
  | Move of Chess.move
  | Take_back
  | Forward
  | Jump of int
[@@bs.deriving {accessors}]    

let init =
  { position = Chess.init_position
  ; moves = Zipper.init
  ; header = []
  ; result = None
  }


let simple_move move san =
  { move = move
  ; san = san
  }


let string_of_result = function
  | Some (Chess.Win White) -> "1-0"
  | Some (Chess.Win Black) -> "0-1"
  | Some Chess.Draw -> "1/2-1/2"
  | _ -> "*"

let game_of_pgn string =
  let make_pgn_move (position, moves) pgn_move =
    let move = Pgn.move_of_pgn_move position pgn_move in
    let san = Chess.san_of_move position move in
    Chess.make_move' position move,
    Zipper.fwd' (simple_move move san) moves in
  try match Pgn.parse_pgn string with
    | Some pgn_game ->
      let header = pgn_game.header and result = pgn_game.result in
      let position, moves = List.fold_left make_pgn_move
          (Chess.init_position, Zipper.init) pgn_game.moves in
      Some { position; moves; header; result }
    | None -> None
  with _ -> None


let jump model how_many =

  let rec jump_fwd position zipper n =
    if n <= 0 then position, zipper
    else let move, zipper' = Zipper.fwd zipper in
      jump_fwd (Chess.make_move' position move.move) zipper' (n - 1) in
  let rec jump_back (position:Chess.position) zipper n =
    match position.prev, n with
    | Some position', n when n < 0 ->
      jump_back position' (Zipper.back zipper) (n + 1)
    | _ -> position, zipper in

  try match how_many with
    | 0 -> model
    | n -> let position, moves =
             if n > 0 then jump_fwd model.position model.moves n
             else jump_back model.position model.moves n in
      { model with position; moves }
  with _ -> model


let update model = function
  | Move move ->
    begin try
        let san = Chess.san_of_move model.position move in
        let position = Chess.make_move' model.position move in
        { model with position
                   ; moves = Zipper.fwd' (simple_move move san) model.moves 
        }, Cmd.none
      with Chess.Illegal_move -> model, Cmd.none
    end
  | Take_back -> jump model (-1), Cmd.none
  | Forward -> jump model 1, Cmd.none
  | Jump how_many -> jump model how_many, Cmd.none


let move_list_view ply (past, future) =
  let open Html in

  let home_view ~highlight offset =
    li [ classList
           [ "move", true
           ; "highlight", highlight ]
       ; if offset <> 0 then onClick (Jump offset) else noProp
       ]
      [ span [ class' "move" ] [ text {js|\u2302|js} ]
      ] in

  let move_view ?(highlight=false) ply' offset move =
    let ply = ply' + offset + 1 in
    let turn = if ply mod 2 = 0 then Chess.White else Chess.Black in
    let number = ply / 2 in
    li [ classList [ "move", true
                   ; "white", turn = Chess.White
                   ; "black", turn = Chess.Black
                   ; "highlight", highlight
                   ]
       ; if offset <> 0 then onClick (Jump offset) else noProp
       ]
      [ span [ class' "number" ] [ string_of_int number |> text ]
      ; span [ class' "move" ] [ text move.san ]
      ] in

  let move_list_future_view ply future =
    let rec loop offset cont = function
      | [] -> cont []
      | hd::tl ->
        loop (offset + 1)
          (fun acc -> move_view ply offset hd::acc |> cont) tl in
    loop 1 (fun x -> x) future in

  let rec loop offset acc = function
    | [] -> acc
    | hd::tl ->
      loop (offset - 1)
        (move_view ~highlight:(offset = 0) ply offset hd::acc) tl in

  home_view ~highlight:(ply = 0) (-ply)::
  loop 0 (move_list_future_view ply future) past
  |> ul [class' "moves"]


let status_view position =
  let open Html in
  p []
    [ begin match Chess.game_status position with
        | Chess.Win Black -> "Black wins by checkmate!"
        | Chess.Win White -> "White wins by checkmate!"
        | Chess.Draw -> "It's a draw!"
        | Chess.Play move_list ->
          Printf.sprintf "It is %s's move,  %d legal moves"
            (match position.turn with | Black -> "Black"
                                      | White -> "White")
            (List.length move_list)
      end |> text
    ]


let header_view pgn_header =
  let open Html in
  let key_value_view (k, v) =
    li [] [ label [] [ [ text k ] |> span [] ]
          ; span [] [ text v ]
          ] in
  List.filter 
    (fun (k, _) -> k = "White" || k = "Black") pgn_header
  |> List.map key_value_view
  |> ul [class' "pgnheader"]

let result_view result =
  let open Html in
  p [ class' "result"]
    [ string_of_result result |> text ]

let view model =
  let open Html in
  div [ class' "pgn" ]
    [ header_view model.header
    ; move_list_view model.position.number model.moves
    ; result_view model.result
    ]


let pgn_of_game model =
  let past, future = model.moves in
  let sans = List.rev_append past future
             |> List.map (fun move -> move.san) in
  let header =
    List.map (fun (k, v) -> Printf.sprintf "[%s \"%s\"]" k v) model.header
    |> String.concat "\n" in
  header ^ "\n\n" ^
  String.concat " " sans ^ " " ^
  (string_of_result model.result)
