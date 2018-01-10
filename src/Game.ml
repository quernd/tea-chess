open Tea

type san = string

type move =
  { move : Chess.move
  ; san : san
  }

type model =
  { position : Chess.position
  ; moves : move Zipper.zipper
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
  }


let simple_move move san =
  { move = move
  ; san = san
  }


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
        let position = Chess.make_move model.position move 0 in
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


let view model =
  let open Html in
  div []
    [ status_view model.position
    ; move_list_view model.position.number model.moves
    ]


