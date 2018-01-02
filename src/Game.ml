open Tea
open Tea.Html

type san = string

type move = (Chess.move * san)

type model =
  { moves : move Zipper.tree_zipper
  ; position : Chess.position
  ; ply : int
  }

type msg =
  | Random_button
  | Back_button
  | Fwd_button
  | Make_move of Chess.move
  | Jump of int
[@@bs.deriving {accessors}]


let init () =
  { position = Ochess.init_position
  ; moves = Zipper.tree_init ()
  ; ply = 0
  }


let update model = function
  | Random_button ->
    model,
    begin match Chess.game_status model.position with
      | Play move_list ->
        List.length move_list
        |> Random.int 0
        |> Random.generate
          (fun random_number ->
             List.nth move_list random_number |> make_move)
      | _ -> Cmd.none
    end
  | Make_move move ->
    let san = Chess.san_of_move model.position move in
    let _, moves = Zipper.tree_fwd' (move, san) model.moves in
    { position = Chess.make_move model.position move
    ; moves
    ; ply = model.ply + 1
    }, Cmd.none
  | Back_button ->
    begin match model.position.prev with
      | Some position ->
        { moves = Zipper.tree_back model.moves
        ; position
        ; ply = model.ply - 1}
      | _ -> model
    end, Cmd.none
  | Fwd_button ->
    begin try let (move, _san), moves = Zipper.tree_fwd model.moves in
        { position = Chess.make_move model.position move
        ; moves
        ; ply = model.ply + 1
        }, Cmd.none
      with Zipper.End_of_list -> model, Cmd.none
    end
  | Jump how_many ->
    let rec jump_fwd position zipper n =
      if n <= 0 then position, zipper
      else let (move, _san), zipper' = Zipper.tree_fwd zipper in
        jump_fwd (Chess.make_move position move) zipper' (n - 1) in
    let rec jump_back (position:Chess.position) zipper n =
      match position.prev, n with
      | Some position', n when n < 0 ->
        jump_back position' (Zipper.tree_back zipper) (n + 1)
      | _ -> position, zipper in
    begin match how_many with
      | 0 -> model, Cmd.none
      | n -> let position, moves =
               if n > 0 then jump_fwd model.position model.moves n
               else jump_back model.position model.moves n in
        {position; moves; ply = model.ply + n}, Cmd.none
    end


let buttons_view =
  [ button [onClick Random_button] [text "random move"]
  ; button [onClick Back_button] [text "back"]
  ; button [onClick Fwd_button] [text "forward"]
  ]


let move_view ?(highlight=false) current_ply offset
    (Zipper.Node ((_move, san), _var)) =
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

let view model =
  let (_context, past), future = model.moves in
  move_list_view model.ply (past, future)
  |> ul [class' "moves"]
