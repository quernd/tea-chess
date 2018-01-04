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
[@@bs.deriving {accessors}]

type action = Fwd | Back | Down | Next | Prev | Up

type c =
  { ply : int
  ; actions : action list
  ; depth : int
  }

let init () =
  { position = Ochess.init_position
  ; moves = Zipper.tree_init ()
  ; ply = 0
  }

exception No_prev_position

let back model =
  match model.position.prev with
  | Some position ->
    begin try { moves = Zipper.tree_back model.moves
              ; position
              ; ply = model.ply - 1}
      with Zipper.Beginning_of_list ->
        { moves = Zipper.tree_up model.moves |> snd |> Zipper.tree_back
        ; position
        ; ply = model.ply - 1}
    end
  | _ -> raise No_prev_position

let fwd model =
  try let (move, _san), moves = Zipper.tree_fwd model.moves in
    { position = Chess.make_move model.position move
    ; moves
    ; ply = model.ply + 1
    }
  with Zipper.End_of_list -> model

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
    begin try back model with _ -> model end, Cmd.none
  | Fwd_button ->
    begin try fwd model with _ -> model end, Cmd.none



let buttons_view =
  [ button [onClick Random_button] [text "random move"]
  ; button [onClick Back_button] [text "back"]
  ; button [onClick Fwd_button] [text "forward"]
  ]


let pgn_of_game (game:model) =

  let line moves = String.concat " " moves in
  let parens moves = line moves |> Printf.sprintf "(%s)" in

  let rec pgn_of_move move variations =
    snd move::variations |> line
  and pgn_of_node = function
    | Zipper.Node (move, variations) ->
      pgn_of_move move (pgn_of_variations variations)
  and pgn_of_variations variations =
    List.map pgn_of_variation variations
  and pgn_of_variation = function
    | Zipper.Var (move, line) ->
      (pgn_of_move move []::pgn_of_line line) |> parens
  and pgn_of_line line =
    List.map pgn_of_node line in

  let rec pgn_of_tree_context ((context, past), future) inner =
    inner::pgn_of_line future
    |> List.rev_append (pgn_of_line past)
    |> pgn_of_line_context context

  and pgn_of_line_context context inner =
    match context with
    | Zipper.Main_line -> inner
    | Zipper.Var_line (context, main, left, var_move, right, future) ->
      let this_var = pgn_of_move var_move []::inner |> parens in
      let variations =
        List.rev_append (pgn_of_variations left)
          (this_var::pgn_of_variations right) in
      snd main::variations |> line
      |> pgn_of_tree_context (context, future)
  in

  let (context, past), future = game.moves in
  pgn_of_line future
  |> List.rev_append (pgn_of_line past)
  |> pgn_of_line_context context
  |> line
(* TODO: add headers *)


let id x = x

(* fold_right' is like right fold, but carrying an additional 
   accumulator c that is updated by g and applied together with f *)
let fold_right' f g =  (* c l *)
  let rec loop cont c = function
    | hd::tl ->
      let c' = g c in
      loop (fun acc' -> cont (f c hd::acc')) c' tl
    | [] -> cont []
  in loop id

(* fold_left' is like left fold, but carrying an additional 
   accumulator c that is updated by g and applied together with f *)
let fold_left f g acc c l =  
  List.fold_left
    (fun (acc, c) item -> f c item::acc, g c)
    (acc, c) l



let view model =

  let make_line = ul [class' "moves"] in
  let make_variations = function
    | [] -> noNode
    | variations -> ol [class' "variations"] variations in
  let make_variation moves = li [class' "variation"] [make_line moves] in


  let fwd_c c = {c with ply = c.ply + 1} in
  let back_c c = {c with ply = c.ply - 1} in


  let rec move_view c move variations =
    let number = c.ply / 2 + 1
    and w_move = c.ply mod 2 = 0 in
    li [ classList
           [ "move", true
           ; "numbered", w_move
           ; "white", w_move
           ; "black", not w_move
           ; "variations", variations <> noNode
           ]
       ]
      [ span [class' "number"]
          [ string_of_int number |> text ]
      ; span [class' "move"]
          [ snd move |> text ]
      ; variations
      ]
  and node_view c = function
    | Zipper.Node (move, variations) ->
      move_view c move (variations_view c variations |> make_variations)
  and variations_view c variations =
    List.map (variation_view c) variations
  and variation_view c = function
    | Zipper.Var (move, line) ->
      (move_view c move noNode::future_view (fwd_c c) line) |> make_variation
  and future_view c l = fold_right' node_view fwd_c c l
  and past_view c l acc = fold_left node_view back_c acc c l in


  let rec tree_context_view c ((context, past), future) inner =
    let acc, c' = inner::future_view (fwd_c c) future
                  |> past_view (back_c c) past in
    line_context_view c' context acc

  and line_context_view c context inner =
    match context with
    | Zipper.Main_line -> inner
    | Zipper.Var_line (context, main, left, var_move, right, future) ->
      let this_var = move_view c var_move noNode::inner |> make_variation in
      let variations =
        List.rev_append (variations_view c left)
          (this_var::variations_view c right)
        |> make_variations in
      move_view c main variations
      |> tree_context_view c (context, future)
  in
  let (context, past), future = model.moves in
  let c = {ply = model.ply - 1; actions = []; depth = 0} in
  let acc, c' = future_view (fwd_c c) future
                |> past_view c past in
  line_context_view c' context acc
  |> make_line
