open Tea
open Tea.Html

type san = string

type move = (Chess.move * san)

type model =
  { moves : move Zipper.tree_zipper
  ; position : Chess.position
  }

type action = Fwd | Back | Down | Next | Prev | Up

type msg =
  | Random_button
  | Back_button
  | Fwd_button
  | Make_move of Chess.move
  | Move_clicked of action list
[@@bs.deriving {accessors}]

type c =
  { ply : int
  ; actions : action list
  }

let init () =
  { position = Ochess.init_position
  ; moves = Zipper.tree_init ()
  }

exception No_prev_position

let game_back model =
  match model.position.prev with
  | Some position ->
    begin try { moves = Zipper.tree_back model.moves
              ; position
              }
      with Zipper.Beginning_of_list ->
        { moves = Zipper.tree_up model.moves |> snd |> Zipper.tree_back
        ; position
        }
    end
  | _ -> raise No_prev_position

let game_fwd model =
  try let (move, _san), moves = Zipper.tree_fwd model.moves in
    { position = Chess.make_move model.position move
    ; moves
    }
  with Zipper.End_of_list -> model

let game_make_move move model =
  let san = Chess.san_of_move model.position move in
  let _, moves = Zipper.tree_fwd' (move, san) model.moves in
  { position = Chess.make_move model.position move
  ; moves
  }

let game_rewind_and_make_move f model =
  try let move, moves = f model.moves in
    match model.position.prev with
    | Some position ->
      { position = Chess.make_move position (fst move)
      ; moves
      }
    | None -> raise No_prev_position
  with _ -> model

let game_next = game_rewind_and_make_move Zipper.tree_next
let game_prev = game_rewind_and_make_move Zipper.tree_prev
let game_down = game_rewind_and_make_move Zipper.tree_down
let game_up = game_rewind_and_make_move Zipper.tree_up



let fun_of_action = function
  | Fwd -> game_fwd
  | Back -> game_back
  | Down -> game_down
  | Next -> game_next
  | Prev -> game_prev
  | Up -> game_up

let id' = fun x -> x

let (<<) f g x = f (g x)
let (>>) f g x = g (f x)

let fun_of_actions actions =
  List.fold_left (fun acc a -> acc << (fun_of_action a)) id' actions


let string_of_action = function
  | Fwd -> "f"
  | Back -> "b"
  | Down -> "v"
  | Next -> "n"
  | Prev -> "p"
  | Up -> "u"

let string_of_actions actions =
  List.fold_left (fun acc a -> string_of_action a ^ acc) "" actions


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
    game_make_move move model, Cmd.none
  | Back_button ->
    begin try game_back model with _ -> model end, Cmd.none
  | Fwd_button ->
    begin try game_fwd model with _ -> model end, Cmd.none
  | Move_clicked actions -> Js.log (string_of_actions actions);
    model |> fun_of_actions actions, Cmd.none



let buttons_view =
  [ button [onClick Random_button] [text "random move"]
  ; button [onClick Back_button] [text "back"]
  ; button [onClick Fwd_button] [text "forward"]
  ]


let game_of_pgn pgn =
  let rec line_of_pgn position = function
    | [] -> []
    | hd::tl ->
      let node, position' = node_of_pgn position hd in
      node::line_of_pgn position' tl
  and variation_of_pgn position = function
    | hd::tl ->
      let move, position' = move_of_pgn position hd in
      Zipper.Var (move, line_of_pgn position' tl)
    | _ -> raise Pgn.Parse_error
  and move_of_pgn position (pgn_move:Pgn.pgn_move) =
    let move = Pgn.move_of_pgn_move position pgn_move.move in
    let san = Chess.san_of_move position move in
    (move, san), Chess.make_move position move
  and node_of_pgn position (pgn_move:Pgn.pgn_move) =
    let rav = List.map (variation_of_pgn position) pgn_move.rav in
    let move, position' = move_of_pgn position pgn_move in
    Zipper.Node (move, rav), position'

  in

  let moves =
    match Pgn.parse_pgn pgn with
    | None -> raise Pgn.Parse_error
    | Some (_headers, line, _result) ->
      line_of_pgn Ochess.init_position line in
  { position = Ochess.init_position
  ; moves = (Zipper.Main_line, []), moves
  }


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
  |> Printf.sprintf "%s *"
(* TODO: add headers *)


(* fold_right' is like right fold, but carrying an additional 
   accumulator c that is updated by g and applied together with f *)
let fold_right' f g =  (* c l *)
  let rec loop cont c = function
    | hd::tl ->
      let c' = g c in
      loop (fun acc' -> cont (f c hd::acc')) c' tl
    | [] -> cont []
  in loop id'

(* fold_left' is like left fold, but carrying an additional 
   accumulator c that is updated by g and applied together with f *)
let fold_left' f g acc c l =  
  List.fold_left
    (fun (acc, c) item -> f c item::acc, g c)
    (acc, c) l


let view model =

  let make_line = ul [class' "moves"] in
  let make_variations = function
    | [] -> noNode
    | variations -> ol [class' "variations"] variations in
  let make_variation moves = li [class' "variation"] [make_line moves] in


  let fwd c = {ply = c.ply + 1; actions = Fwd::c.actions} in
  let back c = {ply = c.ply - 1; actions = Back::c.actions} in
  let down c = {c with actions = Down::c.actions} in
  let prev c = {c with actions = Prev::c.actions} in
  let next c = {c with actions = Next::c.actions} in
  let up c = {c with actions = Up::c.actions} in

  let home_view c =
    li [ classList [ "move", true
                   ; "highlight", c.actions = []
                   ]
       ]
      [ span
          [ class' "move"
          ; onClick (Move_clicked c.actions) ]
          [ text {js|\u2302|js} ]
      ] in

  let rec move_view c move variations =
    let number = c.ply / 2 + 1
    and w_move = c.ply mod 2 = 0 in
    li [ classList
           [ "move", true
           ; "white", w_move
           ; "black", not w_move
           ; "variations", variations <> noNode
           ; "highlight", c.actions = []
           ]
       ]
      [ span [class' "number"]
          [ string_of_int number |> text ]
      ; span
          [ class' "move"
          ; onClick (Move_clicked c.actions)
          ]
          [ snd move |> text ]
      ; variations
      ]
  and node_view c = function
    | Zipper.Node (move, variations) ->
      move_view c move (variations_view c variations |> make_variations)
  and variations_view c variations =
    fold_right' variation_view next (down c) variations
  and variation_view c = function
    | Zipper.Var (move, line) ->
      move_view c move noNode::future_view (fwd c) line |> make_variation
  and future_view c l = fold_right' node_view fwd c l
  and past_view c l acc = fold_left' node_view back acc c l in


  let rec tree_context_view c ((context, past), future) inner =
    let acc, c' = inner::future_view (fwd c) future
                  |> past_view (back c) past in
    line_context_view c' context acc

  and line_context_view c context inner =
    match context with
    | Zipper.Main_line -> home_view c::inner
    | Zipper.Var_line (context, main, left, var_move, right, future) ->
      let this_var = move_view c var_move noNode::inner |> make_variation in
      let variations =
        let right' =
          this_var::fold_right' variation_view next (next c) right in
        fold_left' variation_view prev right' (prev c) left |> fst
        |> make_variations in
      move_view (up c) main variations
      |> tree_context_view (up c) (context, future)
  in
  let (context, past), future = model.moves in
  let c = {ply = model.position.number - 1; actions = []} in
  let acc, c' = future_view (fwd c) future
                |> past_view c past in
  line_context_view c' context acc
  |> make_line
