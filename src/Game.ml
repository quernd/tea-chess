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
        begin try { moves = Zipper.tree_back model.moves
                  ; position
                  ; ply = model.ply - 1}
          with Zipper.Beginning_of_list ->
            { moves = Zipper.tree_up model.moves |> snd |> Zipper.tree_back
            ; position
            ; ply = model.ply - 1}
        end
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
    begin match inner with
      | Some a -> a::pgn_of_line future
      | None -> pgn_of_line future
    end
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
      Some (snd main::variations |> line)
      |> pgn_of_tree_context (context, future)
  in

  (pgn_of_tree_context game.moves None |> line)
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

  let rec move_view move variations =
    li [class' "move"]
      [ snd move |> text
      ; variations
      ]
  and node_view = function
    | Zipper.Node (move, variations) ->
      move_view move (variations_view variations |> make_variations)
  and variations_view variations =
    List.map variation_view variations
  and variation_view = function
    | Zipper.Var (move, line) ->
      (move_view move noNode::line_view line) |> make_variation
  and line_view line =
    List.map node_view line in

  let rec tree_context_view ((context, past), future) inner =
    inner::line_view future
    |> List.rev_append (line_view past)
    |> line_context_view context

  and line_context_view context inner =
    match context with
    | Zipper.Main_line -> inner
    | Zipper.Var_line (context, main, left, var_move, right, future) ->
      let this_var = move_view var_move noNode::inner |> make_variation in
      let variations =
        List.rev_append (variations_view left)
          (this_var::variations_view right)
        |> make_variations in
      move_view main variations
      |> tree_context_view (context, future)
  in
  tree_context_view model.moves noNode
  |> make_line
