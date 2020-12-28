open Tea
open Util

type san = string
type comment = string

type move =
  { move : Chess.move
  ; san : san
  ; pre_comments : comment list
  ; post_comments : comment list
  ; nags : Pgn.nag list
  }

type model =
  { position : Chess.position
  ; moves : move Zipper.tree_zipper
  ; header : (string * string) list
  ; result : Pgn.result
  }

type action = Fwd | Back | Down | Next | Prev | Up

type msg =
  | Move of Chess.move
  | Take_back
  | Forward
  | Jump of action list
[@@bs.deriving {accessors}]

type acc =
  { ply : int
  ; actions : action list
  }

let init_with_position position =
  { position
  ; moves = Zipper.tree_init ()
  ; header = []
  ; result = None              
  }

let init_with_fen fen =
  let position = Chess.FEN.position_of_fen fen in
  init_with_position position

let init = init_with_position Chess.init_position

exception No_prev_position

let simple_move move san =
  { move
  ; san
  ; pre_comments = []
  ; post_comments = []
  ; nags = []
  }

let game_back model =
  match model.position.prev with
  | Some position ->
    begin try { model with
                moves = Zipper.tree_back model.moves
              ; position
              }
      with Zipper.Beginning_of_list ->
        { model with
          moves = Zipper.tree_up model.moves |> snd |> Zipper.tree_back
        ; position
        }
    end
  | _ -> raise No_prev_position

let game_fwd model =
  try let move, moves = Zipper.tree_fwd model.moves in
    { model with
      position = Chess.make_move' model.position move.move
    ; moves
    }
  with Zipper.End_of_list -> model

let game_make_move move model =
  let san = Chess.san_of_move model.position move in
  let _, moves = Zipper.tree_fwd' (simple_move move san) model.moves in
  { model with
    position = Chess.make_move' model.position move
  ; moves
  }

let game_rewind_and_make_move f model =
  try let move, moves = f model.moves in
    match model.position.prev with
    | Some position ->
      { model with
        position = Chess.make_move' position move.move
      ; moves
      }
    | None -> raise No_prev_position
  with _ -> model

let game_next = game_rewind_and_make_move Zipper.tree_next
let game_prev = game_rewind_and_make_move Zipper.tree_prev
let game_down = game_rewind_and_make_move Zipper.tree_down
let game_up = game_rewind_and_make_move Zipper.tree_up

let id = (fun x -> x)

let fun_of_action = function
  | Fwd -> game_fwd
  | Back -> game_back
  | Down -> game_down
  | Next -> game_next
  | Prev -> game_prev
  | Up -> game_up

let fun_of_actions actions =
  List.fold_left (fun acc a -> acc <<< (fun_of_action a)) id actions


let string_of_action = function
  | Fwd -> "f"
  | Back -> "b"
  | Down -> "v"
  | Next -> "n"
  | Prev -> "p"
  | Up -> "u"

let string_of_actions actions =
  List.fold_left (fun acc a -> string_of_action a ^ acc) "" actions

let simple_move move san =
  { move = move
  ; san = san
  ; pre_comments = []
  ; post_comments = []
  ; nags = []
  }


let string_of_result = function
  | Some (Chess.Win White) -> "1-0"
  | Some (Chess.Win Black) -> "0-1"
  | Some Chess.Draw -> "1/2-1/2"
  | _ -> "*"

let game_of_pgn' pgn =
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
  and move_of_pgn position (pgn_move:Pgn.move) =
    let move = Pgn.move_of_pgn_move position pgn_move.move in
    let san = Chess.san_of_move position move in
    { move
    ; san
    ; pre_comments = pgn_move.pre_comments
    ; post_comments = pgn_move.post_comments
    ; nags = pgn_move.nags
    }, Chess.make_move' position move
  and node_of_pgn position (pgn_move:Pgn.move) =
    let rav = List.map (variation_of_pgn position) pgn_move.rav in
    let move, position' = move_of_pgn position pgn_move in
    Zipper.Node (move, rav), position'

  in

  let game = 
    try Pgn.parse_pgn pgn |> Option.get
    with Invalid_argument _ -> raise Pgn.Parse_error in
  let init_fen =
    try Some (List.assoc "FEN" game.header)
    with Not_found -> None in

  let init_position =
    Option.default_map
      Chess.FEN.position_of_fen Chess.init_position init_fen in

  let moves = line_of_pgn init_position game.moves in
  { position = init_position
  ; header = game.header
  ; moves = (Zipper.Main_line, []), moves
  ; result = game.result
  }


let game_of_pgn pgn =
  try Some (game_of_pgn' pgn)
  with _ -> None 


let pgn_of_game model =
  let acc = model.position.number - 1 in
  let fwd ply = ply + 1 in
  let back ply = ply - 1 in
  let make_line moves = String.concat " " moves in
  let make_variation moves = Printf.sprintf "(%s)" moves in
  let make_variations var = var in
  let make_comment comment = Printf.sprintf "{%s}" comment in
  let make_move acc move variations =
    let number = acc / 2 + 1
    and w_move = acc mod 2 = 0 in
    let pre_comments = List.map make_comment move.pre_comments in
    let post_comments = List.map make_comment move.post_comments in
    let nags = List.map Nag.pgn_of_nag move.nags |> String.concat " " in
    pre_comments @
    (if w_move then
       Printf.sprintf "%d." number::move.san::nags::post_comments
     else move.san::post_comments) @ variations
    |> String.concat " " in
  let make_mainline _acc line = line in
  let moves =
    Zipper.fold_zipper
      ~make_mainline ~make_line ~make_variation ~make_variations ~make_move
      ~acc ~fwd ~back ~up:id ~down:id ~next:id ~prev:id model.moves in
  let headers =
    List.map (fun (k, v) -> Printf.sprintf "[%s \"%s\"]" k v) model.header
    |> String.concat "\n" in
  Printf.sprintf "%s\n\n%s %s" headers moves (string_of_result model.result)


let update model = function
  | Move move ->
    game_make_move move model, Cmd.none
  | Take_back ->
    begin try game_back model with _ -> model end, Cmd.none
  | Forward ->
    begin try game_fwd model with _ -> model end, Cmd.none
  | Jump actions ->
    model |> fun_of_actions actions, Cmd.none


let move_list_view model =
  let open Html in
  let acc = {ply = model.position.number - 1; actions = []} in
  let fwd acc = {ply = acc.ply + 1; actions = Fwd::acc.actions} in
  let back acc = {ply = acc.ply - 1; actions = Back::acc.actions} in
  let down acc = {acc with actions = Down::acc.actions} in
  let prev acc = {acc with actions = Prev::acc.actions} in
  let next acc = {acc with actions = Next::acc.actions} in
  let up acc = {acc with actions = Up::acc.actions} in
  let make_line = ul [class' "moves"] in
  let make_variation var = li [class' "variation"] [var] in
  let make_variations = function
    | [] -> noNode
    | variations -> ol [class' "variations"] variations in
  let make_comment comment = span [class' "comment"] [text comment] in
  let make_move acc move variations =
    let number = acc.ply / 2 + 1
    and w_move = acc.ply mod 2 = 0 in
    li [ classList
           [ "move", true
           ; "white", w_move
           ; "black", not w_move
           ; "variations", variations <> noNode
           ; "highlight", acc.actions = []
           ]
       ]
      [ span [class' "comments"] (List.map make_comment move.pre_comments)
      ; span [class' "number"] [ string_of_int number |> text ]
      ; span
          [ class' "move"
          ; onClick (Jump acc.actions)
          ]
          [ move.san |> text ]
      ; span [class' "nag"] (List.map (fun nag ->
            Nag.string_of_nag nag |> text) move.nags)
      ; span [class' "comments"] (List.map make_comment move.post_comments)
      ; variations ] in
  let home_view acc =
    li [ classList [ "move", true
                   ; "home", true
                   ; "highlight", acc.actions = []
                   ] ]
      [ span
          [ class' "move"
          ; onClick (Jump acc.actions) ]
          [ text {js|\u2302|js} ]
      ] in
  let make_mainline acc line = home_view acc::line in
  Zipper.fold_zipper
    ~make_mainline ~make_line ~make_variation ~make_variations ~make_move
    ~acc ~fwd ~back ~up ~down ~next ~prev model.moves


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
    (fun (k, _) -> k = "White" || k = "WhiteElo" || k = "WhiteTitle" ||
                   k = "Black" || k = "BlackElo" || k = "BlackTitle" ||
                   k = "Date") pgn_header
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
    ; move_list_view model
    ; result_view model.result
    ]


