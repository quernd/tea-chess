open Tea
open Tea.Html

open Chess

type size = int

type move' =
  | Completed_move of move
  | Pawn_will_promote

type dragging = { turn : color
                ; source : square
                ; target : square option
                ; legal_targets : (square * move') list
                ; initial : Mouse.position
                ; offset : Mouse.position
                ; coordinates : Mouse.position
                ; size : size
                }

type status =
  | Dragging of dragging
  | Nothing

type interactable =
  | Not_interactable
  | Interactable of color * move list

type model =
  { orientation : color
  ; status : status
  }

type internal_msg =
  | Flip
  | Move_start of dragging
  | Move_drag of Mouse.position
  | Move_drop of Mouse.position
[@@bs.deriving {accessors}]

type msg =
  | Internal_msg of internal_msg
  | Move of move
[@@bs.deriving {accessors}]


let init () =
  { orientation = Chess.White
  ; status = Nothing
  }

let cartesian_decoder field_x field_y =
  let open Json.Decoder in
  let open Mouse in
  map2 (fun x y -> {x; y})
    (field field_x int)
    (field field_y int)

let page =
  cartesian_decoder "pageX" "pageY"
  |> Json.Decoder.decodeEvent

let offset_page_size =
  let open Json.Decoder in
  let size = field "clientWidth" int in
  map3
    (fun a b c -> a, b, c)
    (cartesian_decoder "offsetX" "offsetY")
    (cartesian_decoder "pageX" "pageY")
    (field "target" size)
  |> decodeEvent

let handler decoder msg event =
  let open Result in
  let result = decoder event in
  match result with
  | Ok result -> Some (msg result)
  | Error _ -> None 


let update model = function
  | Internal_msg Flip ->
    let orientation' = Chess.opposite_color model.orientation in
    { model with
      orientation = orientation'
    }, Cmd.none
  | Internal_msg (Move_start info) -> Js.log info; model, Cmd.none
  | _ -> model, Cmd.none

let result_view result =
  p []
    [ begin match result with
        | Chess.Win White -> "White wins by checkmate!" 
        | Chess.Win Black -> "Black wins by checkmate!"
        | Chess.Draw -> "The game is a draw!"
        | Chess.Play move_list ->
          List.length move_list
          |> Printf.sprintf "There are %d legal moves in this position!"
      end |> text
    ]


let filter_targets source moves =
  List.filter (fun (s, _t, _m) -> s = source) moves
  |> List.map (fun (_s, t, m) -> t, m)


let coordinate_pairs turn move =
  let home_rank = function White -> 0 | Black -> 7 in
  match move with
  | Queenside_castle ->
    (4, home_rank turn), (2, home_rank turn),
    Completed_move Queenside_castle
  | Kingside_castle ->
    (4, home_rank turn), (6, home_rank turn),
    Completed_move Kingside_castle
  | Promotion (_piece_type, s_file, t_file) ->
    let promotion_rank = function White -> 7 | Black -> 0
    and pre_promotion_rank = function White -> 6 | Black -> 1 in
    (s_file, pre_promotion_rank turn), (t_file, promotion_rank turn),
    Pawn_will_promote
  | Move (s_file, s_rank, t_file, t_rank) -> 
    (s_file, s_rank), (t_file, t_rank),
    Completed_move (Move (s_file, s_rank, t_file, t_rank))


let buttons_view =
  [ button [onClick (Internal_msg Flip)] [text "flip board"]
  ]


let view interactable pos_ar model =

  let move_start =
    match interactable with
    | Interactable (turn, legal_moves) ->
      Some ((fun file rank (offset, coordinates, size) ->
          Internal_msg
            (Move_start
               { turn
               ; source = (file, rank)
               ; target = None
               ; legal_targets =
                   legal_moves
                   |> List.map (coordinate_pairs turn)
                   |> filter_targets (file, rank) 
               ; initial = coordinates
               ; offset
               ; coordinates
               ; size
               }
            )
        ), turn)
    | Not_interactable -> None
  in

  let files, ranks =
    match model.orientation with
    | White -> [0; 1; 2; 3; 4; 5; 6; 7], [7; 6; 5; 4; 3; 2; 1; 0]
    | Black -> [7; 6; 5; 4; 3; 2; 1; 0], [0; 1; 2; 3; 4; 5; 6; 7] in

  let rank_view rank =
    let square_view rank file =
      node "cb-square" []
        [ match pos_ar.(file).(rank) with
          | Chess.Piece (piece_type, color) ->
            let listener =
              begin match move_start with
                | Some (f, turn) when color = turn -> 
                  onCB "mousedown" ""
                    (handler offset_page_size (f file rank))
                | _ -> noProp end in
            node "cb-piece"
              [ listener
              ; classList
                  [ Chess.string_of_color color, true
                  ; Chess.string_of_piece_type piece_type, true
                  ]
              ] []
          | Chess.Empty -> noNode
        ] in
    List.map (square_view rank) files
    |> node "cb-row" [] in

  List.map rank_view ranks
  |> node "cb-board" []
