open Tea
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

type promoting = { turn : color
                 ; source_file : file
                 ; target_file : file
                 ; size : size
                 }

type state =
  | Dragging of dragging
  | Promoting of promoting
  | Nothing
[@@bs.deriving {accessors}]

type interactable =
  | Not_interactable
  | Interactable of color * move list
[@@bs.deriving {accessors}]

type model =
  { orientation : color
  ; state : state
  }

type internal_msg =
  | Flip_board
  | Move_start of dragging
  | Move_drag of Mouse.position
  | Move_drop of Mouse.position
  | Square_entered of square
  | Square_left of square
  | Promotion_canceled
  | Piece_promoted of piece_type
[@@bs.deriving {accessors}]

type msg =
  | Internal_msg of internal_msg
  | Move of move
[@@bs.deriving {accessors}]


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


let init =
  { orientation = Chess.White
  ; state = Nothing
  }


let update model = function
  | Internal_msg msg ->
    begin match msg, model.state with
      | Flip_board, _ ->
        { model with
          orientation = Chess.opposite_color model.orientation },
        Cmd.none
      | Move_start drag, _ ->
        { model with state = Dragging drag }, Cmd.none
      | Move_drag coordinates, Dragging drag ->
        { model with state = Dragging { drag with coordinates } }, Cmd.none
      | Square_entered square, Dragging drag ->
        { model with state =
                       Dragging { drag with target = Some square }
        }, Cmd.none
      | Square_left _, Dragging drag ->
        { model with state = Dragging {drag with target = None} }, Cmd.none
      | Move_drop _, Dragging drag -> Js.log drag;
        begin match drag.target with
          | Some target ->
            begin try match List.assoc target drag.legal_targets with
              | Completed_move move ->
                { model with state = Nothing }, Cmd.msg (Move move)
              | Pawn_will_promote ->
                { model with
                  state = Promoting { turn = drag.turn
                                    ; source_file = fst drag.source
                                    ; target_file = fst target
                                    ; size = drag.size
                                    }
                }, Cmd.none
              with Not_found -> { model with state = Nothing }, Cmd.none
            end
          | None -> { model with state = Nothing }, Cmd.none
        end
      | Promotion_canceled, _ -> { model with state = Nothing }, Cmd.none
      | Piece_promoted piece_type, Promoting promoting ->      
        let move = Promotion (piece_type,
                              promoting.source_file,
                              promoting.target_file) in
        { model with state = Nothing }, Cmd.msg (Move move)
      | _ -> model, Cmd.none
    end
  | _ -> model, Cmd.none


let flip_button_view =
  let open Html in
  button
    [ onClick (Internal_msg Flip_board) ]
    [ text "Flip board" ]


let filter_targets source moves =
  List.filter (fun ((s, _t), _m) -> s = source) moves
  |> List.map (fun ((_s, t), m) -> t, m)

let completed_move = function
  | Promotion _ -> Pawn_will_promote
  | move -> Completed_move move

let coordinate_pairs turn move =
  Chess.coordinate_pairs turn move, completed_move move

let move_start interactable =
  match interactable with
  | Interactable (turn, legal_moves) ->
    Some (turn,
          fun file rank (offset, coordinates, size) ->
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
                 } ) )
  | Not_interactable -> None


let board_view interactable pos_ar model =
  let open Html in
  let files, ranks =
    match model.orientation with
    | White -> [0; 1; 2; 3; 4; 5; 6; 7], [7; 6; 5; 4; 3; 2; 1; 0]
    | Black -> [7; 6; 5; 4; 3; 2; 1; 0], [0; 1; 2; 3; 4; 5; 6; 7] in

  let drag_transform drag =
    Printf.sprintf "translate(%dpx,%dpx)" 
      (drag.offset.x - (drag.size / 2) + drag.coordinates.x - drag.initial.x)
      (drag.offset.y - (drag.size / 2) + drag.coordinates.y - drag.initial.y)
    |>  style "transform" in

  let target_highlight drag target =
    match drag.target with
    | Some square when square = target -> true
    | _ -> false
  and legal_highlight drag target = List.exists
      (fun (square, _) -> square = target) drag.legal_targets in

  let rank_view rank =

    let square_view rank file =
      let piece_view, listener =
        match pos_ar.(file).(rank) with
        | Chess.Empty -> noNode, noProp
        | Chess.Piece (piece_type, color) ->
          let drag_origin, transform =
            match model.state with
            | Dragging drag when (file, rank) = drag.source ->
              true, drag_transform drag
            | _ -> false, noProp in
          node "cb-piece"
            [ transform
            ; classList
                [ Chess.string_of_color color, true
                ; Chess.string_of_piece_type piece_type, true
                ; "dragged", drag_origin
                ]
            ] [],
          match move_start interactable with
          | Some (turn, msg) when color = turn -> 
            onCB "mousedown" "" (msg file rank |> handler offset_page_size)
          | _ -> noProp in
      node "cb-square"
        (listener::
         match model.state with
         | Dragging drag ->
           [ classList
               [ "destination", legal_highlight drag (file, rank)
               ; "hovering", target_highlight drag (file, rank)
               ]
           ; onMouseEnter (Internal_msg (Square_entered (file, rank)))
           ; onMouseLeave (Internal_msg (Square_left (file, rank)))
           ]
         | _ -> [noProp; noProp; noProp])
        [piece_view] in

    List.map (square_view rank) files
    |> node "cb-row" [] in

  List.map rank_view ranks
  |> node "cb-board"
    [ match model.state with
      | Dragging _ -> class' "dragging"
      | _ -> noProp
    ]

let view interactable pos_ar model =
  let open Html in

  let promo_view promoting =
    let file = promoting.target_file in
    let left, tops =
      begin match model.orientation, promoting.turn with
        | White, White -> file, [0; 1; 2; 3]
        | White, Black -> file, [7; 6; 5; 4]
        | Black, White -> 7 - file, [7; 6; 5; 4]
        | Black, Black -> 7 - file, [0; 1; 2; 3]
      end in

    let promo_piece_view (top, piece_type) =
      node "cb-square"
        [ Internal_msg (Piece_promoted piece_type) |> onClick
        ; styles
            [ "left", Printf.sprintf "%dpx" (left * promoting.size)
            ; "top", Printf.sprintf "%dpx" (top * promoting.size)
            ]
        ]
        [ node "cb-piece"
            [classList
               [ Chess.string_of_color promoting.turn, true
               ; Chess.string_of_piece_type piece_type, true
               ]
            ] []         
        ] in

    List.combine tops [Queen; Knight; Rook; Bishop]
    |> List.map promo_piece_view
    |> node "cb-promo" [ Internal_msg Promotion_canceled |> onClick ] in

  node "cb-wrap" []
    [ begin match model.state with
        | Promoting promoting -> promo_view promoting
        | _ -> noNode end
    ; board_view interactable pos_ar model
    ]


let subscriptions model = match model.state with
  | Dragging _ ->
    Sub.batch 
      [ Mouse.moves (fun x -> Internal_msg (Move_drag x))
      ; Mouse.ups  (fun x -> Internal_msg (Move_drop x))
      ]
  | _ -> Sub.none
