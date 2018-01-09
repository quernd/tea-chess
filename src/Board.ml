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

type state =
  | Dragging of dragging
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
  | Square_entered of file * rank
  | Square_left of file * rank
[@@bs.deriving {accessors}]

type msg =
  | Internal_msg of internal_msg
  | Move of move
[@@bs.deriving {accessors}]


let init =
  { orientation = Chess.White
  ; state = Nothing
  }


let update model = function
  | Internal_msg Flip_board ->
    { model with
      orientation = Chess.opposite_color model.orientation },
    Cmd.none
  | _ -> model, Cmd.none


let flip_button_view =
  let open Html in
  button
    [ onClick (Internal_msg Flip_board) ]
    [ text "Flip board" ]

let view pos_ar model =
  let open Html in
  let files, ranks =
    match model.orientation with
    | White -> [0; 1; 2; 3; 4; 5; 6; 7], [7; 6; 5; 4; 3; 2; 1; 0]
    | Black -> [7; 6; 5; 4; 3; 2; 1; 0], [0; 1; 2; 3; 4; 5; 6; 7] in

  let rank_view rank =

    let square_view rank file =
      let piece_view =
        match pos_ar.(file).(rank) with
        | Chess.Piece (piece_type, color) ->
          node "cb-piece"
            [ classList
                [ Chess.string_of_color color, true
                ; Chess.string_of_piece_type piece_type, true
                ]
            ] []
        | Chess.Empty -> noNode in
      node "cb-square" [] [piece_view] in

    List.map (square_view rank) files
    |> node "cb-row" [] in

  List.map rank_view ranks
  |> node "cb-board" []
