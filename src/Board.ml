open Tea
open Tea.Html

open Chess

type size = int

type partial_move =
  | CompletedMove of move
  | PawnWillPromote

type dragging = { turn : color
                ; source : square
                ; target : square option
                ; legal_targets : (square * partial_move) list
                ; initial : Mouse.position
                ; offset : Mouse.position
                ; coordinates : Mouse.position
                ; size : size
                ; orientation : color
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


let update model = function
  | Internal_msg Flip ->
    let orientation' = Chess.opposite_color model.orientation in
    { model with
      orientation = orientation'
    }, Cmd.none
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


let buttons_view =
  [ button [onClick (Internal_msg Flip)] [text "flip board"]
  ]


let view pos_ar model =
  let files, ranks =
    match model.orientation with
    | White -> [0; 1; 2; 3; 4; 5; 6; 7], [7; 6; 5; 4; 3; 2; 1; 0]
    | Black -> [7; 6; 5; 4; 3; 2; 1; 0], [0; 1; 2; 3; 4; 5; 6; 7] in

  let rank_view rank =
    let square_view rank file =
      node "cb-square" []
        [ match pos_ar.(file).(rank) with
          | Chess.Piece (piece_type, color) ->
            node "cb-piece"
              [ classList
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
