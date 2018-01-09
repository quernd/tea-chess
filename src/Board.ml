open Tea

type msg =
  | Flip_board
[@@bs.deriving {accessors}]

type model =
  { orientation : Chess.color
  }


let init =
  { orientation = Chess.White
  }


let update model = function
  | Flip_board ->
    { model with
      orientation = Chess.opposite_color model.orientation },
    Cmd.none


let flip_button_view =
  let open Html in
  button
    [ onClick Flip_board ]
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
