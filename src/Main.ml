open Tea
open Tea.Html

type color = Chess.color

type model =
  { orientation : color
  ; position : Chess.position
  }

type msg =
  | Flip
[@@bs.deriving {accessors}]


let init () =
  { orientation = White
  ; position = Chess.init_position
  }, Cmd.none


let update model = function
  | Flip ->
    let orientation' = Chess.opposite_color model.orientation in
    { model with
      orientation = orientation'
    }, Cmd.none


let view model =
  let files, ranks =
    match model.orientation with
    | White -> [0; 1; 2; 3; 4; 5; 6; 7], [7; 6; 5; 4; 3; 2; 1; 0]
    | Black -> [7; 6; 5; 4; 3; 2; 1; 0], [0; 1; 2; 3; 4; 5; 6; 7] in

  let rank_view rank =
    let square_view rank file =
      node "cb-square" []
        [ match model.position.ar.(file).(rank) with
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

  div []
    [ List.map rank_view ranks
      |> node "cb-board" []
    ; p [] [button [onClick Flip] [text "flip board"]]
    ]


let subscriptions _model =
  Sub.none


let main =
  App.standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
