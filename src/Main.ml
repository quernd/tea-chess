open Tea
open Tea.Html

type color =
  | Black
  | White

type model =
  { orientation : color
  }

type msg =
  | Flip
[@@bs.deriving {accessors}]


let init () =
  { orientation = White
  }, Cmd.none


let update model = function
  | Flip ->
    let orientation' = match model.orientation with
      | Black -> White
      | White -> Black in
    { orientation = orientation'
    }, Cmd.none


let view model =
  let files, ranks =
    match model.orientation with
    | White -> [0; 1; 2; 3; 4; 5; 6; 7], [7; 6; 5; 4; 3; 2; 1; 0]
    | Black -> [7; 6; 5; 4; 3; 2; 1; 0], [0; 1; 2; 3; 4; 5; 6; 7]
  and char_of_file file = "abcdefgh".[file]
  and char_of_rank rank = "12345678".[rank] in

  let rank_view rank =
    let square_view rank file =
      td []
        [ Printf.sprintf "%c%c" (char_of_file file) (char_of_rank rank) 
          |> text
        ] in
    List.map (square_view rank) files
    |> tr [] in

  div []
    [ List.map rank_view ranks
      |> table []
    ; button [onClick Flip] [text "flip board"]
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
