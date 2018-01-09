open Tea

type model =
  { moves : int
  ; turn : Ochess.color
  }

type msg =
  | Move


let init () =
  { moves = 1
  ; turn = White
  }, Cmd.none


let update model = function
  | Move ->
    let turn = Ochess.opposite_color model.turn in
    let moves = model.moves + 1 in
    { turn; moves }, Cmd.none


let view model =
  let open Html in
  div []
    [ p [] [ Printf.sprintf "Move %d.  It is %s's move."
               model.moves
               (match model.turn with | Black -> "Black"
                                      | White -> "White")
             |> text
           ]
    ; p [] [ button
               [ onClick Move ]
               [ text "Make a move!" ]
           ]
    ]


let main =
  App.standardProgram
    { init
    ; update
    ; view
    ; subscriptions = (fun _ -> Sub.none)
    }
