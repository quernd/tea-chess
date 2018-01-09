open Tea

type model =
  { position : Ochess.position
  ; orientation : Ochess.color
  }

type msg =
  | Flip_board
  | Random_button
  | Random_move of Ochess.move


let init () =
  { position = Ochess.init_position
  ; orientation = White
  }, Cmd.none


let update model = function
  | Flip_board ->
    { model with
      orientation = Ochess.opposite_color model.orientation },
    Cmd.none
  | Random_button -> model, Cmd.none
  | Random_move _ -> model, Cmd.none


let view model =
  let open Html in
  div []
    [ p [] [ Printf.sprintf "Move %d.  It is %s's move."
               model.position.number
               (match model.position.turn with | Black -> "Black"
                                               | White -> "White")
             |> text
           ]
    ; p [] [ button
               [ onClick Flip_board ]
               [ text "Flip board" ]
           ; button
               [ onClick Random_button ]
               [ text "Make a random move!" ]
           ]
    ]


let main =
  App.standardProgram
    { init
    ; update
    ; view
    ; subscriptions = (fun _ -> Sub.none)
    }
