open Tea

type san = string

type move =
  { move : Chess.move
  ; san : san
  }

type model =
  { position : Chess.position
  ; moves : move list
  }

type msg =
  | Move of Chess.move
  | Take_back
[@@bs.deriving {accessors}]    

let init =
  { position = Chess.init_position
  ; moves = []
  }


let simple_move move san =
  { move = move
  ; san = san
  }


let update model = function
  | Move move ->
    begin try
        let san = Chess.san_of_move model.position move in
        let position = Chess.make_move model.position move 0 in
        { model with position
                   ; moves = simple_move move san :: model.moves
        }, Cmd.none
      with Chess.Illegal_move -> model, Cmd.none
    end
  | Take_back ->
    begin match model.position.prev, model.moves with
      | Some position, _hd::moves ->
        { model with position; moves }, Cmd.none
      | _ -> model, Cmd.none
    end


let view model =
  let open Html in
  let move_view move =
    li [] [ text move.san ] in

  div []
    [ p [] [ Printf.sprintf "Move %d.  It is %s's move."
               model.position.number
               (match model.position.turn with | Black -> "Black"
                                               | White -> "White")
             |> text
           ]
    ; List.rev_map move_view model.moves |> ul []
    ]
