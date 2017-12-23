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

let view _model =
  noNode

let subscriptions _model =
  Sub.none

let main =
  App.standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
