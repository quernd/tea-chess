open Tea

type stockfish = <
  postMessage : string -> unit [@bs.meth];
> Js.t


type status =
  | Idle
  | Thinking
  | Loading

type msg =
  | Data of string
  | Move of Pgn.pgn_move
  | Ready
  | Make_move of string
  | Set_depth of string
  | Autoplay of bool
[@@bs.deriving {accessors}]

type parse =
  | Parsed_move of Pgn.pgn_move
  | Parsed_ready

type model =
  { status : status
  ; stockfish : stockfish
  ; depth : int
  ; autoplay : bool
  }

let init stockfish =
  { status = Loading
  ; stockfish = stockfish
  ; depth = 12
  ; autoplay = true
  }

let update model = function
  | Autoplay autoplay -> {model with autoplay}, Cmd.none
  | Set_depth depth ->
    begin try {model with depth = int_of_string depth}, Cmd.none
      with _ -> model, Cmd.none end
  | Data data ->
    let open Opal in
    let parser =
      (token "Stockfish" >> return Parsed_ready)
      <|>
      (token "bestmove" >>
       lexeme (many1 alpha_num) >> 
       token "bestmoveSan" >>
       Pgn.pgn_move () >>= fun move ->
       exactly ' ' >>
       return (Parsed_move move)) in
    let move = LazyStream.of_string data |> parse parser in
    begin match move with
      | Some Parsed_ready -> {model with status = Idle}, Cmd.msg Ready
      | Some (Parsed_move move) -> {model with status = Idle}, Cmd.msg (Move move)
      | None -> model, Cmd.none
    end
  | Make_move position ->
    model.stockfish##postMessage ("position fen " ^ position) ;
    model.stockfish##postMessage (Printf.sprintf "go depth %d" model.depth) ;
    {model with status = Thinking}, Cmd.none
  | Move _ -> model, Cmd.none
  | Ready -> model, Cmd.none




let view model =
  let open Html in
  form []
    [ node "fieldset" []
        [ node "legend" [] [text "Stockfish"]
        ; p []
            [ label [for' "strength"]
                [text "strength"
                ; input'
                    [ type' "range"
                    ; string_of_int model.depth |> value
                    ; Attributes.min "1"
                    ; Attributes.max "24"
                    ; Attributes.step "1"
                    ; onInput set_depth
                    ; id "strength"
                    ]
                    []
                ]
            ]
        ; p []
            [ label [for' "autoplay"]
                [text "autoplay"
                ; input'
                    [ type' "checkbox"
                    ; onCheck autoplay
                    ; id "autoplay"
                    ]
                    []
                ]
            ]
        ]
    ]

