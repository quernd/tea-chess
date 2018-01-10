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
  | Move of Pgn.move
  | Ready
  | Make_move of string
  | Set_autoplay of bool
  | Set_depth of int
[@@bs.deriving {accessors}]

type parse =
  | Parsed_move of Pgn.move
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
  | Set_autoplay autoplay -> { model with autoplay }, Cmd.none
  | Set_depth depth -> { model with depth }, Cmd.none
  | Data data -> Js.log data;
    let open Opal in
    let parser =
      (token "Stockfish" >> return Parsed_ready)
      <|>
      (token "bestmove" >>
       lexeme (many1 alpha_num) >> 
       token "bestmoveSan" >>
       Pgn.move () >>= fun move ->
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


let view model fen =
  let open Html in
  let open Util in
  p []
    [ begin match model.status with
        | Idle -> button
                    [ Make_move fen |> onClick ]
                    [ text "move, computer!" ]
        | Loading -> text "loading Stockfish"
        | Thinking -> text "Stockfish is thinking" end
    ; input' [ id "autoplay"
             ; type' "checkbox"
             ; onCheck set_autoplay
             ; checked model.autoplay
             ] []
    ; label [ for' "autoplay" ] [ text "autoplay" ]
    ; input' [ id "depth"
             ; type' "number"
             ; style "width" "3em"
             ; style "margin-left" "1em"
             ; onChange (int_of_string >> set_depth)
             ; value (string_of_int model.depth)
             ; Attributes.min "1"
             ; Attributes.max "24"
             ] []
    ; label [ for' "depth"] [ text "strength" ]
    ]

