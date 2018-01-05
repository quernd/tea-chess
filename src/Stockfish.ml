type stockfish_event = string

type msg =
  | Irrelevant
  | Ready
  | Move of Pgn.pgn_move

let stockfish_event =
  let open Tea.Json.Decoder in
  (field "detail" (field "data" string))

let registerGlobal name key tagger =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn = fun ev -> 
      let open Tea_json.Decoder in
      let open Tea_result in
      match decodeEvent stockfish_event ev with
      | Error _ -> None
      | Ok pos -> Some (tagger pos) in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () ->
      let _ = eventHandler_Unregister elem name cache in
      ()
  in Tea_sub.registration key enableCall

let stockfish ?(key="") tagger =
  registerGlobal "stockfish" key tagger

let parse s =
  let open Opal in
  let parser =
    (token "Stockfish" >> return Ready)
    <|>
    (token "bestmove" >>
     lexeme (many1 alpha_num) >> 
     token "bestmoveSan" >>
     Pgn.pgn_move () >>= fun move ->
     exactly ' ' >>
     return (Move move)) in
  let move = LazyStream.of_string s |> parse parser in
  match move with
  | Some msg -> msg
  | None -> Irrelevant
