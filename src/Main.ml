open Tea
open Tea.Html
open Tea.App

type model =
  { position : Chess.position
  ; boards : Board.model list
  }

type msg =
  | Board_msg of int * Board.msg
  | Random_button
  | Random_move of Chess.move
  | Add_board
  | Delete_board of int
[@@bs.deriving {accessors}]


let init () =
  { position = Chess.init_position
  ; boards = [Board.init ()]
  }, Cmd.none


let replace_nth n item list =
  List.mapi
    (fun i x -> if i = n then item else x) list

let rec delete_nth n list =
  match list with
  | [] -> []
  | _hd::tl when n = 0 -> tl
  | hd::tl -> hd::delete_nth (n - 1) tl


let update model = function
  | Board_msg (i, msg) ->
    let board', cmd = Board.update (List.nth model.boards i) msg in
    { model with
      boards = replace_nth i board' model.boards
    }, Cmd.map (board_msg i) cmd
  | Random_button ->
    model,
    begin match Chess.game_status model.position with
      | Play move_list ->
        List.length move_list
        |> Random.int 0
        |> Random.generate
          (fun random_number ->
             List.nth move_list random_number |> random_move)
      | _ -> Cmd.none
    end
  | Random_move move ->
    { model with
      position = Chess.make_move model.position move 0 }, Cmd.none
  | Add_board ->
    { model with
      boards = Board.init ()::model.boards
    }, Cmd.none
  | Delete_board i ->
    { model with
      boards = delete_nth i model.boards
    }, Cmd.none


let view model =
  let board_view i board =
    div []
      [ Board.view model.position.ar board |>
        map (board_msg i)
      ; (button [onClick (Delete_board i)] [text "delete board"]
         ::List.map (map (board_msg i)) Board.buttons_view)
        |> p []
      ] in
  ([ button [onClick Random_button] [text "random move"]
   ; button [onClick Add_board] [text "add board"]
   ] |> p []
  )::List.mapi board_view model.boards
  |>  div []


let subscriptions _model =
  Sub.none


let main =
  standardProgram
    { init
    ; update
    ; view
    ; subscriptions
    }
