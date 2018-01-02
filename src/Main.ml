open Tea
open Tea.Html
open Tea.App

open Lens.Infix

module IntT = struct
  type t = int
  let compare = compare
end

module StringT = struct
  type t = string
  let compare = compare
end

module IntMap = Map.Make(IntT)
module StringMap = Map.Make(StringT)

module MapLens(Key : Map.OrderedType) = struct
  let for_key key =
    let module M = Map.Make(Key) in
    let open Lens in
    { get = M.find key
    ; set = M.add key
    }
end

module IntMapLens = MapLens(IntT)
module StringMapLens = MapLens(StringT)

type 'a transfer =
  | Idle
  | Loading
  | Failed
  | Received of 'a

type model =
  { game : game_lens
  ; board : Board.model
  ; scratch_game : Game.model
  ; local_games : Game.model IntMap.t
  ; tournament : (string * string list) list transfer
  ; lichess_games : int StringMap.t
  ; route : route
  }
and game_lens = (model, Game.model) Lens.t
and route =
  | Local of int
  | Lichess of string
  | Scratch
  | Tournament


let local_lens = let open Lens in
  { get = (fun x -> x.local_games)
  ; set = (fun v x -> { x with local_games = v })
  }
let lichess_lens = let open Lens in
  { get = (fun x -> x.lichess_games)
  ; set = (fun v x -> { x with lichess_games = v })
  }
let scratch_lens = let open Lens in
  { get = (fun x -> x.scratch_game)
  ; set = (fun v x -> { x with scratch_game = v })
  }


type msg =
  | Board_msg of Board.msg
  | Game_msg of Game.msg
  | Key_pressed of Keyboard.key_event
  | New_tab
  | Save_games
  | Clear_games
  | Games_loaded of (string * string) list
  | Load_tournament
  | Tournament_data of (string, string Http.error) Result.t
  | Location_change of Web.Location.location  
  | Pgn_requested of string
  | Pgn_data of game_lens * (string, string Http.error) Result.t
  | Close_tab of string
  | Validate_pgn of string
[@@bs.deriving {accessors}]

let proxy = "http://localhost:3000/fetch/"

let route_of_location location =
  let open Web.Location in
  let route = Chess.split_on_char '/' location.hash in
  match route with
  | ["#"; ""] -> Scratch
  | ["#"; "tournament"] -> Tournament
  | ["#"; "local"; id] -> Local (int_of_string id)
  | ["#"; "lichess"; id] -> Lichess id
  | _ -> Scratch

let location_of_route = function
  | Local id -> Printf.sprintf "#/local/%d" id
  | Lichess id -> Printf.sprintf "#/lichess/%s" id
  | Scratch -> "#/"
  | Tournament -> "#/tournament"

let update_route model route =
  if model.route = route then model, Cmd.none
  else
    match route with
    | Local id when IntMap.mem id model.local_games ->
      let game = local_lens |-- IntMapLens.for_key id in
      {model with route; game},
      location_of_route route |> Navigation.modifyUrl
    | Local _ -> {model with route},
                 location_of_route route |> Navigation.modifyUrl
    | Lichess game_id ->
      begin try
          let id = StringMap.find game_id model.lichess_games in
          let game = local_lens |-- IntMapLens.for_key id in
          let route = Local id in
          {model with route; game},
          location_of_route route |> Navigation.modifyUrl
        with Not_found ->
          let key = begin try (IntMap.max_binding model.local_games |> fst) + 1
            with Not_found -> 1 end in
          let game = local_lens |-- IntMapLens.for_key key in
          let route = Local key in
          { model with
            route
          ; local_games = IntMap.add key (Game.init ()) model.local_games
          ; game
          ; lichess_games = StringMap.add game_id key model.lichess_games },
          Cmd.batch
            [ location_of_route route |> Navigation.modifyUrl
            ; Printf.sprintf
                "%shttps://lichess.org/game/export/%s.pgn" proxy game_id
              |> Http.getString
              |> Http.send (pgn_data game) ]
      end
    | _ -> {model with route = Scratch; game = scratch_lens}, Cmd.none


let load_games =
  Ex.LocalStorage.length
  |> Tea_task.andThen (fun length ->
      let rec loop i acc =
        if i >= 0 then
          loop (i - 1) (Ex.LocalStorage.key i::acc)
        else acc in
      loop length [] |> Tea_task.sequence
    )
  |> Tea_task.andThen (fun keys ->
      List.map
        (fun key ->
           (Ex.LocalStorage.getItem key
            |> Tea_task.map (fun value -> (key, value))
           )
        ) keys
      |> Tea_task.sequence)
  |> Tea_task.attemptOpt (function | Ok games -> Some (Games_loaded games)
                                   | Error _ -> None)


let init_model =
  { scratch_game = Game.init ()
  ; game = scratch_lens
  ; board = Board.init ()
  ; tournament = Idle
  ; local_games = IntMap.empty
  ; lichess_games = StringMap.empty
  ; route = Scratch
  }

let init () location =
  let model, cmd =
    route_of_location location |> update_route init_model in
  model, Cmd.batch [cmd; load_games]


let update model = function
  | Board_msg (Move move) ->
    let game, cmd = Game.update (model |. model.game) (Game.Make_move move) in
    model |> model.game ^= game, Cmd.map game_msg cmd
  | Board_msg msg ->
    let board, cmd = Board.update model.board msg in
    {model with board}, Cmd.map board_msg cmd
  | Game_msg msg ->
    let game, cmd = Game.update (model |. model.game) msg in
    model |> model.game ^= game, Cmd.map game_msg cmd
  | Key_pressed key_event ->
    model,
    begin match key_event.ctrl, key_event.key_code with
      | _, 37 (* left *) | true, 66 (* Ctrl-b *) ->
        Cmd.msg (Game_msg Back_button)
      | _, 39 (* right *) | true, 70 (* Ctrl-f *) ->
        Cmd.msg (Game_msg Fwd_button)
      | true, 82 (* Ctrl-r *) -> Cmd.msg (Game_msg Random_button)
      | true, 84 (* Ctrl-t *) -> Cmd.msg (Game_msg Back_button)
      | _ -> Cmd.none
    end
  | New_tab ->
    let key = try (IntMap.max_binding model.local_games |> fst) + 1
      with Not_found -> 1 in
    update_route
      { model with
        local_games = IntMap.add key (Game.init ()) model.local_games
      ; scratch_game = Game.init ()
      } (Local key)
  | Save_games ->
    let keys, cmds =
      IntMap.fold
        (fun key game (acc_keys, acc_pgn) ->
           let key' = string_of_int key in
           let pgn = Pgn.string_of_game game in
           (key'::acc_keys),
           (Ex.LocalStorage.setItemCmd key' pgn::acc_pgn))
        model.local_games
        ([], []) in
    let games = String.concat " " keys in
    let cmd = Ex.LocalStorage.setItemCmd "games" games in
    model, cmd::cmds |> Cmd.batch
  | Games_loaded list ->
    begin try
        let games =
          List.assoc "games" list
          |> Js.String.split " " |> Array.to_list in
        let local_games =
          List.fold_left
            (fun acc v ->
               IntMap.add
                 (int_of_string v)
                 (List.assoc v list |> Pgn.game_of_string)
                 acc
            ) IntMap.empty games in
        let game =
          match model.route with
          | Local id -> local_lens |-- IntMapLens.for_key id
          | _ -> model.game in        
        {model with local_games; game}, Cmd.none
      with e -> Js.log e; model, Cmd.none
    end
  | Clear_games ->
    model, Ex.LocalStorage.clearCmd ()
  | Load_tournament ->
    let url = "https://lichess.org/api/tournament/GToVqkC9" in
    {model with route = Tournament; tournament = Loading},
    Http.getString url |> Http.send tournament_data
  | Tournament_data (Error _e) ->
    {model with tournament = Failed}, Cmd.none
  | Tournament_data (Ok data) -> 
    let open Json.Decoder in
    let players_decoder = list string in
    let pairing_decoder = map2 (fun x y -> x, y)
        (field "id" string)
        (field "u" players_decoder) in
    let list_decoder = list pairing_decoder in
    let pairings_decoder = field "pairings" list_decoder in
    {model with
     tournament = match decodeString pairings_decoder data with
       | Ok tournament -> Received tournament
       | Error _ -> Failed
    }, Cmd.none
  | Pgn_data (lens, (Ok data)) ->
    begin try
        let game = Pgn.game_of_string data in
        model |> lens ^= game, Cmd.none
      with _e -> model, Cmd.none end
  | Location_change location ->
    route_of_location location |> update_route model
  | _ -> model, Cmd.none



let header_nav_view =
  nav [class' "top"]
    [ ul []
        [ li [class' "home"] [text "TEA-Chess"]
        ; li []
            [ a [href "https://quernd.github.io/tutorials/tea-chess"]
                [text "Tutorial"] ]
        ; li []
            [ a [href "https://github.com/quernd/tea-chess"]
                [text "Code"] ]
        ]
    ]

let tournament_view tournament =
  match tournament with
  | Idle -> text ""
  | Loading -> text "Loading tournament..."
  | Received tournament' ->
    List.map
      (fun (id, players) ->
         td [] [ a [href (Printf.sprintf "#/lichess/%s" id)] [text id]]::
         (List.map (fun player -> td [] [text player]) players) |> tr [])
      tournament'
    |> table []
  | Failed -> text "Tournament could not be loaded."



let game_nav_view model =
  let game_nav_item current link label =
    li [ if current then class' "current" else noProp ]
      [ if current then text label else a [href link] [text label] ] in

  let g id _game k =
    let item = game_nav_item 
        (model.route = Local id)
        (Printf.sprintf "#/local/%d" id)
        (Printf.sprintf "Game %d" id)
    in fun tl -> k (item::tl) in

  (* http://blog.wakatta.jp/blog/2011/11/11/haskell-foldr-as-foldl/ *)

(*
  let f hd tl = game_nav_item 
      (model.route = Local hd)
      (Printf.sprintf "#/local/%d" hd)
      (Printf.sprintf "Game %d" hd)::tl in

  let g id _game k acc =
    k (f id acc) in
*)

  nav [class' "top tabbed"]
    [ ul []
        (game_nav_item (model.route = Scratch) "#/" "*scratch*"
         ::(IntMap.fold g model.local_games (fun x -> x)) [])
    ]

let buttons_view =
  nav []
    [ button [onClick New_tab] [text "new tab"]
    ; button [onClick Save_games] [text "save all games in local storage"]
    ; button [onClick Clear_games] [text "clear local storage"]
    ; button [onClick Load_tournament] [text "load a game from Lichess"]
    ]

let scratch_view =
  div []
    [ buttons_view
    ; p []
        [ text "This is a scratch buffer.  Click \"new tab\" to open a new game in a separate tab."]
    ]


let view model =
  let game = model |. model.game in
  let game_view = Game.view game |> map game_msg in
  let interactable =
    match Chess.game_status game.position with
    | Play move_list ->
      Board.Interactable (game.position.turn, move_list)
    | _ -> Board.Not_interactable in
  main []
    [ section [id "board"]
        [ header_nav_view
        ; Board.view interactable game.position.ar model.board
          |> map board_msg
          ;
          List.map (map board_msg) Board.buttons_view @
          List.map (map game_msg) Game.buttons_view
          |> nav [id "buttons"]
          (* ; Board.result_view game_status *)
        ]
    ; section [id "game"]
        [ game_nav_view model
        ; section [class' "scroll"]
            begin match model.route with
              | Scratch -> scratch_view::[game_view]
              | Tournament -> [tournament_view model.tournament]
              | _ -> buttons_view::[game_view]
            end
        ]
    ]


let subscriptions model =
  Sub.batch [ Board.subscriptions model.board |> Sub.map board_msg
            ; Keyboard.downs key_pressed
            ]


let main =
  Navigation.navigationProgram location_change
    { init
    ; update
    ; view
    ; subscriptions
    ; shutdown = (fun _ -> Cmd.none)
    }
