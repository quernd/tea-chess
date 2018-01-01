open Tea
open Tea.Html
open Tea.App

open Lens.Infix

module Int = struct
  type t = int
  let compare = compare
end

module String = struct
  type t = string
  let compare = compare
end

module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

module MapLens(Key : Map.OrderedType) = struct
  let for_key key =
    let module M = Map.Make(Key) in
    let open Lens in
    { get = M.find key
    ; set = M.add key
    }
end

module IntMapLens = MapLens(Int)
module StringMapLens = MapLens(String)

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
  ; lichess_games : (int * string transfer) StringMap.t
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
  | Save_scratch
  | Load_tournament
  | Tournament_data of (string, string Http.error) Result.t
  | Location_change of Web.Location.location  
  | Pgn_requested of string
  | Pgn_data of string * (string, string Http.error) Result.t
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
      {model with route; game}, Cmd.none
    | _ -> {model with route = Scratch; game = scratch_lens}, Cmd.none


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
  route_of_location location |> update_route init_model


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
  | Save_scratch ->
    let key = try (IntMap.max_binding model.local_games |> fst) + 1
      with Not_found -> 0 in
    update_route
      { model with
        local_games = IntMap.add key (model |. model.game) model.local_games
      ; scratch_game = Game.init ()
      } (Local key)
  | Load_tournament ->
    let url = "https://lichess.org/api/tournament/GToVqkC9" in
    {model with route = Tournament; tournament = Loading},
    Http.getString url |> Http.send tournament_data
  | Tournament_data (Result.Error e) -> Js.log e;
    {model with tournament = Failed}, Cmd.none
  | Tournament_data (Result.Ok data) -> 
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
         td [] [ a [href (Printf.sprintf "#/pgn/%s" id)] [text id]]::
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

let scratch_view =
  div []
    [ nav []
        [ button [onClick Save_scratch] [text "save this game"]
        ; button [onClick Load_tournament] [text "load a game from Lichess"]
        ]
    ; p []
        [ text "This is a scratch buffer.  Moves you make will not be saved in the browser's local storage." ]
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
              | _ -> [game_view]
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
