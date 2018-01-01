module Opal = struct
  include Opal

  let maybe p = option None (p >>= fun x -> return (Some x))

  (* tail-recursive fold_right *)
  let fold_right f init p =
    let rec loop acc input =
      match p input with
      | Some (res, input') ->
        loop (fun tl -> acc (f res tl)) input'
      | None -> Some (acc init, input)
    in loop (fun x -> x)

  let many p = fold_right
      (fun x xs -> x :: xs) [] p

  (* let implode chars =
   *   let buf = Buffer.create 16 in
   *   List.iter (Buffer.add_char buf) chars;
   *   Buffer.contents buf *)

end

exception Ambiguous_move
exception Parse_error

(* PGN STRUCTURE *)

type pgn_tag_name = string
type pgn_tag_value = string
type pgn_tag_pair = pgn_tag_name * pgn_tag_value
type pgn_header = pgn_tag_pair list

type pgn_comment = string
type pgn_nag = string
type pgn_san = string

type pgn_result =
  | Win of Chess.color
  | Draw

type piece_type = Chess.piece_type
type promotion = piece_type

type file = Chess.file
type rank = Chess.rank

type square = file * rank

type pgn_tokenmove =
  | Queenside_castle
  | Kingside_castle
  | Piece_move of piece_type * file option * rank option * square 
  | Pawn_move of file option * square * promotion option

type pgn_move = { pre_comment: pgn_comment list
                ; move: pgn_tokenmove
                ; nags: pgn_nag list
                ; rav: pgn_rav
                ; post_comment: pgn_comment list
                }
and pgn_rav = pgn_line list
and pgn_line = pgn_move list

type pgn_game = pgn_header * pgn_line * pgn_result option


(* PGN PARSER *)

open Opal

let pgn_string = none_of ['"'] |> many
let pgn_tag_key = none_of [' '] |> many1
let pgn_tag_value = exactly '"' >> pgn_string << exactly '"'
let pgn_tag_key_value =
  pgn_tag_key << many1 space >>= fun tag_key ->
  pgn_tag_value >>= fun tag_value ->
  return (tag_key |> implode, tag_value |> implode)

let pgn_tag = exactly '[' >> pgn_tag_key_value << exactly ']'
let pgn_header = many (pgn_tag << newline)

let pgn_piece =
  one_of ['K'; 'Q'; 'R'; 'B'; 'N'; 'P'] => Chess.piece_type_of_char

let pgn_file =
  let files = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'] in
  List.mapi (fun i file -> (exactly file >> return i)) files |> choice

let pgn_rank =
  let ranks = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'] in
  List.mapi (fun i file -> (exactly file >> return i)) ranks |> choice

let pgn_square =
  pgn_file >>= fun file -> 
  pgn_rank >>= fun rank -> return (file, rank)

let pgn_capture = exactly 'x'
let pgn_check = exactly '+'
let pgn_checkmate = exactly '#'

let pgn_promotion = (maybe (exactly '=')) >> pgn_piece
let pgn_nag = (exactly '$' >> (many1 digit => implode)) <|>
              (many1 (one_of ['!';'?']) => implode)

(* nonstandard 0-0 castle *)
let pgn_castle =
  (token "O-O-O" <|> token "0-0-0" >> return Queenside_castle) <|>
  (token "O-O" <|> token "0-0" >> return Kingside_castle)

let pgn_disambiguated_move =
  pgn_piece >>= fun piece ->
  maybe pgn_file >>= fun file ->
  maybe pgn_rank >>= fun rank ->
  optional pgn_capture >>
  pgn_square => fun destination ->
    Piece_move (piece, file, rank, destination)

let pgn_normal_move =
  pgn_piece >>= fun piece ->
  optional pgn_capture >>
  pgn_square => fun destination ->
    Piece_move (piece, None, None, destination)

let pgn_pawn_move =
  pgn_square >>= fun destination ->
  maybe pgn_promotion => fun promotion ->
    Pawn_move (None, destination, promotion)

let pgn_pawn_capture_move =
  maybe pgn_file >>= fun source_file ->
  optional pgn_capture >>
  pgn_square >>= fun destination ->
  maybe pgn_promotion => fun promotion ->
    Pawn_move (source_file, destination, promotion)

let pgn_san =
  (pgn_castle <|>
   pgn_disambiguated_move <|>
   pgn_normal_move <|>
   pgn_pawn_move <|>
   pgn_pawn_capture_move) <<
  optional (pgn_check <|> pgn_checkmate)

let pgn_comment =
  (exactly '{' >> many (none_of ['}']) << exactly '}') => implode

(* nonstandard `. ...` *)
let pgn_number =
  many1 digit << (token ". ..." <|> token "..." <|> token ".")

let rec pgn_rav () =
  (exactly '(') >>
  lexeme (pgn_line ()) <<
  lexeme (exactly ')')

(* comments can be sequential *)
(* TODO: semicolon until end-of-line comment *)
and pgn_move () =
  many (lexeme pgn_comment) >>= fun pre_comment ->
  maybe (lexeme pgn_number) >> lexeme pgn_san >>= fun move ->
  many (lexeme pgn_nag) >>= fun nags ->
  many (lexeme pgn_comment) >>= fun post_comment ->
  many (lexeme (pgn_rav ())) >>= fun rav ->
  return {pre_comment; move; nags; rav; post_comment}

and pgn_line () =
  sep_by (pgn_move ()) space

(* nonstandard 1/2 *)
let pgn_result =
  (token "1-0" >> return (Some (Win White))) <|>
  (token "1/2-1/2" >> return (Some Draw)) <|>
  (token "1/2" >> return (Some Draw)) <|>
  (token "0-1" >> return (Some (Win White))) <|>
  (token "*" >> return None)

(* nonstandard non-required newline *)
let pgn_game =
  lexeme pgn_header << (maybe newline) >>= fun header ->
  pgn_line () << spaces >>= fun movetext ->
  pgn_result >>= fun result -> return (header, movetext, result)


let unify_moves (position:Chess.position) pgn_move move =
  match pgn_move, move with
  | Queenside_castle, Chess.Queenside_castle -> true
  | Kingside_castle, Chess.Kingside_castle -> true
  | Pawn_move (s_file, (t_file, t_rank), Some p_type),
    Promotion (p_type', s_file', t_file') ->
    let promo_rank =
      match position.turn with Black -> 0 | White -> 7
    and s_rank' =
      match position.turn with Black -> 1 | White -> 6 in
    position.ar.(s_file').(s_rank') = Chess.Piece (Chess.Pawn, position.turn) &&
    Chess.unify s_file' s_file && t_file' = t_file &&
    promo_rank = t_rank && p_type' = p_type
  | Pawn_move (s_file, (t_file, t_rank), None),
    Move (s_file', s_rank', t_file', t_rank') ->
    position.ar.(s_file').(s_rank') = Chess.Piece (Chess.Pawn, position.turn) &&
    Chess.unify s_file' s_file && t_file' = t_file && t_rank' = t_rank
  | Piece_move (p_type, s_file, s_rank, (t_file, t_rank)),
    Move (s_file', s_rank', t_file', t_rank') ->
    position.ar.(s_file').(s_rank') =
    Chess.Piece (p_type, position.turn) &&
    Chess.unify s_file' s_file && Chess.unify s_rank' s_rank &&
    t_file' = t_file && t_rank' = t_rank    
  | _ -> false


let move_of_pgn_move position move =
  let moves = Chess.legal_moves position in
  let possible_moves = List.filter (unify_moves position move) moves in
  match possible_moves with
  | [] -> raise Chess.Illegal_move
  | [move'] -> move'
  | _ -> raise Ambiguous_move


let game_of_string string =
  let open Game in

  let advance (position, ply, acc) (move:pgn_move) =
    let move' = move_of_pgn_move position move.move in
    let position' = Chess.make_move position move' in
    let san = Chess.san_of_move position move' in
    let acc' = (move', san)::acc in
    (position', ply + 1, acc')
  in

  let pgn = LazyStream.of_string string |> parse pgn_game in
  match pgn with
  | Some (_, pgn', _) ->
    let position, ply, past =
      List.fold_left advance (Chess.init_position, 0, []) pgn' in
    {position; ply; moves = (past, [])}
  | None -> raise Parse_error

let string_of_game (game:Game.model) =
  let past, future = game.moves in
  let moves = List.rev_append past future in
  let sans = List.map snd moves in
  String.concat " " sans ^ " *"
