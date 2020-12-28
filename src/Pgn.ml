open Opal

let maybe p = (p >>= fun x -> return (Some x)) <|> return None

exception Ambiguous_move
exception Parse_error

(* PGN STRUCTURE *)

type tag_name = string
type tag_value = string
type tag_pair = tag_name * tag_value
type header = tag_pair list

type comment = string
type nag = Nag.t
type san = string

type result = Chess.game_status option

type piece_type = Chess.piece_type
type promotion = piece_type

type file = Chess.file
type rank = Chess.rank

type square = file * rank

type token_move =
  | Queenside_castle
  | Kingside_castle
  | Piece_move of piece_type * file option * rank option * square 
  | Pawn_move of file option * square * promotion option

type line = move list
and move =
  { move : token_move
  ; pre_comments : comment list
  ; post_comments : comment list
  ; nags : nag list
  ; rav : line list
  }

type game =
  { header : header
  ; moves : line
  ; result : result
  }


(* PGN PARSER *)

let string = none_of ['"'] |> many
let tag_key = none_of [' '] |> many1
let tag_value = exactly '"' >> string << exactly '"'
let tag_key_value =
  tag_key << many1 space >>= fun tag_key ->
  tag_value >>= fun tag_value ->
  return (tag_key |> implode, tag_value |> implode)

let tag = exactly '[' >> tag_key_value << exactly ']'
let header = many (tag << newline)

let piece =
  one_of ['K'; 'Q'; 'R'; 'B'; 'N'; 'P'] => Chess.piece_type_of_char

let file =
  let files = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'] in
  List.mapi (fun i file -> (exactly file >> return i)) files |> choice

let rank =
  let ranks = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'] in
  List.mapi (fun i file -> (exactly file >> return i)) ranks |> choice

let square =
  file >>= fun file -> 
  rank >>= fun rank -> return (file, rank)

let capture = exactly 'x'
let check = exactly '+'
let checkmate = exactly '#'

let promotion = maybe (exactly '=') >> piece
let nag = (exactly '$' >> (many1 digit => implode => int_of_string)) <|>
          (many1 (one_of ['!';'?']) => implode >>= (fun nag ->
               try Nag.nag_of_string nag |> return
               with Nag.Nag_error _ -> raise Parse_error))

(* nonstandard 0-0 castle *)
let castle =
  (token "O-O-O" <|> token "0-0-0" >> return Queenside_castle) <|>
  (token "O-O" <|> token "0-0" >> return Kingside_castle)

let disambiguated_move =
  piece >>= fun piece ->
  maybe file >>= fun file ->
  maybe rank >>= fun rank ->
  optional capture >>
  square => fun destination ->
    Piece_move (piece, file, rank, destination)

let normal_move =
  piece >>= fun piece ->
  optional capture >>
  square => fun destination ->
    Piece_move (piece, None, None, destination)

let pawn_move =
  square >>= fun destination ->
  maybe promotion => fun promotion ->
    Pawn_move (None, destination, promotion)

let pawn_capture_move =
  maybe file >>= fun source_file ->
  optional capture >>
  square >>= fun destination ->
  maybe promotion => fun promotion ->
    Pawn_move (source_file, destination, promotion)

let san =
  (castle <|>
   disambiguated_move <|>
   normal_move <|>
   pawn_move <|>
   pawn_capture_move) <<
  optional (check <|> checkmate)

let comment =
  (exactly '{' >> many (none_of ['}']) << exactly '}') => implode

(* nonstandard `. ...` *)
let number =
  many1 digit << (token ". ..." <|> token "..." <|> token ".")

let rec var () =
  exactly '(' >>
  lexeme (line ()) <<
  lexeme (exactly ')')

(* comments can be sequential *)
(* TODO: semicolon until end-of-line comment *)
and move () =
  many (lexeme comment) >>= fun pre_comments ->
  maybe (lexeme number) >> lexeme san >>= fun move ->
  many (lexeme nag) >>= fun nags ->
  many (lexeme comment) >>= fun post_comments ->
  many (lexeme (var ())) >>= fun rav ->
  return { pre_comments; move; nags; post_comments; rav }

and line () =
  sep_by (move ()) space

(* nonstandard 1/2 *)
let result =
  let open Chess in
  (token "1-0" >> return (Some (Win White))) <|>
  (token "1/2-1/2" >> return (Some Draw)) <|>
  (token "1/2" >> return (Some Draw)) <|>
  (token "0-1" >> return (Some (Win Black))) <|>
  (token "*" >> return None)

(* nonstandard non-required newline *)
let game =
  lexeme header << (maybe newline) >>= fun header ->
  line () << spaces >>= fun moves ->
  result >>= fun result -> return { header; moves; result }


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


let parse_pgn s =
  LazyStream.of_string s |> parse game
