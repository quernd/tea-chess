include Ochess

type file = int
type rank = int

type square = file * rank

let char_of_file file = "abcdefgh".[file]
let char_of_rank rank = "12345678".[rank]

let string_of_piece_type = function
  | King -> "king"
  | Queen -> "queen"
  | Rook -> "rook"
  | Bishop -> "bishop"
  | Knight -> "knight"
  | Pawn -> "pawn"

let string_of_color = function
  | White -> "white"
  | Black -> "black"
