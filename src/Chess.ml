include Ochess

type file = int
type rank = int
type square = file * rank

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

