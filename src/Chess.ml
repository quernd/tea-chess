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

let home_rank = function White -> 0 | Black -> 7
let promotion_rank = function White -> 7 | Black -> 0
let pre_promotion_rank = function White -> 6 | Black -> 1

let coordinate_pairs turn = function
  | Queenside_castle -> (4, home_rank turn), (2, home_rank turn)
  | Kingside_castle -> (4, home_rank turn), (6, home_rank turn)
  | Promotion (_piece_type, s_file, t_file) ->
    (s_file, pre_promotion_rank turn), (t_file, promotion_rank turn)
  | Move (s_file, s_rank, t_file, t_rank) -> 
    (s_file, s_rank), (t_file, t_rank)
