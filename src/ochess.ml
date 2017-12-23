(**************************************************************************

   O'Chess - a simple chess engine in O'Caml

   Copyright (c) 2005, Oleg Trott <ot14@columbia.edu>


   To compile:  ocamlopt -o ochess str.cmxa ochess.ml

   To run:      xboard -fcp ochess


   Version: 0.2.3 (alpha)



   The MIT License

   Copyright (c) 2005  Oleg Trott <ot14@columbia.edu>

   Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the "Software"), to deal in 
   the Software without restriction, including without limitation the rights to 
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
   of the Software, and to permit persons to whom the Software is furnished to do 
   so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all 
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
   SOFTWARE.

 ******************************************************************************)

open Printf
open Sys
(* open Str *)
let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

(* 
    Chess rules
*)

type color = Black | White

type piece_type = King | Queen | Rook | Bishop | Knight | Pawn

type piece = piece_type * color

type field = Piece of piece | Empty

type can_castle = bool * bool

type position = { ar: field array array; 
                  turn: color; 
                  cas_w : can_castle; cas_b : can_castle; 
                  en_passant : int option; 
                  prev : position option; 
                  irr_change : int; (* number of moves since capture or pawn move *)
                  king_w : (int*int); king_b : (int * int); (* king positions are required often *)
                  number : int; (* position number, initial position is 0 *)
                  eval : int} (* essentially, memoized value of eval for this position *)

let identical_positions x y = 
  x.eval = y.eval && (* if evals differ, positions are different *)
  x.turn = y.turn &&
  x.cas_w = y.cas_w && x.cas_b = y.cas_b &&
  x.en_passant = y.en_passant &&
  x.ar = y.ar

let rec draw_by_repetition_aux pos p n = 
  if n <= 0 then true else
    (* if an irreversible change took place recently, 
       there could not have been enough repetitions *)
  if 2*n > p.irr_change then false else
    match p.prev with
    | None -> false
    | Some pp -> if identical_positions pos pp 
      then draw_by_repetition_aux pos pp (n-1) 
      else draw_by_repetition_aux pos pp n

(* when this position is identical to two that already appeared *)

let draw_by_repetition pos = draw_by_repetition_aux pos pos 2

(* 50 moves by each player without capture or pawn move -> draw *)

let draw_by_lack_of_progress pos = pos.irr_change >= 100

let draw pos = draw_by_lack_of_progress pos || draw_by_repetition pos

type move = Move of int * int * int * int 
          | Kingside_castle | Queenside_castle 
          | Promotion of piece_type * int * int (* the int's are initial column and final column *)

let opposite_color = function Black -> White | White -> Black

let color_sign = function White -> 1 | Black -> -1

let piece_chars = [(King, 'K'); (Queen, 'Q'); (Rook, 'R'); (Bishop, 'B'); (Knight, 'N'); (Pawn, 'P')]

let char_of_piece_type pt = List.assoc pt piece_chars

(* O'Caml needs this in its standard library: *)

let rec rassoc x lst =
  match lst with
  | [] -> raise Not_found
  | (a, b) :: t -> if x = b then a else rassoc x t

let piece_type_of_char c = rassoc (Char.uppercase c) piece_chars

let int_of_letter x = 
  let x = Char.lowercase x in
  if 'a' <= x && x <= 'h' 
  then int_of_char x - int_of_char 'a'
  else raise Not_found

let letter_of_int x = "abcdefgh".[x]

(*   Board printing. Useful for debugging and text interface: *)

let print_board ar = 
  let separator = "\n   +----+----+----+----+----+----+----+----+\n" in
  print_string separator;
  for j = 7 downto 0 do
    printf " %d |" (j + 1);
    for i = 0 to 7 do
      match ar.(i).(j) with
      | Piece(pt, c) -> printf " %c%c |" (if c = White then ' ' else '*') (char_of_piece_type pt) 
      | Empty -> print_string "    |"
    done;
    print_string separator;
  done;
  print_string "\n      a    b    c    d    e    f    g    h\n"

let print_position p =
  print_board p.ar;
  ( match p.turn with
    | White -> printf "\n White's turn to move\n\n"
    | Black -> printf "\n Black's turn to move\n\n")

(* initial position *) 

let init_position = 
  let init_array = Array.make_matrix 8 8 Empty in
  let put i j pt = init_array.(i).(j) <- Piece(pt, White); 
    init_array.(i).(7-j) <- Piece(pt, Black) in
  for i = 0 to 7 do
    put i 1 Pawn
  done;
  put 0 0 Rook; put 7 0 Rook;
  put 1 0 Knight; put 6 0 Knight;
  put 2 0 Bishop; put 5 0 Bishop;
  put 3 0 Queen; put 4 0 King;
  {ar = init_array; turn = White; 
   cas_w = (true, true); cas_b = (true, true); 
   king_w = (4,0); king_b = (4,7);
   en_passant = None;
   prev = None; eval = 0; irr_change = 0; number = 0}

let within_range i = i >= 0 && i <= 7
let within_range2 (i, j) = within_range i && within_range j

let copy_matrix m = let tmp = Array.copy m in
  Array.iteri (fun i elt -> tmp.(i) <- Array.copy elt) m; tmp

let no_k_castle (x, _) = (x, false)

let no_q_castle (_, x) = (false, x)


exception Illegal_move

(* make_move: current_position -> move -> new_position
   this function does not check move validity, which is done elsewhere *)

let make_move pos m del = 
  let ar2 = copy_matrix pos.ar in 
  let mv x1 y1 x2 y2 = ar2.(x2).(y2) <- ar2.(x1).(y1); ar2.(x1).(y1) <- Empty in
  let tmp = {pos with ar = ar2; turn = opposite_color pos.turn; en_passant = None; prev = Some pos; 
                      eval = -pos.eval+del; number = pos.number + 1} in
  let t = pos.turn in
  let ic = ref false in
  let ret = 
    (match m with
     | Queenside_castle -> ( match t with 
         | White -> mv 0 0 3 0; mv 4 0 2 0; {tmp with cas_w = (false, false); king_w = (2,0)}
         | Black -> mv 0 7 3 7; mv 4 7 2 7; {tmp with cas_b = (false, false); king_b = (2,7)}
       )
     | Kingside_castle -> ( match t with 
         | White -> mv 7 0 5 0; mv 4 0 6 0; {tmp with cas_w = (false, false); king_w = (6,0)} 
         | Black -> mv 7 7 5 7; mv 4 7 6 7; {tmp with cas_b = (false, false); king_b = (6,7)}
       )
     | Promotion(pt, x1, x2) -> 
       ic := true;
       let (y1, y2) = match t with White -> (6, 7) | Black -> (1, 0) in
       mv x1 y1 x2 y2; ar2.(x2).(y2) <- Piece(pt, pos.turn); tmp;
     | Move(x1, y1, x2, y2) -> 
       ((match pos.ar.(x2).(y2) with Empty -> ic := false | _ -> ());
        mv x1 y1 x2 y2;
        let p = (match ar2.(x2).(y2) with Piece(x, _) -> x | Empty -> raise Illegal_move) in
        ( match p with
          | King -> (match pos.turn with 
              | White -> {tmp with cas_w = (false, false); king_w = (x2,y2)} 
              | Black -> {tmp with cas_b = (false, false); king_b = (x2,y2)})
          | Rook -> (match (pos.turn, x1, y1) with
              | (White, 0, 0) -> {tmp with cas_w = no_q_castle(pos.cas_w)}
              | (White, 7, 0) -> {tmp with cas_w = no_k_castle(pos.cas_b)}
              | (Black, 0, 7) -> {tmp with cas_b = no_q_castle(pos.cas_w)}
              | (Black, 7, 7) -> {tmp with cas_b = no_k_castle(pos.cas_b)}
              | _ -> tmp)
          | Pawn ->
            (* created an en passant situation ? *)
            ( ic := true;
              let e2 = 
                ( match (ar2.(x2).(y2), pos.turn) with
                  | Piece(Pawn, White), White when x1 = x2 && y1 = 1 && y2 = 3 -> Some(x1)
                  | Piece(Pawn, Black), Black when x1 = x2 && y1 = 6 && y2 = 4 -> Some(x1)
                  | _ -> None) in
              (* captured an en passant pawn ? *)
              ( match (pos.en_passant) with
                | Some(i) when i = x2 -> 
                  ( match pos.turn with 
                    | White -> if y2 = 5 then ar2.(i).(4) <- Empty 
                    | Black -> if y2 = 2 then ar2.(i).(3) <- Empty)
                | _ -> ());
              {tmp with en_passant = e2})
          | Knight | Bishop | Queen -> tmp)))
  in {ret with irr_change = (if !ic (* || pos.cas_w <> ret.cas_w || pos.cas_b <> ret.cas_b *) then 0 else pos.irr_change + 1)}



(* Generate possible moves *)

let knight = [(1, 2); (1, -2); (2, 1); (2, -1); (-1, 2); (-1, -2); (-2, 1); (-2, -1)]

let rook = [(-1, 0); (0, -1); (0, 1); (1, 0)]

let bishop = [(-1,-1); (-1, 1); (1, -1); (1, 1)]

let queen = bishop @ rook

let pawn_white_mv  = [(0, 1)]
let pawn_black_mv  = [(0,-1)]
let pawn_white_cap = [(-1,1); (1,1)]
let pawn_black_cap = [(-1,-1); (1,-1)]

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let rec explore_direction (pos:position) x_init y_init x y dx dy num (acc : move list) = 
  if num < 1 then acc else
    let (x2, y2) = add (x, y) (dx, dy) in
    if within_range2 (x2, y2) then
      (match pos.ar.(x2).(y2) with
       | Piece(_pt, c) when c <> pos.turn -> Move(x_init, y_init, x2, y2) :: acc
       | Empty -> explore_direction pos x_init y_init x2 y2 dx dy (num - 1) (Move(x_init, y_init, x2, y2) :: acc)
       | Piece(_pt, _c) -> acc)
    else acc

let explore_directions pos x y lst num =
  List.flatten (List.map (fun direction -> explore_direction pos x y x y (fst direction) (snd direction) num []) lst)


(* Procedures to see if the king is checked
   Castling can not happen from or over checked fields *)

let rec checked_direction pos t pt_list x y dx dy num = (* t is king's color *)
  if num < 1 then false else
    let (x2, y2) = add (x, y) (dx, dy) in
    if within_range2 (x2, y2) then
      (match pos.ar.(x2).(y2) with
       | Piece(pt, c) -> c <> t &&  List.mem pt pt_list
       | Empty -> checked_direction pos t pt_list x2 y2 dx dy (num - 1))
    else false

let rec checked_directions pos c pt_list x y lst num = 
  (match lst with
   | [] -> false
   | (dx,dy) :: t -> (  checked_direction  pos c pt_list x y   dx dy num 
                        || checked_directions pos c pt_list x y t       num))

let checked pos t x y = 
  checked_directions pos t [Rook;   Queen] x y rook   10 ||
  checked_directions pos t [Bishop; Queen] x y bishop 10 ||
  checked_directions pos t [Knight]        x y knight 1 ||
  checked_directions pos t [King]          x y queen  1 ||
  let tmp = if t = White then pawn_white_cap else pawn_black_cap in
  checked_directions pos t [Pawn]          x y tmp    1


let find_king pos c = Some (match c with White -> pos.king_w | Black -> pos.king_b)

(*
let find_king pos c = 
   let tmp = ref None in
   for i = 0 to 7 do
       for j = 0 to 7 do
           match pos.ar.(i).(j) with
           | Piece(King, x) when x = c -> tmp := Some (i, j)
           | _ -> ()
        done
    done;
    (* FIXME *) 
    (match !tmp with 
    | None -> None 
    | Some (x, y) -> (assert ((x,y) = (match c with White -> pos.king_w | Black -> pos.king_b)); !tmp))
    (* !tmp *)
*)

let king_checked pos c = 
  match find_king pos c with
  | Some (i, j) -> checked pos c i j
  | None -> false

(* Procedures needed to see if castling can be done *)

let rec empty_segment pos row x y = 
  if x > y then true else
  if pos.ar.(x).(row) <> Empty then false else empty_segment pos row (x+1) y

let check_castle pos still_can row next last rk = 
  still_can && pos.ar.(rk).(row) = Piece(Rook, pos.turn) &&
  empty_segment pos row (min next last) (max next last) && (not (checked pos pos.turn 4 row)) && 
  (let a = copy_matrix pos.ar in
   a.(4).(row)    <- Empty; 
   a.(next).(row) <- Piece(King,pos.turn);
   not (checked {pos with ar = a} pos.turn next row))

let castle_moves pos =
  let ((q,k),row) = match pos.turn with White -> (pos.cas_w,0) | Black -> (pos.cas_b,7) in
  (if check_castle pos q row 3 1 0 then [Queenside_castle] else []) @
  (if check_castle pos k row 5 6 7 then [Kingside_castle]  else [])

(* Generate all legal moves *)


(* return all moves that are legal and start at a given position, ignore checking *)

let possible_field_simple_moves (pos:position) x y =
  let exp1 lst = explore_directions pos x y lst 1
  and expn lst = explore_directions pos x y lst 10 in
  match pos.ar.(x).(y) with
  | Piece(p, t) when t = pos.turn -> (match p with 
      | King -> castle_moves pos @ exp1 queen
      | Queen -> expn queen
      | Rook -> expn rook
      | Bishop -> expn bishop
      | Knight -> exp1 knight
      | Pawn -> let y2 = (match t with White -> y + 1 | Black -> y - 1) 
        and tmp = ref [] in
        let also some_move = tmp := some_move :: !tmp 
        and officers = [Bishop; Knight; Rook; Queen] in
        if (y2 = 7 && t = White) || (y2 = 0 && t = Black) then
          (* the pawn is about to be promoted *)
          ((match pos.ar.(x).(y2) with 
              | Empty -> List.iter (fun pt -> also (Promotion(pt, x, x))) officers
              | _ -> ());
           List.iter (fun x2 -> if within_range x2 then 
                         (match pos.ar.(x2).(y2) with
                          | Piece(_, c) when c <> t-> List.iter (fun pt -> also (Promotion(pt, x, x2))) officers
                          | _ -> ())
                     ) [x - 1; x + 1])
        else
          ((match pos.ar.(x).(y2) with Empty -> also (Move(x, y, x, y2)) | _ -> ());
           List.iter (fun x2 -> if within_range x2 then 
                         (match pos.ar.(x2).(y2) with
                          | Piece(_, c) when c <> t -> also (Move(x, y, x2, y2))
                          | _ -> ())
                     ) [x - 1; x + 1]);
        (* see if the pawn can also go two steps forward *)
        (match (y, t) with
         | (1, White) -> (match (pos.ar.(x).(2), pos.ar.(x).(3)) with 
             | (Empty, Empty) -> also (Move(x, y, x, 3)) 
             | _ -> ())
         | (6, Black) -> (match (pos.ar.(x).(5), pos.ar.(x).(4)) with 
             | (Empty, Empty) -> also (Move(x, y, x, 4))
             | _ -> ())
         | _ -> ());
        (* see if the pawn can capture en passant pawns *)
        (match pos.en_passant with
         | Some(i) -> let y_en_passant = (match t with White -> 5 | Black -> 2) in
           List.iter (fun x2 -> if within_range x2 && x2 = i && y2 = y_en_passant then also (Move(x, y, x2, y2))
                     ) [x - 1; x + 1];
         | None -> ());
        !tmp;
    )
  | Piece(_, _) -> []
  | Empty -> []

(* Generate all legal moves, but ignore checking *)

let possible_moves (pos:position) = 
  let tmp = ref [] in
  for x = 0 to 7 do
    for y = 0 to 7 do
      tmp := possible_field_simple_moves pos x y @ !tmp;
    done
  done;
  !tmp

(* 
    The evaluation function for chess
*)

(* let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2) *)

(* reward being in the center *)

let board_center = [|
  [|  0;  0;  0;  0;  0;  0;  0;  0|];
  [|  0;  0;  0;  0;  0;  0;  0;  0|];
  [|  0;  0;  3;  5;  5;  3;  0;  0|];
  [|  0;  0;  5;  9;  9;  5;  0;  0|];
  [|  0;  0;  5;  9;  9;  5;  0;  0|];
  [|  0;  0;  3;  5;  5;  3;  0;  0|];
  [|  0;  0;  0;  0;  0;  0;  0;  0|];
  [|  0;  0;  0;  0;  0;  0;  0;  0|]
|]


let mirror_y c y = (match c with White -> y | Black -> 7 - y)

let piece_value pt x y c = 
  let y = mirror_y c y in
  (match pt with
   | King -> 1000000 - 5*board_center.(x).(y)
   | Queen -> 9000 + 10*board_center.(x).(y)
   | Rook -> 5000 + 10*board_center.(x).(y)
   | Knight -> 3000 + 30*board_center.(x).(y)
   | Bishop -> 3200 + 20*board_center.(x).(y)
   | Pawn -> 1000 + 30*board_center.(x).(y)) + 5*y

let field_value pos x y = 
  (match pos.ar.(x).(y) with
   | Empty -> 0
   | Piece(pt, c) -> piece_value pt x y c)

let win = piece_value King 0 0 White / 2

let delta pos mv = 
  if abs pos.eval >= win then 0 else 
    (match mv with
     | Kingside_castle ->  500
     | Queenside_castle -> 400
     | Move(x1, y1, x2, y2) -> 
       (match pos.ar.(x1).(y1) with
        | Empty -> raise Illegal_move
        | Piece(pt, c) -> piece_value pt x2 y2 c - piece_value pt x1 y1 c + field_value pos x2 y2)
     | Promotion(pt, x1, x2) -> let (y1,y2) = match pos.turn with White -> (6,7) | Black -> (1, 0) in
       piece_value pt x2 y2 pos.turn - piece_value Pawn x1 y1 pos.turn + field_value pos x2 y2)


(* 
    Alpha-Beta pruning
*)


exception Interrupt

let deadline = ref None

let check_timer () = 
  (match !deadline with
   | None -> ()
   | Some x -> if time () > x then raise Interrupt)

let set_timer interval =  (* in seconds *)
  deadline := Some (time () +. interval)

let del_timer () = deadline := None


exception Illegal_position


(* To save time, alpha_beta search generates all moves, i.e. including the ones
   that are illegal (result in the friendly king being checked). Such moves
   are rejected in the next ply: by throwning Illegal_position.

   Checkmates and stalemates need to be distinguished from the situations
   resulting in the heuristic value falling outside of the [achievable .. cuttoff] window *)

type best_move = Bad_moves | No_moves | Good of move

let rec alpha_beta pos depth achievable cutoff =
  check_timer ();
  let pm = List.map (fun m -> delta pos m, m) (possible_moves pos) in
  let pm = List.sort (fun (x, _) (y, _) -> compare y x) pm in (* sort in decreasing order *)
  (match pm with 
   | (d,_m) :: _ when d >= win -> raise Illegal_position (* king can be captured *)
   | _ -> ());
  let rec loop ach cut lst best =
    if ach >= cut then (ach, best) else (* PRUNING *)
      (match lst with
       | [] -> (ach, best)
       | (d,m) :: tl -> 
         (try
            let p_new = make_move pos m d in
            let v = if draw p_new 
              then 0 
              else - fst (alpha_beta p_new (depth - 1) (- cut) (- ach)) 
            in if v > ach
            then loop v   cut tl (Good m)
            else loop ach cut tl (match best with No_moves -> Bad_moves | x -> x)
          with Illegal_position -> loop ach cut tl best)) in
  let prelim = 
    (match pm with
     | []         -> (0, No_moves)
     | (d,m) :: _ -> if depth <= 1 then (-pos.eval+d, Good m) else loop achievable cutoff pm No_moves) 
  in (match prelim with
      | (_, No_moves)  -> if king_checked pos pos.turn then (-win, None) else (0, None)
      | (v, Bad_moves) -> (v, None)  (* this shouldn't show up in the top-level call to a/b though *)
      | (v, Good m)    -> (v, Some m))

let alpha_beta_search pos depth = alpha_beta pos depth (-100*win) (100*win)

(*
let legal_moves pos = List.filter (fun m -> let (v,_) = alpha_beta_search (make_move pos m 0) 1 in v < win) 
                                  (possible_moves pos)
*)

let legal_moves pos = List.filter (fun m -> let p_new = make_move pos m 0 in not (king_checked p_new pos.turn))
    (possible_moves pos)
let max_depth = 10

let alpha_beta_deepening pos interval = 
  del_timer ();
  let current_best = ref (alpha_beta_search pos 2) in (* alpha_beta_seach _ 2   can only return legal moves *)
  ((try 
      set_signal sigint (Signal_handle (fun _ -> raise Interrupt));
      set_timer interval;
      let rec loop i = 
        if i > max_depth then () else
          let tmp = alpha_beta_search pos i in
          current_best := tmp;
          if (fst tmp) >=  win (* we can checkmate *)
          || (fst tmp) <= -win (* we get checkmated anyway, deny the opponent extra time to think *)
          then () else loop (i+1) 
      in loop 3;
      set_signal sigint Signal_ignore;
      del_timer ();
    with Interrupt -> ());
   set_signal sigint Signal_ignore;
   del_timer ();
   !current_best)

(* 
    XBoard interface
*)

let previous pos = 
  match pos.prev with
  | None -> pos
  | Some p -> p

let print_move (p:position) = function (* does not check validity *)
  | Move(x1, y1, x2, y2) -> 
    ( match p.ar.(x1).(y1) with 
      | Piece(_pt, _) -> printf "%c%d%c%d" (letter_of_int x1) (y1 + 1) (letter_of_int x2) (y2 + 1)
      | _ -> raise Illegal_move )
  | Kingside_castle -> printf "O-O"
  | Queenside_castle -> printf "O-O-O"
  | Promotion(pt, x1, x2) -> 
    let (y1, y2) = ( match p.turn with White -> (6,7) | Black -> (1, 0)) in
    printf "%c%d%c%d%c" (letter_of_int x1) (y1 + 1) (letter_of_int x2) (y2 + 1) (Char.lowercase (char_of_piece_type pt))

let is_digit c = '0' <= c && c <= '9'

let int_of_char c = int_of_char c - int_of_char '0' - 1

let parse_move_string str = 
  try 
    Some (match str with
        | "O-O"   -> Kingside_castle
        | "O-O-O" -> Queenside_castle
        | s -> 
          (match String.length s with
           | 4 -> Move(int_of_letter s.[0], int_of_char   s.[1], int_of_letter s.[2], int_of_char   s.[3])
           | 5 when (is_digit s.[1] && is_digit s.[3]) ->
             Promotion(piece_type_of_char s.[4], int_of_letter s.[0], int_of_letter s.[2])
           | _ -> raise Exit))
  with _ -> None

let parse_move pos str = 
  (match parse_move_string str with
   | Some (Move (4, 0, 6, 0)) when pos.ar.(4).(0) = Piece(King, White) -> Some(Kingside_castle)
   | Some (Move (4, 0, 2, 0)) when pos.ar.(4).(0) = Piece(King, White) -> Some(Queenside_castle)
   | Some (Move (4, 7, 6, 7)) when pos.ar.(4).(7) = Piece(King, Black) -> Some(Kingside_castle)
   | Some (Move (4, 7, 2, 7)) when pos.ar.(4).(7) = Piece(King, Black) -> Some(Queenside_castle)
   | v -> v)

type game_status = Win of color | Draw | Play of move list

let game_status pos = 
  if draw pos then Draw else
    (match legal_moves pos with
     | [] -> if king_checked pos pos.turn
       then Win (opposite_color pos.turn)
       else Draw
     | lm -> Play lm)

let best_move pos interval = 
  (match legal_moves pos with
   | [x] -> Some x  (* don't think when there is only one move *)
   | _   -> snd (alpha_beta_deepening pos interval))


type clock = Conventional of int * float | Incremental of float * float | Exact of float (* three types of time controls *)

let init_rem_time = function
  | Exact x -> x
  | Incremental (x, _y) -> x
  | Conventional (_n, y) -> y

let thinking_interval moves_made cl remaining =  
  (match cl with
   | Exact x -> 0.95 *. x
   | Incremental (_x, y) -> let est_moves_to_play = max 20 (50 - moves_made) in
     0.95 *. min remaining (remaining /. float est_moves_to_play +. y)
   | Conventional (n, _y) -> let moves_until_incr = n - moves_made mod n + 1
     and est_moves_to_play = max 20 (50 - moves_made) in
     let rem_moves = min moves_until_incr est_moves_to_play in
     0.95 *. remaining /. float (rem_moves + 2))



let update_remaining _moves_made cl remaining interval =  (* in case the front end does not send 'time' *)
  (match cl with
   | Exact x -> x
   | Incremental (x, y) -> x +. y -. interval
   | Conventional (_, _) -> remaining -. interval)

type state = {pos : position; c : color; cl : clock; rem_time : float; gui : bool}

let parse_time str = 
  (match split_on_char ':' str with
   | [x] -> 60.0 *. float_of_string x
   | [x; y] -> 60.0 *. float_of_string x +. float_of_string y
   | _ -> raise Exit)

let flush () = Pervasives.flush Pervasives.stdout

let parse_level a1 a2 a3 =  (* time control command Xboard calls 'level' *)
  let a1 = int_of_string a1
  and a2 = parse_time a2
  and a3 = float_of_string a3 in
  if a1 <= 0 then Incremental (a2, a3) else Conventional (a1, a2)

let main () = 
  set_signal sigint Signal_ignore; (* xboard sends these *)
  printf " Ctrl-C is bound to \"move now!\". Use Ctrl-D to quit\n";
  let x0 = {pos = init_position; c = Black; cl = Exact 3.0; rem_time = 30000.0; gui = false} in
  let rec think x = (* mutually recursive 'think' and 'interact' *)
    if not x.gui then (print_position x.pos; flush ()) else ();
    let game_over msg = print_string msg; flush (); interact x [] in
    (match game_status x.pos with
     | Draw      -> game_over "1/2-1/2 {Draw}\n"
     | Win White -> game_over "1-0 {White wins}\n"
     | Win Black -> game_over "0-1 {Black wins}\n"
     | Play lm ->
       assert (lm <> []);
       if x.c = x.pos.turn 
       then 
         let interval = thinking_interval (x.pos.number / 2)  x.cl x.rem_time in
         (match best_move x.pos interval with None -> assert false | Some mv -> 
             (assert (List.mem mv lm);
              printf "move "; print_move x.pos mv; print_newline (); flush ();
              let pos_new = make_move x.pos mv (delta x.pos mv) in
              think {x with pos = pos_new; rem_time = update_remaining (x.pos.number / 2) x.cl x.rem_time interval}))
       else interact x lm)
  and interact x lst = (* mutually recursive 'think' and 'interact' *)
    let str = read_line () in
    let ignoring str = printf " Ignoring command %s\n" str; flush (); think x in
    (match split_on_char ' ' str with
     | ["xboard"] -> (printf "feature myname=\"O'Chess\" done=1\n"; flush (); think {x with gui = true})
     | ["hard"] | ["easy"] | ["random"] | ["?"] | ["force"] | ["draw"]  (* ignored *)
     | "result" :: _ | ["otim"; _ ] -> think x                          (* ignored *)
     | ["level"; a1; a2; a3] -> (try let tmp = parse_level a1 a2 a3 in 
                                   think {x with cl = tmp; rem_time = init_rem_time tmp} 
                                 with _ -> think x)
     | ["st"; a1] -> (try let tmp = Exact (float_of_string a1) in (* the time is in seconds, not minutes[:seconds] *)
                        think {x with cl = tmp; rem_time = init_rem_time tmp} 
                      with _ -> think x)
     | ["remove"] -> think {x with pos = (previous (previous x.pos))}
     | ["white"] -> think {x with c = White}
     | ["black"] -> think {x with c = Black}
     | ["quit"] ->  raise Exit
     | ["new"]  -> think {x0 with gui = x.gui}
     | ["time"; t_str] -> (try think {x with rem_time = float_of_string t_str /. 100.0} 
                           with _ -> ignoring str)
     | _ -> 
       (match parse_move x.pos str with
        | None -> ignoring str
        | Some mv -> 
          if List.mem mv lst 
          then let pos_new = make_move x.pos mv (delta x.pos mv) in
            think {x with pos = pos_new}
          else (printf "Illegal move: %s\n" str; flush (); think x)))
  in try think x0 with End_of_file | Exit -> flush ()


(* 
    Start playing
*)

let () = main ()

