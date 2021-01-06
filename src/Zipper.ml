type 'a context = 'a list
type 'a zipper = 'a context * 'a list

exception End_of_list
exception Beginning_of_list

(* move forward and return item and new zipper *)
let fwd (past, future) =
  match future with
  | [] -> raise End_of_list
  | hd::future' -> hd, (hd::past, future')

(* move back and return new zipper *)
let back (past, future) =
  match past with
  | [] -> raise Beginning_of_list
  | hd::past' -> past', hd::future

let fwd' item (past, future) =
  match future with
  | hd::future' when hd = item -> hd::past, future'
  | _ -> item::past, []

let init = [], []


(* TREE ZIPPER *)
(* https://pavpanchekha.com/blog/zippers/derivative.html *)

let id x = x

type 'a node = Node of 'a * 'a variation list
and 'a variation = Var of 'a * 'a line
and 'a line = 'a node list

(* line context represents position of a line in the game tree *)
type 'a line_context =
  | Main_line
  | Var_line of 'a tree_zipper        (* outer context *)
                * 'a                  (* main line move *)
                * 'a variation list   (* variations before *)
                * 'a                  (* move starting this variation *)
                * 'a variation list   (* variations after *)

(* tree zipper represents position in a line within a tree *)
and 'a tree_zipper = 'a line_context  (* line context *)
                      * 'a line       (* past *)
                      * 'a line       (* future *)


exception No_variations
exception No_next_variation
exception No_prev_variation
exception Not_in_variation
exception Not_beginning_of_list

let tree_fwd (context, past, future) =
  match future with
  | [] -> raise End_of_list
  | Node(item, _) as hd::future' -> item, (context, hd::past, future')

let tree_back (context, past, future) =
  match context, past with
  | context, hd::past' -> (context, past', hd::future)
  | _, [] -> raise Beginning_of_list

(* rewinds one step and jumps into the first variation *)
let tree_down (context, past, future) =
  match past with
  | [] -> raise Beginning_of_list
  | node::past' ->
    match node with
    | Node (_, []) -> raise No_variations
    | Node (main, Var (var, line)::variations) ->
      let context' =
        Var_line ((context, past', future), main, [], var, variations) in
      var, (context', [], line)

let tree_next (context, past, future) =
  match past, context with
  | [], Main_line -> raise No_next_variation
  | [], Var_line (_, _, _, _, []) -> raise No_next_variation
  | [], Var_line (context', main, left, var, Var (next, line)::right) ->
    let var' = Var_line
        (context', main, Var (var, future)::left, next, right) in
    next, (var', [], line)
  | _ -> raise Not_beginning_of_list

let tree_prev (context, past, future) =
  match past, context with
  | [], Main_line -> raise No_prev_variation
  | [], Var_line (_, _, [], _, _) -> raise No_prev_variation
  | [], Var_line (context', main, Var (prev, line)::left, var, right) ->
    let var' = Var_line
        (context', main, left, prev, Var (var, future)::right) in
    prev, (var', [], line)
  | _ -> raise Not_beginning_of_list

let tree_up (context, past, future) =
  match past, context with
  | [], Main_line -> raise Not_in_variation
  | [], Var_line ((context', past', future'), main, left, var, right) ->
    let variations = List.rev_append left (Var (var, future)::right) in
    let main_node = Node (main, variations) in
    main, (context', main_node::past', future')
  | _ -> raise Not_beginning_of_list

let tree_fwd' item (context, past, future) =
  match future with
  | [] -> 0, (context, Node (item, [])::past, [])
  | Node (main, _) as hd::future' when main = item ->
    1, (context, hd::past, future')
  | Node (main, variations)::future' ->
    let rec tree_fwd_var item left right =
      match right with
      | [] ->
        let context' =
          Var_line ((context, past, future'), main, left, item, right) in
        (context', [], [])
      | Var (var, line)::right' when item = var ->
        let context' =
          Var_line ((context, past, future'), main, left, var, right') in
        (context', [], line)
      | variation::right' -> tree_fwd_var item (variation::left) right' in
    1, (tree_fwd_var item [] variations)


let tree_init () = (Main_line, [], [])  (* outer context + past + future *)

(* fold_right' is like right fold, but carrying an additional
   accumulator c that is updated by g and applied together with f *)
let fold_right' f g =  (* c l *)
  let rec loop cont c = function
    | hd::tl ->
      let c' = g c in
      loop (fun acc' -> cont (f c hd::acc')) c' tl
    | [] -> cont []
  in loop id

(* fold_left' is like left fold, but carrying an additional
   accumulator c that is updated by g and applied together with f *)
let fold_left' f g c l acc =
  List.fold_left
    (fun (acc, c) item -> f c item::acc, g c)
    (acc, c) l


(* Generic map-fold catamorphism for move tree *)
let fold_zipper
    ~make_mainline ~make_line ~make_variation ~make_variations ~make_move
    ~acc ~fwd ~back ~up ~down ~next ~prev (context, past, future) =

  let rec fold_node acc (Node (move, variations)) =
    fold_variations acc variations
    |> make_move acc move
  and fold_variations acc variations =
    fold_right' fold_variation next (down acc) variations
    |> make_variations
  and fold_variation acc (Var (move, line)) =
    make_move acc move (make_variations [])::fold_future (fwd acc) line
    |> make_line
    |> make_variation
  and fold_future acc list = fold_right' fold_node fwd acc list in

  let fold_past acc list = fold_left' fold_node back acc list in

  let rec fold_tree_context acc (context, past, future) inner =
    let fold_acc, acc' =
      inner::fold_future (fwd acc) future
      |> fold_past (back acc) past in
    fold_line_context acc' context fold_acc

  and fold_line_context acc context inner =
    match context with
    | Main_line -> make_mainline acc inner
    | Var_line (context, main, left, var_move, right) ->
      let this_var = make_move acc var_move (make_variations [])::inner
                     |> id (* + acc make "not mainline" ? *)
                     |> make_line
                     |> make_variation in
      this_var::(fold_right' fold_variation next (next acc) right)
      |> fold_left' fold_variation prev (prev acc) left
      |> fst
      |> make_variations
      |> make_move (up acc) main
      |> fold_tree_context (up acc) context in

  let fold_acc, acc' = fold_future (fwd acc) future
                       |> fold_past acc past in
  fold_line_context acc' context fold_acc
  |> make_line
