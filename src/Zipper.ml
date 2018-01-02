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

let init () = [], []


(* TREE ZIPPER *)
(* https://pavpanchekha.com/blog/zippers/derivative.html *)

type 'a node = Node of 'a * 'a variation list
and 'a variation = Var of 'a * 'a line
and 'a line = 'a node list

type 'a tree_context = 'a line_context  (* outer context *)
                       * 'a line        (* past *)

and 'a line_context =
  | Main_line
  | Var_line of 'a tree_context       (* outer context with past *)
                * 'a                  (* main line move *)
                * 'a variation list   (* variations before *)
                * 'a                  (* move starting this variation *)
                * 'a variation list   (* variations after *)
                * 'a line             (* future *)

type 'a tree_zipper = 'a tree_context * 'a line

type action = Fwd | Back | Down | Next | Prev | Up

exception No_variations
exception No_next_variation
exception No_prev_variation
exception Not_in_variation
exception Not_beginning_of_list

let tree_fwd ((context, past), future) =
  match future with
  | [] -> raise End_of_list
  | Node(item, _) as hd::future' -> item, ((context, hd::past), future')

let tree_back ((context, past), future) =
  match context, past with
  | context, hd::past' -> (context, past'), hd::future
  | _, [] -> raise Beginning_of_list

(* rewinds one step and jumps into the first variation *)
let tree_down ((context, past), future) =
  match past with
  | [] -> raise Beginning_of_list
  | node::past' ->
    match node with
    | Node (_, []) -> raise No_variations
    | Node (main, Var (var, line)::variations) ->
      let context' =
        Var_line ((context, past'), main, [], var, variations, future) in
      var, ((context', []), line)

let tree_next ((context, past), future) =
  match past, context with
  | [], Main_line -> raise No_next_variation
  | [], Var_line (_, _, _, _, [], _) -> raise No_next_variation
  | [], Var_line (context', main, left, var, Var (next, line)::right, future') ->
    let var' = Var_line
        (context', main, Var (var, future)::left, next, right, future') in
    next, (var', []), line
  | _ -> raise Not_beginning_of_list

let tree_prev ((context, past), future) =
  match past, context with
  | [], Main_line -> raise No_prev_variation
  | [], Var_line (_, _, [], _, _, _) -> raise No_prev_variation
  | [], Var_line (context', main, Var (prev, line)::left, var, right, future') ->
    let var' = Var_line
        (context', main, left, prev, Var (var, future)::right, future') in
    prev, (var', []), line
  | _ -> raise Not_beginning_of_list

let tree_up ((context, past), future) =
  match past, context with
  | [], Main_line -> raise Not_in_variation
  | [], Var_line ((context', past'), main, left, var, right, future') ->
    let variations = List.rev_append left (Var (var, future)::right) in
    let main_node = Node (main, variations) in
    main, ((context', main_node::past'), future')
  | _ -> raise Not_beginning_of_list

let tree_fwd' item ((context, past), future) =
  match future with
  | [] -> 0, ((context, (Node (item, [])::past)), [])
  | Node (main, _) as hd::future' when main = item ->
    1, ((context, hd::past), future')
  | Node (main, variations)::future' ->

    let rec tree_fwd_var item left right =
      match right with
      | [] ->
        let inner_context = (context, past) in
        let context' =
          Var_line (inner_context, main, left, item, right, future') in
        (context', []), []
      | Var (var, line)::right' when item = var ->
        let inner_context = (context, past) in
        let context' =
          Var_line (inner_context, main, left, var, right', future') in
        (context', []), line
      | variation::right' -> tree_fwd_var item (variation::left) right' in

    1, (tree_fwd_var item [] variations)


let tree_init () = (Main_line, []), []  (* outer context + past + future *)
