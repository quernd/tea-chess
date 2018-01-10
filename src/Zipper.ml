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
