exception Nag_error of string

type t = int

let pgn_of_nag = fun nag ->
  Printf.sprintf "$%i" nag

let nag_of_string = function
  | "!" -> 1
  | "?" -> 2
  | "!!" -> 3
  | "??" -> 4
  | "!?" -> 5
  | "?!" -> 6
  | a -> raise (Nag_error a)

let string_of_nag = function
  | 0 -> ""
  | 1 -> "!"
  | 2 -> "?"
  | 3 -> "!!" (* alternative: {js|\u203c|js} *)
  | 4 -> "??" (* alternative: {js|\u2047|js} *)
  | 5 -> "!?" (* alternative: {js|\u2049|js} *)
  | 6 -> "?!" (* alternative: {js|\u2048|js} *)
  | 7 -> {js|\u25a1|js}  (* forced move *)
  (* 8, 9 *)
  | 10 -> "=" (* drawish position or even *)
  (* 11, 12 *)
  | 13 -> {js|\u221e|js} (* unclear position *)
  | 14 -> {js|\u2a72|js} (* White has a slight advantage *)
  | 15 -> {js|\u2a71|js} (* Black has a slight advantage *)
  | 16 -> {js|\u00b1|js} (* White has a moderate advantage *)
  | 17 -> {js|\u2213|js} (* Black has a moderate advantage *)
  | 18 -> "+-" (* White has a decisive advantage *)
  | 19 -> "-+" (* Black has a decisive advantage *)
  (* 20, 21 *)
  | 22 | 23 -> {js|\u2a00|js} (* zugzwang *)
  (* 24-31 *)
  | 32 | 33 -> {js|\u27f3|js} (* development advantage *)
  (* 34, 35 *)
  | 36 | 37 -> {js|\u2192|js} (* initiative *)
  (* 38, 39 *)
  | 40 | 41 -> {js|\u2191|js} (* attack *)
  (* 42-131 *)
  | 132 | 133 -> {js|\u21c6|js} (* counterplay *)
  (* 134-137 *)
  | 138 | 139 -> {js|\u2a01|js} (* zeitnot *)
  | _ -> ""

