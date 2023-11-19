(* Get the last element in a list *)
let rec last x =
  match x with
  | [] -> None
  | [y] -> Some y
  | _ :: ys -> last ys

(* Get the last two elements in a list *)
let rec last_two x =
  match x with
    | [a; b] -> Some (a, b)
    | [] -> None
    | _ :: ys -> last_two ys

(* Get nth element in a list *)
let rec list_nth x n =
  match (x, n) with
    | ([], _) -> None
    | (y::_, 0) -> Some y
    | (_::ys, n) -> list_nth ys (n - 1)

(* Get the length of a list *)
let length x =
  let rec length_iter x n = 
    match (x, n) with 
      | ([], n) -> Some n
      | (_ :: ys, _) -> length_iter ys (n + 1)
in length_iter x 0;;
