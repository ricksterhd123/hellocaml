(* Check if two lists are equal *)
let rec list_equal list_a list_b =
  match (list_a, list_b) with
    | ([], []) -> true
    | (_, []) -> false
    | ([], _) -> false
    | (b :: bs, c :: cs) -> (b = c) && (list_equal bs cs);;

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

(* Get tail of a list *)
let tail x = 
  match x with
    | (_ :: xs) -> xs
    | [] -> []

(* Append element onto end of list *)
let rec list_append t v =
  match (t, v) with
    | ([], v) -> [v]
    | (y :: ys, _) -> y :: list_append ys v

(* Merge two lists into one list *)
let merge_list x y =
  let rec merge_list_iter a b c =
    match (a, b, c) with
      | ([], [], c) -> c
      | (a :: aas, _, c) -> merge_list_iter aas b (list_append c a)
      | (_, b :: bs, c) -> merge_list_iter a bs (list_append c b)
  in merge_list_iter x y []

(* Reverse a list *)
let rec reverse x = 
  match x with
    | [] -> []
    | y :: ys -> merge_list (reverse (ys)) [y]
