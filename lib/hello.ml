(* Check if two lists are equal *)
let rec list_equal list_a list_b =
  match (list_a, list_b) with
    | ([], []) -> true
    | (_, []) -> false
    | ([], _) -> false
    | (b :: bs, c :: cs) -> (b = c) && (list_equal bs cs);;

(* Check if two 'a option list are equal *)
let list_option_equal list_opt_a list_opt_b =
  match (list_opt_a, list_opt_b) with
    | (None, None) -> true
    | (_, None) -> false
    | (None, _) -> false
    | (Some b, Some c) -> list_equal b c;;

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

(* Get head of a list *)
let head x =
  match x with
    | [] -> None
    | (x :: _) -> Some x

(* Get tail of a list *)
let tail x =
  match x with
    | [] -> None
    | (_ :: xs) -> Some xs

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

(* Is list palindrome? *)
let is_palindrome x = (list_equal x (reverse x))

(* Run-Length Encoding *)
let encode x =
  match x with
    | [] -> []
    | (first :: _) ->
      let rec encode_iter list last count rle =
        match (list, last, count, rle) with
          | ([], last, count, rle) -> list_append rle (count, last)
          | (l :: ls, last, count, rle) -> 
              if l = last then
                encode_iter ls l (count + 1) rle
              else
                encode_iter ls l 1 (list_append rle (count, last))
      in encode_iter x first 0 []
