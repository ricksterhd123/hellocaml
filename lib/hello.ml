(* Check if two lists are equal *)
let rec list_equal list_a list_b =
  match (list_a, list_b) with
    | ([], []) -> true
    | (_, []) -> false
    | ([], _) -> false
    | (b :: bs, c :: cs) -> (b = c) && (list_equal bs cs);;

let%test "#test.list_equal.1" = (list_equal [1] [1]);;
let%test "#test.list_equal.2" = (list_equal [1;2] [1;2]);;
let%test "#test.list_equal.3" = (list_equal [1;2;3] [1;2;3]);;
let%test "#test.list_equal.4" = (not (list_equal [1;2;3] []));;

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

let%test "#last.1" = ((last [1]) = Some 1);;
let%test "#last.2" = ((last []) = None);;

(* Get the last two elements in a list *)
let rec last_two x =
  match x with
    | [a; b] -> Some (a, b)
    | [] -> None
    | _ :: ys -> last_two ys

let%test "#last_two.1" = ((last_two [1;2;3;4;5]) = Some (4, 5));;
let%test "#last_two.2" = ((last_two [1]) = None);;
let%test "#last_two.3" = ((last_two []) = None);;

(* Get nth element in a list *)
let rec list_nth x n =
  match (x, n) with
    | ([], _) -> None
    | (y::_, 0) -> Some y
    | (_::ys, n) -> list_nth ys (n - 1)

let%test "#last_nth.1" = ((list_nth [1;2;3;4;5] 4) = Some 5);;

(* Get the length of a list *)
let length x =
  let rec length_iter x n = 
    match (x, n) with 
      | ([], n) -> n
      | (_ :: ys, _) -> length_iter ys (n + 1)
in length_iter x 0;;

let%test "#length.1" = ((length [1;2;3;4;5]) = 5);;
let%test "#length.2" = ((length []) = 0);;

(* Get head of a list *)
let head x =
  match x with
    | [] -> None
    | (x :: _) -> Some x

let%test "#head.1" = ((head [1;2;3;4]) = Some 1);;
let%test "#head.2" = ((head [3;4]) = Some 3);;
let%test "#head.3" = ((head []) = None);;

(* Get tail of a list *)
let tail x =
  match x with
    | [] -> None
    | (_ :: xs) -> Some xs

let%test "#tail.1" = (list_option_equal (tail [1;2;3;4]) (Some [2;3;4]));;
let%test "#tail.2" = (list_option_equal (tail [2;3;4]) (Some [3;4]));;
let%test "#tail.2" = ((tail []) = None);;

(* Append element onto end of list *)
let rec list_append t v =
  match (t, v) with
    | ([], v) -> [v]
    | (y :: ys, _) -> y :: list_append ys v

let%test "#list_append.1" = (list_equal (list_append [1; 2; 3] 4) [1; 2; 3; 4]);;
let%test "#list_append.2" = (list_equal (list_append [] 1) [1]);;

(* Merge two lists into one list *)
let merge_list x y =
  let rec merge_list_iter a b c =
    match (a, b, c) with
      | ([], [], c) -> c
      | (a :: aas, _, c) -> merge_list_iter aas b (list_append c a)
      | (_, b :: bs, c) -> merge_list_iter a bs (list_append c b)
  in merge_list_iter x y []

let%test "#merge_list.1" = (list_equal (merge_list [] []) []);;
let%test "#merge_list.2" = (list_equal (merge_list [] [1]) [1]);;
let%test "#merge_list.3" = (list_equal (merge_list [1] []) [1]);;
let%test "#merge_list.4" = (list_equal (merge_list [1] [2]) [1;2]);;
let%test "#merge_list.5" = (list_equal (merge_list [1] [2; 3]) [1;2;3]);;
let%test "#merge_list.6" = (list_equal (merge_list [1;2] [3; 4]) [1;2;3;4]);;

(* Reverse a list *)
let rec reverse x = 
  match x with
    | [] -> []
    | y :: ys -> merge_list (reverse (ys)) [y]

let%test "#reverse.1" = (list_equal (reverse [1;2;3]) ([3;2;1]));;
let%test "#reverse.2" = (list_equal (reverse [5;4;3;2;1]) ([1;2;3;4;5]));;

(* Is list palindrome? *)
let is_palindrome x = (list_equal x (reverse x))

let%test "#is_palindrome.1" = (is_palindrome [1;2;2;1]);;
let%test "#is_palindrome.2" = (not (is_palindrome [1;2]));;

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

let%test "#encode.1" = (list_equal (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;
let%test "#encode.2" = (list_equal (encode ["a";"a";"a"]) [(3, "a")]);;

(* Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_modified x =
  match x with
    | [] -> []
    | (first :: _) ->
      let encode_item count item = if count > 1 then Many (count, item) else One item in
      let rec encode_iter list last count rle =
        match (list, last, count, rle) with
          | ([], last, count, rle) -> list_append rle (encode_item count last)
          | (l :: ls, last, count, rle) -> 
              if l = last then
                encode_iter ls l (count + 1) rle
              else
                encode_iter ls l 1 (list_append rle (encode_item count last))
      in encode_iter x first 0 []

let%test "#encode_modified.1" = (list_equal (encode_modified ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]) [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);;
let%test "#encode_modified.2" = (list_equal (encode_modified ["a";"a";"a"]) [Many (3, "a")]);;

let rec duplicate x =
  match x with
    | [] -> []
    | (first :: xs) -> merge_list [first; first] (duplicate xs)

let%test "#duplicate.1" = (list_equal (duplicate []) [])
let%test "#duplicate.2" = (list_equal (duplicate ["a"]) ["a"; "a"])
let%test "#duplicate.3" = (list_equal (duplicate ["a"; "b"; "c"; "c"; "d"]) ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])

(* Helper function to test equality of (list 'a * list 'a) *)
let tuple_list_list_equal (ax, ay) (bx, by) = (list_equal ax bx) && (list_equal ay by)

let%test "#tuple_list_list_equal.1" = tuple_list_list_equal ([], []) ([], [])
let%test "#tuple_list_list_equal.2" = tuple_list_list_equal (["a"; "a"], ["b"; "b"]) (["a"; "a"], ["b"; "b"])

(* split list into two parts where the first part is at most the length of n *)
let weird_split x n =
  if n > 0 then
    let rec split x first last =
      match x with
        | [] -> (first, last)
        | (x :: xs) -> 
          if (length first) < n then
            split xs (list_append first x) last
          else
            split xs first (list_append last x) in
    split x [] []
  else
    ([], [])

let%test "#weird_split.1" = tuple_list_list_equal (weird_split [] 0) ([], [])
let%test "#weird_split.2" = tuple_list_list_equal (weird_split [] 1) ([], [])
let%test "#weird_split.3" = tuple_list_list_equal (weird_split ["a"; "b"] 1) (["a"], ["b"])
let%test "#weird_split.4" = tuple_list_list_equal (weird_split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3) (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
