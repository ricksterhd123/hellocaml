exception Error of string

let test test_name success =
  if success then 
    print_endline ("SUCCESS: " ^ test_name) 
else 
  raise (Error ("FAILED: " ^ test_name));;

(* list_equal *)
test "#test.list_equal.1" (Hello.list_equal [1] [1]);;
test "#test.list_equal.2" (Hello.list_equal [1;2] [1;2]);;
test "#test.list_equal.3" (Hello.list_equal [1;2;3] [1;2;3]);;
test "#test.list_equal.4" (not (Hello.list_equal [1;2;3] []));;

(* last *)
test "#last.1" ((Hello.last [1]) = Some 1);;
test "#last.2" ((Hello.last []) = None);;

(* last_two *)
test "#last_two.1" ((Hello.last_two [1;2;3;4;5]) = Some (4, 5));;
test "#last_two.2" ((Hello.last_two [1]) = None);;
test "#last_two.3" ((Hello.last_two []) = None);;

(* last_nth *)
test "#last_nth.1" ((Hello.list_nth [1;2;3;4;5] 4) = Some 5);;

(* length *)
test "#length.1" ((Hello.length [1;2;3;4;5]) = Some 5);;
test "#length.2" ((Hello.length []) = Some 0);;

(* tail *)
test "#tail.1" (Hello.list_equal (Hello.tail [1;2;3;4]) [2;3;4]);;
test "#tail.2" (Hello.list_equal (Hello.tail [2;3;4]) [3;4]);;

(* list_append *)
test "#list_append.1" (Hello.list_equal (Hello.list_append [1; 2; 3] 4) [1; 2; 3; 4]);;
test "#list_append.2" (Hello.list_equal (Hello.list_append [] 1) [1]);;

(* merge_list *)
test "#merge_list.1" (Hello.list_equal (Hello.merge_list [] []) []);;
test "#merge_list.2" (Hello.list_equal (Hello.merge_list [] [1]) [1]);;
test "#merge_list.3" (Hello.list_equal (Hello.merge_list [1] []) [1]);;
test "#merge_list.4" (Hello.list_equal (Hello.merge_list [1] [2]) [1;2]);;
test "#merge_list.5" (Hello.list_equal (Hello.merge_list [1] [2; 3]) [1;2;3]);;
test "#merge_list.6" (Hello.list_equal (Hello.merge_list [1;2] [3; 4]) [1;2;3;4]);;

(* reverse *)
test "#reverse.1" (Hello.list_equal (Hello.reverse [1;2;3]) ([3;2;1]));;
test "#reverse.2" (Hello.list_equal (Hello.reverse [5;4;3;2;1]) ([1;2;3;4;5]));;
