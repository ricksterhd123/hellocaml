exception Error of string

let test test_name success =
  if success then 
    print_endline ("SUCCESS: " ^ test_name) 
else 
  raise (Error ("FAILED: " ^ test_name));;

test "#last.1" ((Hello.last [1]) = Some 1);;
test "#last.2" ((Hello.last []) = None);;
test "#last_two.1" ((Hello.last_two [1;2;3;4;5]) = Some (4, 5));;
test "#last_two.2" ((Hello.last_two [1]) = None);;
test "#last_two.3" ((Hello.last_two []) = None);;
test "#last_nth.1" ((Hello.list_nth [1;2;3;4;5] 4) = Some 5);;
test "#length.1" ((Hello.length [1;2;3;4;5]) = Some 5);;
test "#length.2" ((Hello.length []) = Some 0);;
