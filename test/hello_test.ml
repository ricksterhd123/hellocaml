exception Error of string

let expect value = if value then () else raise (Error "Test Failed");;

expect ((Hello.last [1]) = Some 1);;
expect ((Hello.last []) = None);;
expect ((Hello.last_two [1;2;3;4;5]) = Some (4, 5));;
expect ((Hello.last_two [1]) = None);;
expect ((Hello.last_two []) = None);;
expect ((Hello.list_nth [1;2;3;4;5] 4) = Some 5);;
