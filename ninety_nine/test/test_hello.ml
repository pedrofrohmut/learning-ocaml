open OUnit2

let test1 _ = assert_equal "Hello, Bob!" (Ninety_nine.Hello.hi "Bob")
let test2 _ = assert_equal "Hello, Susan!" (Ninety_nine.Hello.hi "Susan")

let test3 _ = assert_equal "Hello, John!" (Ninety_nine.Hello.hi "John")

let () = run_test_tt_main (
  "Hello tests suite" >:::
    [ "test 1" >:: test1
    ; "test 2" >:: test2
    ]
)

let () = run_test_tt_main (
  "Hello tests suite 2" >:::
    [ "test 3" >:: test3
    ]
)
