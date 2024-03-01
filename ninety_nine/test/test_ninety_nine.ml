let main_tests =
  "Main tests here" >:::
  [ "Test 1" >:: (fun _ -> assert_equal "foo" "bar")
  ]
