let test_fix_string =
  let test ~name input ~expected =
    (name, `Quick, fun () ->
       let got = UTop.Private.fix_string input in
       Alcotest.check Alcotest.string __LOC__ expected got
    )
  in
  ( "fix_string"
  , [ test ~name:"small" "x" ~expected:"x"
    ; test ~name:"empty" "" ~expected:""
    ]
  )

let () = Alcotest.run "utop" [test_fix_string]
