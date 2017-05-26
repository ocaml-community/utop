type t = A of int | B of string

let some_value = [A 42; B "Hello, world"]

let () =
  print_endline "Starting utop now!";
  UTop_main.interact ()
    ~unit:__MODULE__
    ~loc:__POS__
    ~values:[V ("some_value", some_value)]
;;
