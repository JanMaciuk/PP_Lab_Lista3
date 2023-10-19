print_endline "Hello World"
let lista = [1;2;3;2]
let rec getLAstElement list = match list with
  | [] -> None
  | [x] -> Some(x)
  | _ :: tail -> getLAstElement tail

  print_endline string_of_int getLAstElement lista
