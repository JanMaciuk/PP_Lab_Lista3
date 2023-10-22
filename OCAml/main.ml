print_endline "Hello World"
  let extractFromOption x = match x with
  | Some i -> string_of_int i
  | None -> "None";;
  (*Wydobywam wartość z opcji*)

  let optionToList optionValue = match optionValue with
  | None -> []  
  | Some lst -> lst 
  (*Wydobywam listę z opcji*)

  let rec print_int_list lst = match lst with
  | [] -> ()  (* Pusta lista, pomijam *)
  | head :: tail ->
    let _ = Printf.printf "%d " head in print_int_list tail


(*Zadanie 1*)
  let rec getLastElement list = match list with
  | [] -> None
  | [x] -> Some(x)
  | _ :: tail -> getLastElement tail
  (*przechodzę dalej aż do ostatniego elementu*)

  let _ = print_endline (extractFromOption (getLastElement [1;2;3;4;5]))


(*Zadanie 2*)
  let rec getTwoLastElements lista previousElement = match lista with
  | [] -> None  (* Pusta lista *)
  | [lastElement] -> (match previousElement with
      | Some prevElem -> Some [prevElem; lastElement]
      | None -> None) (*Lista jednoelementowa*)
  | head :: tail -> getTwoLastElements tail (Some head)
  (*Analogiczne do pierwszej, ale przenoszę przedostatni element do następnego wywołania*)

  let _ = print_int_list (optionToList (getTwoLastElements [1;2;3;4;5] None))
  let _ = print_endline ""


(*Zadanie 3*)
  let rec listLength list : int = match list with
  | [] -> 0
  | head::tail -> 1 + (listLength tail)

  let _ = print_endline (string_of_int (listLength [1;2;3;4;5]))


(*Zadanie 4*)
  let rec listReverse list = match list with
    | [] -> []
    | head :: tail -> (listReverse tail) @ [head]
  (*Przechodzę całą listę wyciągając pierwszy element na koniec*)

  let _ = print_int_list (listReverse [1;2;3;4;5])
  let _ = print_endline ""


(*Zadanie 5*)
  let palindrome list = list = (listReverse list)
    (*jeżeli lista jest identyczna z jej odwrotnością - jest palindromem*)

  let _ = print_endline (string_of_bool (palindrome [1;2;3;4;5]))


(*Zadanie 6*)
let rec listContains (list) (element) :bool= match list with
  | [] -> false
  | head :: tail -> if head = element then true 
  else listContains tail element
(*Przechodzę całą listę element po elemencie, sprawdzając czy jest równy szukanemu*)
(*To tylko funkcja pomocnicza żeby nie używać gotowych metod*)

let rec uniqueElementsOnly list = match list with
  | [] -> []
  | head :: tail -> if (listContains tail head) then uniqueElementsOnly tail else head :: (uniqueElementsOnly tail)
(*jeżeli element występuje w ogonie to pomijam go, jeżeli nie to biorę go do listy wynikowej*)

let _ = print_int_list (uniqueElementsOnly [1;2;3;4;5;1;2;3;4;5])
let _ = print_endline ""

(*Zadanie 7*)
let rec evenIndexesOnly list (evenIndex:bool) = match list with
  | [] -> []
  | head :: tail -> if evenIndex then head :: (evenIndexesOnly tail false) else evenIndexesOnly tail true
(*jeżeli jestem na parzystym indeksie to biorę element, jeżeli nie to pomijam*)

let _ = print_int_list (evenIndexesOnly [1;2;3;4;5] true)
let _ = print_endline ""

(*Zadanie 8*)
let rec checkPrime((n:int),(i:int)):bool = 
  if (n<=1) then false
  else if (n<=3) then true
  else if (n mod i = 0) then false
  else if (i*i > n) then true
  else checkPrime(n,i+1)

let _ = print_endline (string_of_bool (checkPrime(17,2)))