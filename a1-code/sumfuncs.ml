


let array_sum arr =
  let len = Array.length arr in
  let sum : int ref = ref 0 in
  for i = 0 to (len-1) do
    sum := !sum + Array.get arr i
  done;
  !sum
  ;;


(* val array_sum : int array -> int
   Return the sum of int array arr. Uses Array.length to calculate its
   length. Uses a ref to a summing int and a loop over the array
   elements.

   REPL EXAMPLES:
   # array_sum [|1; 3; 5|];;
   - : int = 9
   # array_sum [|4; -3; 12; 2|];;
   - : int = 15
   # array_sum [||];;
   - : int = 0
*)
let rec list_sum lst =
  match lst with
    | [] -> 0
    | head::[] -> List.hd lst
    | head::tail -> (List.hd lst) + (list_sum (List.tl lst))
;;


(* val list_sum : int list -> int
   Return the sum of int list lst. Uses recursion and NO mutation.
   Uses List.hd and List.tl to get the head and tail of a list.

   let len = Array.length arr in
   let sum : int ref = ref 0 in
   let i = 0 in
   let rec loop i =
     if i < (len-1) then (Array.get arr i) + loop(i+1)
     else Array.get arr i in
   loop 0
   ;;


   REPL EXAMPLES:
   # list_sum [1; 3; 5];;
   - : int = 9
   # list_sum [4; -3; 12; 2];;
   - : int = 15
   # list_sum [];;
   - : int = 0
*)
