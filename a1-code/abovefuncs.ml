let array_above thresh arr =
  let len = Array.length arr in
  let count :  int ref = ref 0 in
  for i = 0 to (len-1) do
    if thresh < (Array.get arr i)
    then count := !count + 1
  done;
  let newarr = Array.make !count thresh in
  let place : int ref = ref 0 in
  for i = 0 to (len -1) do
    if (Array.get arr i) > thresh
    then
    begin
      Array.set newarr !place (Array.get arr i);
      place := !place + 1;
    end
  done;
  newarr
;;
(* val array_above : 'a -> 'a array -> 'a array

   Creates a new array which has only elements which are greater than
   parameter thresh in it. Elements from arr that are larger than
   thresh appear in the same order the return array as they do in arr.
   Uses two passes to count the elements above in arr, allocates
   another array of appropriate size, and then copies in elements from
   arr.  Does not modify the original array arr.

   REPL EXAMPLES:
   # array_above 0 [|0; 1; 2; 0|];;
   - : int array = [|1; 2|]
   # array_above 0 [|4; -2; -1; 7; 0; 3|];;
   - : int array = [|4; 7; 3|]
   # array_above 3 [|4; -2; -1; 7; 0; 3|];;
   - : int array = [|4; 7|]
   # array_above 1.5 [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|];;
   - : float array = [|4.2; 7.6; 8.9; 8.5|]
   # array_above 0.0 [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|];;
   - : float array = [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|]
   # array_above 9.0 [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|];;
   - : float array = [||]
   # array_above false [|false; true; false; true; true;|];;
   - : bool array = [|true; true; true|]
*)

let rec list_above thresh lst =
  match lst with
    | [] -> []
    | head::[] -> if (List.hd lst) > thresh
      then List.hd lst::[]
      else []
    | head::tail -> if (List.hd lst) > thresh
      then (List.hd lst)::(list_above thresh (List.tl lst))
      else list_above thresh (List.tl lst)

;;
(* val list_above : 'a -> 'a list -> 'a list

   Create a list which has only elements from lst that are larger than
   thresh.  Uses recursion to accomplish this in a single pass over
   the original list. Does not modify the original list lst.

   REPL EXAMPLES:
   # list_above 0 [0; 1; 2];;
   - : int list = [1; 2]
   # list_above 0 [0; 1; 2; 0];;
   - : int list = [1; 2]
   # list_above 0 [4; -2; -1; 7; 0; 3];;
   - : int list = [4; 7; 3]
   # list_above 3 [4; -2; -1; 7; 0; 3];;
   - : int list = [4; 7]
   # list_above 1.5 [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5];;
   - : float list = [4.2; 7.6; 8.9; 8.5]
   # list_above 0.0 [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5];;
   - : float list = [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5]
   # list_above 9.0 [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5];;
   - : float list = []
   # list_above false [false; true; false; true; true;];;
   - : bool list = [true; true; true]
*)
