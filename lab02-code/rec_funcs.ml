let rec last_elem list = (*recursive function that takes in a list of any type*)
  if list = [] then (*check if the input list is empty. If it is then throw a message*)
    raise (Failure "No last element in an empty list")
  else
    let elem = List.hd list in (*grab the first element in the list*)
    let rest = List.tl list in (*grab the rest of the list*)
    if rest = [] then (*check if the rest of the list is empty, which means there's only one element in the list*)
      elem (*if there's only one element, this must be the last so return it*)
    else
      last_elem rest (*if there are other elements in the list, call last_elem on whatever is left*)
;;

let elems_outside start stop list = (*function that takes in 2 ints and a list of type 'a*)
  let rec helper pos lst = (*beginning of recursive functino that takes in an int and a list of type 'a. The int is going to be a position in the list, and the list is the given list from elems_outside*)
    if lst=[] then (*check if the list is empty, if so, then return the empty list*)
      []
    else if start<=pos && pos<=stop then (*check if the current position is between the two input values from elems_outside*)
      helper (pos+1) (List.tl lst) (*if it is, call the helper function again, this time with the position value incremented by 1*)
    else(*if the position value is outisde of the given ints then*)
      let elem = List.hd lst in (*grab the first element in the list*)
      let rest = List.tl lst in (*grab the rest of the list*)
      let result =  helper (pos+1) rest in (*call the helper function with the rest of the list, and the position value incremented by 1*)
      elem :: result (*and build a new list with the head of the input list and whatever the helper function returns*)
  in
  helper 0 list (*call the helper function with a starting position of 0, and the entire input list*)
;;

(* REPL EXAMPLES
   # elems_outside 3 5 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 1; 2; 6; 7]
   # elems_outside 1 5 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 6; 7]
   # elems_outside 2 4 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 1; 5; 6; 7]
   # elems_outside 2 4 [];;
   - : 'a list = []
*)
