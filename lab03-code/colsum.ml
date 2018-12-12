let rec colsum_nt n =
  Printf.printf "%d\n" n;
  if n = 1 then
    1
  else
    let next =
      if n mod 2 = 0
      then n/2
      else 3*n+1
    in
    let rest = colsum_nt next in
    n + rest
;;

let sum = colsum_nt 15 in
Printf.printf "sum: %d\n" sum;
;;


let rec colsum_tr n =
  let rec helper num total = (*start the helepr funtion. This keeps a running total, as well as the input number*)
    Printf.printf "%d\n" num;
    if num = 1 then          (*base case. *)
      total                  (*This is different that colsum_nt because the num is added before getting here*)
    else                     (*the else clause is the same. It just has a new vairable*)
      let next =
        if num mod 2 = 0
        then num/2
        else 3*num+1
      in
      helper next (next+total) (*call to the helper function. This does the recursion, and the addition in one step so that the function doesn't have to go back up the stack*)
  in
  helper n n (*call helper. The total starts with n becuase the next value is computed befoere the first addition can be performed*)
;;

let sum = colsum_tr 15 in
Printf.printf "sum: %d\n" sum;
;;
