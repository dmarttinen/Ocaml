let array_rev arr =
  let len = Array.length arr in
  let place : int ref = ref (len-1) in
  let counter : int ref = ref (len/2) in
  if len != 0 then
    begin
      if (len mod 2) = 0 then
        counter := !counter - 1;
      for i = 0 to !counter do
        let temp = Array.get arr i in
          Array.set arr i (Array.get arr !place);
          Array.set arr !place temp;
          place := !place - 1;
        done;
    end
  ;;

(* val array_rev : 'a array -> unit

   Reverses the given array in place. Uses iteration and mutation to
   do so efficiently. DOES NOT generate any internal copies of the
   parameter array.

   REPL EXAMPLES:
   # let a1 = [|1; 2; 3;|];;
   val a1 : int array = [|1; 2; 3|]
   # array_rev a1;;
   - : unit = ()
   # a1;;
   - : int array = [|3; 2; 1|]
   # let a2 = [|"a"; "b"; "c"; "d"; "e"; "f"|];;
   val a2 : string array = [|"a"; "b"; "c"; "d"; "e"; "f"|]
   # array_rev a2;;
   - : unit = ()
   # a2;;
   - : string array = [|"f"; "e"; "d"; "c"; "b"; "a"|]
   # let a3 = [|true; true; false; false; true;|];;
   val a3 : bool array = [|true; true; false; false; true|]
   # array_rev a3;;
   - : unit = ()
   # a3;;
   - : bool array = [|true; false; false; true; true|]
*)

let list_rev lst =
  let rec help oldlist revlist =
    match oldlist with
      | [] -> revlist
      | head::[] -> help [] (List.hd oldlist::revlist)
      | head::tail -> help (List.tl oldlist) (List.hd oldlist::revlist)
  in
  help lst []
;;


(* val list_rev : 'a list -> 'a list

   Return a reversed copy of the given list. Does not (and cannot)
   modify the original list. Uses an internal recursive function to
   build the reversed list. The internal function is tail-recursive.

   REPL EXAMPLES:
   # list_rev lst1;;
   - : int list = [3; 2; 1]
   # lst1;;
   - : int list = [1; 2; 3]
   # let lst2 = ["a"; "b"; "c"; "d"; "e"; "f"];;
   val lst2 : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   # list_rev lst2;;
   - : string list = ["f"; "e"; "d"; "c"; "b"; "a"]
   # lst2;;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   # let lst3 = [true; true; false; false; true];;
   val lst3 : bool list = [true; true; false; false; true]
   # list_rev lst3;;
   - : bool list = [true; false; false; true; true]
   # lst3;;
   - : bool list = [true; true; false; false; true]
*)
