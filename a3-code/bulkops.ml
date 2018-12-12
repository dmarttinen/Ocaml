(* bulkops.ml: Implement bulk operations on Doccol's of string list
   Documents that are useful for multimanager.  Since the functions in
   this module require access to fields and types of other modules, start
   the file by opening those two modules:
*)
open Document;;
open Doccol;;

let showall doccol =
  let helper (name,doc) =                 (*helper function to do stuff*)
    Printf.printf "--List %s--\n" name;   (*print out the list header*)
    Sortedlist.print doc.current;         (*print out the list*)
    in
  List.iter helper doccol.docs            (*use List.iter to call helper on each doc in doccol*)
;;
(* val showall : string list Doccol.doccol -> unit
   Prints all documents in doccol to the screen. For each list,
   prints the list name first and then each element of the list using
   Sortedlist functions. Uses higher-order functions to iterate over
   the doclist.

   EXAMPLE:
   --List test-data/heros.txt--
   Asami
   Bolin
   Bumi
   Jinora
   Korra
   Kya
   Mako
   Tenzin

   --List test-data/villains.txt--
   Amon
   Hiroshi
   Kuvira
   Ming-Hua
   P-li
   Unalaq
   Zaheer

   --List default.txt--
   Korra
   Meelo
   Pema
*)

let saveall doccol =
  let helper (name,doc) =                   (*helper function to do stuff*)
    Util.strlist_to_file doc.current name;  (*Util.strlist_to_file takes current list in doc and the name of doc and makes a file*)
    in
  List.iter helper doccol.docs              (*use List.iter to call helper on each doc in doccol*)
;;
(* val saveall : string list Doccol.doccol -> unit
   Saves all documents in doccol. Makes use of Util functions to do
   I/O. Makes use of higher-order functions to write each list to
   associated file name. *)

let addall doccol elem =
  let helper (name,doc) =                                   (*helper function to do stuff*)
    Document.set doc (Sortedlist.insert doc.current elem)   (*Document.set will add the new list with the element instered into the doc*)
    in
  List.iter helper doccol.docs                              (*use List.iter to call helper on each doc in doccol*)
;;
(* val addall : 'a list Doccol.doccol -> 'a -> unit
   Adds the given element to all docs in doccol. Makes use of
   higher-order functions and Sortedlist functions to modify each
   list. Each doc/list can individually undo the addition. *)

let mergeall doccol =
  let helper (name,doc) =                                                 (*helper function to do stuff*)
    doc.current                                                           (*grab the current list out of the doc*)
    in
  let doclist = List.map helper doccol.docs in                            (*create a new list that has all of the lists from each doc using List. map to iterate through them all*)
  List.fold_left Sortedlist.merge (List.hd doclist) (List.tl doclist)     (*merge the first list from doclist with the rest of doclist using fold_left to iterate through them all and combine them into one list*)
;;
(* val mergeall : 'a list Doccol.doccol -> 'a list
   Merges all lists in doccol.docs into a single list and returns
   it. Uses higher-order functions and Sortedlist functions to perform
   the merge. *)
