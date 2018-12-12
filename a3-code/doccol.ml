open Document;;

(* doccol.ml: Type and functions for a collection of named documents.
   Tracks a current document and its name along with an association
   list of all docs in the collection.  Preserves uniqueness of names
   in the collection. Makes use of built-in List functions to
   ad/remove/get docs from the association list. *)

(* Type to track a collection of named documents in an association
   list. *)
type 'a doccol = {
  mutable count   : int;                                  (* count of docs in list *)
  mutable curdoc  : 'a Document.document;                 (* current list being edited *)
  mutable curname : string;                               (* name of current list *)
  mutable docs    : (string * 'a Document.document) list; (* association list of names/docs *)
};;

let make name doc =
  let col = {count=1; curdoc=doc; curname=name; docs=[(name,doc)]}
  in col
;;
(* val make : string -> 'a Document.document -> 'a doccol
   Create a doccol. The parameters name and doc become the current
   doc and the only pair in the docs association list. *)

let add doccol name doc =
  let exists = List.assoc_opt name doccol.docs in (*use assoc_opt to check through the entire list of docs*)
  match exists with
    | (Some _)-> false (*if there's already something that matches, then false*)
    | None ->           (*if there's nothing there, then add it and update doccol*)
      begin
        doccol.count <- doccol.count + 1;
        doccol.docs <- (name,doc)::doccol.docs;
        true          (*and return true*)
      end
;;
(* val add : 'a doccol -> string -> 'a Document.document -> bool
   If there is already a doc with name in doccol, do nothing and
   return false.  Otherwise, add the given doc to doccol with the
   given name, update the count of docs and return true. Uses
   association list functions from the List module. *)

let remove doccol name =
  let exists = List.assoc_opt name doccol.docs in   (*use assoc_opt to check through the entire list of docs*)
  match exists with
    | None -> false       (*if there's nothing there, then return false*)
    | (Some _) ->         (*if there's something there, then remove it and update doccol*)
      begin
        if name = doccol.curname then false
        else
        begin
          doccol.count <- doccol.count-1;
          doccol.docs <- List.remove_assoc name doccol.docs; (*use remove_assoc to pull the tuple out of the list*)
          true              (*and return true*)
        end
      end
;;
(* val remove : 'a doccol -> string -> bool
   If name is equal to curname for the doccol, do nothing and return
   false.  If there is no doc with name in doccol, do nothing and
   return false.  Otherwise, remove the named doc from doccol,
   decrement the count of docs, and return true. Uses association list
   functions from the List module. *)

let has doccol name =
  List.mem_assoc name doccol.docs   (*use mem_assoc to check through the entire list and return true or false if there's a match or not*)
;;
(* val has : 'a doccol -> string -> bool
   Returns true if the named doc is in the doccol and false otherwise. *)

let switch doccol name =
  let exists = List.assoc_opt name doccol.docs in   (*use assoc_opt to check through the entire list of docs*)
  match exists with
  | None -> false             (*if there's nothing there, then return false*)
  | (Some doc) -> begin       (*if there's something there, then replace the values in doccol with the new ones*)
    doccol.curdoc <- doc;
    doccol.curname <- name;
    true                      (*and return true*)
  end
;;
(* val switch : 'a doccol -> string -> bool
   Change the current document/name to the named document and return
   true. If the named document does not exist, return false and make
   no changes to doccol. *)

let string_of_doccol doccol =
  let strlist = List.map (fst) doccol.docs in                   (*grab all of the names out of the list of docs*)
  let countstr = Printf.sprintf "%i docs\n" doccol.count in     (*create a string that shows the number of docs*)
  let str = countstr ^ "- " ^ String.concat "\n- " strlist in   (*print the number of docs, then use String.concat to stick a newlint and a hyphen between each string in the strlist*)
  str
;;
(* val string_of_col : 'a doccol -> string
   Creates a string representation of doccol showing the count of
   docs and the names of all docs. Each doc is listed on its own
   line. It has the following format:

   4 docs
   - test-dir/heros.txt
   - places.txt
   - stuff.txt
   - default.txt

   Does not define any helper functions. Makes use of higher order
   functions such as List.map and/or List.fold. May also use string
   processing functions such asString.concat and/or Printf.sprintf *)
