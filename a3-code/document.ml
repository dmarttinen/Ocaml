(* document.ml: Defines a "document" type which tracks a current state
   along with undo/redo history.  The document type is polymorphic
   meaning data can be any type such as 'int document' or 'string list
   document'. *)

(* document type declaration. *)
type 'a document = {
    mutable current    : 'a ;       (* current document state *)
    mutable undo_stack : 'a list;   (* past states that can be reached via undo *)
    mutable redo_stack : 'a list;   (* undone states that can be reached via redo *)
};;

let make initial =
  let doc = {current = initial; undo_stack = []; redo_stack=[]}
  in doc
;;
(* val make : 'a -> 'a document
   Create a new document with initial as the current state and empty
   undo/redo stacks. *)

let set document data =
  document.undo_stack <- document.current::document.undo_stack;
  document.current <- data;
  document.redo_stack <- [];
;;
(* val set : 'a document -> 'a -> unit
   Set the document to the given data. Push current state into the
   undo stack. Empty the redo stack *)

let undo document =
  if document.undo_stack = [] (*check if the undo stack is empty*)
  then                (*if so return false*)
    false
  else                (*otherwise*)
    begin
      document.redo_stack <- document.current::document.redo_stack; (*move current list onto the redo stack*)
      document.current <- List.hd document.undo_stack;      (*pop off the top of the undo stack into the current list*)
      document.undo_stack <- List.tl document.undo_stack;
      true
    end
;;
(* val undo : 'a document -> bool
   If the undo_stack is not empty, undo the last operation. current
   is moved to the redo_stack and the top element of undo_stack is
   removed and becomes current.  Returns true if changes are made
   and false if undo_stack is empty and no changes are made. Operates
   in constant time. *)

let redo document =
  if document.redo_stack = [] (*check if the redo stack is empty*)
    then                (*if so return false*)
      false
    else                (*otherwise*)
      begin
        document.undo_stack <- document.current::document.undo_stack; (*move the current list onto the undo stack*)
        document.current <- List.hd document.redo_stack;      (*pop off the to of the redo stack into the current list*)
        document.redo_stack <- List.tl document.redo_stack;
        true
      end
;;
(* val redo : 'a document -> bool
   If the redo_stack is not empty, redo the last operation. current
   is moved to the undo_stack and the top element of redo_stack is
   removed and becomes current.  Returns true if changes are made
   and false if redo_stack is empty and no changes are made. Operates
   in constant time. *)
