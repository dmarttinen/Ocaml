(* map_modules.ml: provides two modules for maps.
   1. StringStringMap which maps strings to strings
   2. IntpairBoolMap which maps pairs of ints to bools.
   Both modules are created by creating a short module adhering to the
   Treemap.KEYVAL_SIG signature and then invoking the Treemap.Make
   functor. *)

open Printf;;

(* Interface module for maps of string to string *)
module StringStringKV = struct
  type key_t = string;;                                   (*set the key type to be a string*)
  type value_t = string;;                                 (*set the value type to be a string*)
  let compare_keys a b = String.compare a b;;             (*define the compare_keys function, since both are string, just use String.compare*)
  let keyval_string a b = sprintf "{%s -> %s}" a b;;      (*define the keyval_string function, use sprintf to convert to a formatted string*)
end

(* A map module from string keys to string values. *)
module StringStringMap = Treemap.Make (StringStringKV);;  (*use Treemap.Make and the StringStringKV interface to make a string string map*)

(* Interface module for maps of int pairs to bool *)
module IntpairBoolKV = struct
  type key_t = (int * int);;            (*define the key type as a pair of ints*)
  type value_t = bool;;                 (*define the value type as a bool*)
  let compare_keys (a1,a2) (b1,b2) =    (*define the compare_keys function*)
    if a1 = b1 then a2 - b2             (*if the first value from each is the same, then compare the second values*)
    else a1 - b1                        (*otherwise, compare the first values*)
  ;;
  let keyval_string (a1,a2) b = sprintf "{%i > %i : %b}" a1 a2 b;;  (*define the keyval_string function, use sprintf to create a formatted string*)
end

(* A map module from int pair keys to bool values. *)
module IntpairBoolMap = Treemap.Make (IntpairBoolKV);;
