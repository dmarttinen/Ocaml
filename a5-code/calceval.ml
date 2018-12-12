(* calceval.ml:  *)
open Calclex;;
open Calcparse;;
open Printf;;

(* Module for mapping variable names to variable data. *)
module Varmap = Map.Make(String);;

(* Data type for evaluated expressions; variable names are bound to
   data_t values in variable maps. *)
type data_t =
  | IntDat  of int
  | BoolDat of bool
  | Closure of {param_name: string;
                code_expr:  Calcparse.expr;
                mutable varmap: varmap_t; }

(* Type for a a variable map which maps string to data_t (name to value) *)
and varmap_t = data_t Varmap.t;;

(* Empty variable map. *)
let empty_varmap : varmap_t = Varmap.empty;;

(* Creates a short string version of the a data_t value. *)
let rec data_string data =
  match data with
  | IntDat(i)  -> sprintf "IntDat(%d)"  i
  | BoolDat(b) -> sprintf "BoolDat(%b)" b
  | Closure(c) -> sprintf "Closure(%s, <fun>)" c.param_name

(* Creates a long string version of the data_t value. *)
and data_long_string data =
  match data with
  | IntDat(i)  -> sprintf "IntDat(%d)"  i
  | BoolDat(b) -> sprintf "BoolDat(%b)" b
  | Closure(c) ->
     sprintf "Closure(param_name: %s, varmap=%s code=\n%s)"
       c.param_name (varmap_string c.varmap) (Calcparse.parsetree_string c.code_expr)

(* Creates a string version of a varmap_t by iterating through all its
   names (keys) and appending string versions of the associated data
   (values) *)
and varmap_string varmap =
  let buf = Buffer.create 256 in                    (* extensibel character buffer *)
  Buffer.add_string buf "{";
  let help vname vdata =
    let str = sprintf "%s: %s, " vname (data_string vdata) in
    Buffer.add_string buf str;
  in
  Varmap.iter help varmap;
  if varmap <> Varmap.empty then
    Buffer.truncate buf ((Buffer.length buf)-2);
  Buffer.add_string buf "}";
  Buffer.contents buf
;;

(* Exception associated with evaluation errors. All strings to enable
   easier printing when the exception is caught. *)
exception EvalError of {
    msg    : string;
    varmap : string;
    expr   : string
};;

(* Convenience function which stringifies arguments then creates an
   exception. *)
let eval_error msg varmap expr =
  EvalError{msg = msg;
            varmap=varmap_string varmap;
            expr="\n"^(Calcparse.parsetree_string expr); }
;;

(* Evaluation function for expressions. Recursively descends through
   the expression tree evaluating each kind of expression. Parameter
   varmap is a varmap_t with the current variable bindings. *)
let rec eval_expr varmap expr =
  match expr with
  | IntExp i -> IntDat i                                             (* constants are self-evaluating *)

  | BoolExp b -> BoolDat b

  | Varname varname ->                                               (* look up variables in varmap *)
     begin
       match Varmap.find_opt varname varmap with
       | Some d -> d
       | None ->
          let msg = sprintf "No variable '%s' bound" varname in
          raise (eval_error msg varmap expr)
     end

  | Intop(o) ->                                                      (* integer operations like + - * / *)
     begin
      let ldata = eval_expr varmap o.lexpr in
      let rdata = eval_expr varmap o.rexpr in
      match ldata,rdata with
      | (IntDat li),(IntDat ri) ->
         begin
          match o.op with
          | Add     -> IntDat  (li + ri)
          | Sub     -> IntDat  (li - ri)
          | Mul     -> IntDat  (li * ri)
          | Div     -> IntDat  (li / ri)
          | Less    -> BoolDat (li < ri)
          | Greater -> BoolDat (li > ri)
          | Equal   -> BoolDat (li = ri)
        end
      | (IntDat li),rerr ->
         let msg = sprintf "Expect Int for right arithmetic expression, found '%s'" (data_string rerr) in
         raise (eval_error msg varmap expr)
      | lerr,_ ->
         let msg = sprintf "Expect Int for right arithmetic expression, found '%s'" (data_string lerr) in
         raise (eval_error msg varmap expr)
    end

  | Boolop(o) ->                                                     (* boolean operations *)
     begin                                                           (* PATCH PROBLEM *)
       raise (eval_error "Boolean operations not yet impelemented" varmap expr)
     end

  | Cond(c) ->                                                       (* IMPLEMENT #1: conditionals *)
     begin
       let b = eval_expr varmap c.if_expr in            (*check the test case*)
       match b with
       | BoolDat b ->                                   (*if the test case returns a boolean*)
        begin
          match b with                                  (*match the boolean to determine which path to follow*)
          | true -> eval_expr varmap c.then_expr        (*if true, evaluate the true expression*)
          | false -> eval_expr varmap c.else_expr       (*if false, evaluate the false expression*)
        end
       | _ ->
        let msg = sprintf "Expected Bool for if(), found '%s'" (data_string b) in   (*if something other than a bool is passed in as the test case, throw an error*)
        raise (eval_error msg varmap expr)
     end

  | Letin(l) ->                                                      (* let/in expressions *)
     begin
       let var_data = eval_expr varmap l.var_expr in
      match var_data with
      | Closure c ->                                                    (*check if the binding is a closure. If so, then in needs to be processed recursively*)
        c.varmap <- Varmap.add l.var_name var_data c.varmap;            (*add the new binding into the varmap so it can find itself if it needs to recurse*)
        eval_expr c.varmap l.in_expr                                    (*evaluate the expression with the closure map*)
      | _ -> let new_varmap = Varmap.add l.var_name var_data varmap in  (*if not a closure, evaluate like normal*)
      eval_expr new_varmap l.in_expr

     end

  | Lambda(l) ->                                                     (* lambda expressions *)
     begin                                                           (* IMPLEMENT #3: Build a Closure *)
       let nclosure = Closure{param_name = l.param_name; code_expr = l.code_expr; varmap = varmap} in (*create a Closure*)
       nclosure                                                      (*return the new closure*)
     end

  | Apply(apply) ->                                                  (* function application *)
     begin                                                           (* IMPLEMENT #4: evaluate the application expression *)
       let fun_ex = eval_expr varmap apply.func_expr in
       match fun_ex with
       | Closure c ->                                                 (*check if the expression is a CLosure*)
        begin
          let param_ex = eval_expr varmap apply.param_expr in         (*evaluate the parameter expression*)
          let new_map = Varmap.add c.param_name param_ex c.varmap in  (*add the parameter name and expression to the closure's varmap*)
          let rec helper map code =                                   (*recursively evaluate the expressions in the closure*)
            eval_expr map code
          in helper new_map c.code_expr
        end
       | _ -> let msg = sprintf "Expected Closure for application, found '%s'" (data_string fun_ex) in   (*if something other than a Closure is passed in, throw an error*)
       raise (eval_error msg varmap expr)
     end
;;

(********************************************************************************)
(* PATCH PROBLEM: Optimizes an expression by evaluating some of the
   constant expressions in it (constant folding and constant
   propagation). Performs only optimizations on left-associative
   expressions. For example, the expression "2+3+n" parses to

   Add (Add (IntExp 2,
             IntExp 3),
        Varname n)

   and is optimized to

   Add (IntExp 5,
        Varname n)

   However, the expression "n+2+3" parses to

   Add (Add (Varname n,
             IntExp 2),
        IntExp 3)

   and is not optimized/transformed by this function.

   Optimizes code within coditionals eliminating branches if possible
   and also within lambda code expressions.
*)
let rec optimize_expr varmap expr =
  (* IMPLEMENT THIS FUNCTION to complete the PATCH PROBLEM *)
  failwith "Expression Optimization not yet implemented"
;;
