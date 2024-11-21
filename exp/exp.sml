use "../libs/utils.sml";
type Env = (string*int) List;
(*
    (("x",2),(("y",3),empty))
*)

fun search (empty: Env) (needle: string) = raise EmptyList "unbound variable" |
    search (cons ((varname,content), haystack): Env) (needle: string) = 
        if varname = needle then content
        else search haystack needle;

datatype Exp = K of int | Plus of (Exp*Exp) | Var of string 
    | Let of (string*Exp*Exp);
(*
    (("z",3),(("x",2),(("y",3),empty)))
*)
fun eval (env: Env) ((K (n)): Exp) = n |
    eval (env: Env) ((Var (s)): Exp) = search env s | (* deve leggere dall'env e recuperare il valore, altrimenti dare errore*)
    eval (env: Env) (Plus (n, m)) = (eval env n) + (eval env m) |
    eval (env: Env) (Let (s, m, n)) =
        let
            val newEnv = cons ((s, eval env m), env);
        in
            eval newEnv n
        end; (* creare la variabile s, assegnargli m ed eseguire n *)

val myEnv: Env = empty;
eval myEnv (Let ("x",K 9,(Plus ((Var "x"),K 9))));
(* let x = 9 in x + 9 *)