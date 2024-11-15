use "include/utils.sml";

datatype Exp = K of int | Plus of Exp*Exp | Var of string 
    | Let of string*Exp*Exp;

fun eval env (K n) = n |
    eval env (Var s) = search env s | (* deve leggere dall'env e recuperare il valore, altrimenti dare errore*)
    eval env (Plus (n, m)) = (eval env n) + (eval env m) |
    eval env (Let (s, m, n)) =
        let
            val newEnv = cons ((s, eval env m), env);
        in
            eval newEnv n
        end; (* creare la variabile s, assegnargli m ed eseguire n *)

val myEnv: Env = empty;
eval myEnv (Let ("x",K 2,(Plus ((Var "x"),K 9))));
