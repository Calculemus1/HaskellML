
use "libs/definitions.sml";


(* Env*Fun -> EnvItem *)
fun eval (env: Env) (K (n)) = Integer n |
    eval (env: Env) (Var (s)) = raise TODO | (* TODO: scrivere richiamo a replace*)
    eval (env: Env) (Plus (n, m)) = Integer ((toInt (eval env n)) + (toInt (eval env m))) |
    eval (env: Env) (Fn (s, m)) = Function (s,m) |
    eval (env: Env) (Call (m,n)) = 
        let
            val (param,corpo) =  toFun (eval env m);
            val newEnv = cons ((param,Exp n), env);
        in
            eval newEnv corpo
        end;

val myEnv: Env = empty;
(* (fn x => x + 1) 5 -> 6 *)
eval myEnv (Call ((Fn ("x",(Plus(Var ("x"),K 1)))),(K 5)));
(* let x = 1 in let y = x in let x = 2 in y*)
(*let x = M in N*)
(*(fn x => N) M*)
(* (fn x => (fn y => (fn x => y) 2) x) 1 *)
eval myEnv (Call ((Fn ("x",(Call((Fn("y", (Call(Fn("x",(Var("y"))),K 2)) )),(Var("x")))))),(K 1)));