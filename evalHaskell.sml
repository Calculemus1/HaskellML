
use "libs/definitions.sml";

(* Env*Fun -> Env*Result *)
fun eval ((env: Env), (K (n))): Env*Result = (env, Integer n) |
    eval ((env: Env), (Variable (s))): Env*Result = 
    let
        val exp = toExp (search env s) (*TODO: cattura eccezione e imposta res a quel valore*)
        val (newEnv,newVal) = (eval (env, exp))
        val (oldVal,newEnv) = replace newEnv s newVal
    in
        (newEnv,newVal)
    end |
    eval ((env: Env), (Plus (n, m))): Env*Result = 
        let
            val (env1, n1) =  (eval (env, n));
            val (env2, n2) =  (eval (env1, m));
        in
            (env2, Integer ((toInt n1) + (toInt n2)))
        end |
    eval ((env: Env), (Fn (s, m))): Env*Result = (env,Function (s,m)) |
    eval ((env: Env), (Call (m,n))): Env*Result = 
        let
            val (env1,n1) = (eval (env, m));
            val (param,corpo) =  toFun n1;
            val newEnv = cons ((param,Exp n), env1);
        in
            (eval (newEnv,corpo))
        end;

val myEnv: Env = empty;
(* (fn x => x + 1) 5 -> 6 *)
eval myEnv (Call ((Fn ("x",(Plus(Variable ("x"),K 1)))),(K 5)));
(* let x = 1 in let y = x in let x = 2 in y*)
(*let x = M in N*)
(*(fn x => N) M*)
(* (fn x => (fn y => (fn x => y) 2) x) 1 *)
eval myEnv (Call ((Fn ("x",(Call((Fn("y", (Call(Fn("x",(Variable("y"))),K 2)) )),(Variable("x")))))),(K 1)));