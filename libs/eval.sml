
use "libs/definitions.sml";
(*TODO: capire se ho rispettato i requisiti della call-by-name*)
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
            val (env1, n1) =  (eval (env, n)); (*TODO: capire se mettere due eval diversi o sempre lo stesso*)
            val (env2, n2) =  (eval (env1, m));
            (*val newEnv = union env1 env2;*)
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
        end |
    eval ((env: Env), (Let (s,m,n))): Env*Result =
        let
            val newEnv = push env (s,Exp m);
        in
            (eval (newEnv, n))
        end;