
use "libs/definitions.sml";
(* Env*Fun -> Env*Result *)
fun eval ((env: Env), (K (n))): Env*Result = (env, Integer n) |
    eval ((env: Env), (Variable (s))): Env*Result =
    (*Abbiamo il caso in cui nella variabile abbiamo associato un espressione oppure un valore*) 
    (*Potremmo trovare un espressione associata o un valore, se è un valore da errore e quindi non viene fatta la conversione*)
    let 
        val ((newEnv,newVal),err) = ((eval (env,(toExp (search env s))),false) handle
            NotConvertable other => ((env,other),true));
    in
        if err then (newEnv,newVal) (*non c'è bisogno di modificare env*)
        else (push newEnv (s,newVal),newVal)
    end |
    eval ((env: Env), (Plus (n, m))): Env*Result = 
        let
            val (env1, n1) =  (eval (env, n)); (*TODO: capire se mettere due eval diversi o sempre lo stesso*)
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
        end |
    eval ((env: Env), (Let (s,m,n))): Env*Result =
        let
            val newEnv = push env (s,Exp m);
        in
            (eval (newEnv, n))
        end;