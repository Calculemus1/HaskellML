use "include/utils.sml";
(* Fun eagear dinamico *)
datatype Fun = K of int | Plus of (Fun*Fun) | Var of string 
    | Fn of (string*Fun) | Call of (Fun*Fun);

datatype EnvItem = Integer of int | Function of (string*Fun);

exception NotConvertable of string;

fun toInt (Integer x) =  x |
    toInt (Function _) = raise NotConvertable "non posso convertire funzione in intero";

fun toFun (Integer _) = raise NotConvertable "non posso convertire un intero a una funzione" |
    toFun (Function f) = f;


type Env = (string*EnvItem) List;

(*TODO: capire se lasciare qua questa funzione o in utils*)
fun search (empty: Env) (needle: string) = raise EmptyList "unbound variable" |
    search (cons ((varname,content), haystack): Env) (needle: string) = 
        if varname = needle then content
        else search haystack needle;
(* Env*Fun -> EnvItem *)
fun eval (env: Env) (K (n)) = Integer n |
    eval (env: Env) (Var (s)) = search env s | (* deve leggere dall'env e recuperare il valore, altrimenti dare errore*)
    eval (env: Env) (Plus (n, m)) = Integer ((toInt (eval env n)) + (toInt (eval env m))) |
    eval (env: Env) (Fn (s, m)) = Function (s,m) |
    eval (env: Env) (Call (m,n)) = 
        let
            val (param,corpo) =  toFun (eval env m);
            val valueIn: EnvItem = eval env n;
            val newEnv = cons ((param,valueIn), env);
        in
            eval newEnv corpo
        end;

val myEnv: Env = empty;
eval myEnv (Call ((Fn ("x",(Plus(Var ("x"),K 1)))),(K 5)));
(* (fn x => x + 1) 5 -> 6 *)
(* let x = 1 in let y = x in let x = 2 in y*)
(*let x = M in N*)
(*(fn x => N) M*)
(* (fn x => (fn y => (fn x => y) 2) x) 1 *)
eval myEnv (Call ((Fn ("x",(Call((Fn("y", (Call(Fn("x",(Var("y"))),K 2)) )),(Var("x")))))),(K 1)));