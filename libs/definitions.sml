use "libs/utils.sml";

(* Fun eagear dinamico *)
datatype Fun = K of Val | Plus of (Fun*Fun) | Variable of Var (*nome valore*)
    | Fn of (Var*Fun) | Call of (Fun*Fun);

type 'a GenericEnv = (Var*'a) List;

datatype EnvItem = Integer of Val |
    Function of (Var*Fun) | Exp of Fun | NewEnv of EnvItem GenericEnv*Val;

type Env = (EnvItem GenericEnv)

fun toInt (Integer x) =  x |
    toInt (other:EnvItem) = raise NotConvertable;

fun toFun (Function f) = f |
    toFun (other:EnvItem) = raise NotConvertable;

fun toExp (Exp e) = e |
    toExp (other:EnvItem) = raise NotConvertable;

fun toEnv (NewEnv (env,item)) = (env,item) |
    toEnv (other:EnvItem) = raise NotConvertable;


fun search (empty: Env) (needle: Var) = raise EmptyList |
    search (cons ((varname,content), haystack): Env) (needle: Var) = 
        if varname = needle then content
        else search haystack needle;

(*deve ritornare il nuovo env*)
fun replace (empty: Env) (needle:Var) (newVal: EnvItem) = raise EmptyList |
    replace (cons ((name,oldVal), haystack): Env) (needle:Var) (newVal:EnvItem) = 
        if name = needle then raise TODO
        else raise TODO;

fun search_and_replace (env: Env) (needle: Var) =
    let
        val res = search env needle
    in
        replace env needle res
    end; 