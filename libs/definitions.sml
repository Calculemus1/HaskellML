use "libs/utils.sml";

(* Fun eagear dinamico *)
datatype Fun = K of int | Plus of (Fun*Fun) | Variable of Var 
    | Fn of (Var*Fun) | Call of (Fun*Fun);

datatype EnvItem = Integer of Val | Function of (Var*Fun) | Exp of Fun;

exception NotConvertable of string;

fun toInt (Integer x) =  x |
    toInt (Function _) = raise NotConvertable "non posso convertire funzione in intero" |
    toInt (Exp _) = raise NotConvertable "non posso convertire direttamente un espressione in intero";

fun toFun (Integer _) = raise NotConvertable "non posso convertire un intero a una funzione" |
    toFun (Function f) = f |
    toFun (Exp _) = raise NotConvertable "non posso convertire direttamente un espressione a una funzione";

fun toExp (Integer _) = raise NotConvertable "non posso convertire un expressione a un intero" |
    toExp (Function _) = raise NotConvertable "non posso convertire un expressione a una funzione" |
    toExp (Exp e) = e;

type Env = (string*EnvItem) List;

fun search (empty: Env) (needle: Var) = raise EmptyList "unbound variable" |
    search (cons ((varname,content), haystack): Env) (needle: Var) = 
        if varname = needle then content
        else search haystack needle;

(*deve ritornare il nuovo env*)
fun replace  (empty: Env) (needle:Var) (newVal: EnvItem) = raise EmptyList "unbound variable" |
    replace (cons ((name,oldVal), haystack): Env) (needle:Var) (newVal:EnvItem) = 
        if name = needle then raise TODO
        else raise TODO;

fun search_and_replace (env: Env) (needle: Var) =
    let
        val res = search env needle
    in
        replace env needle res
    end; 