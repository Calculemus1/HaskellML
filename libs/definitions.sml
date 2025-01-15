use "libs/utils.sml";

(*TODO: valutarci se lasciare Var oppure mettere TypeVar*)
datatype TauTypes = number | boolean | VarType of Var | Arrow of (TauTypes*TauTypes);
datatype SigmaTypes = Type of TauTypes | ForAll of (Var*SigmaTypes);

datatype Fun = K of Val | Plus of (Fun*Fun) | Variable of Var (*nome valore*)
    | Fn of (Var*Fun) | Call of (Fun*Fun) | Let of (Var*Fun*Fun);

datatype Result = Integer of Val |
    Function of (Var*Fun) | Exp of Fun;

exception NotConvertable of Result;

type Env = (Result GenericEnv)

fun toInt (Integer x) =  x |
    toInt (other:Result) = raise NotConvertable other;

fun toFun (Function f) = f |
    toFun (other:Result) = raise NotConvertable other;

fun toExp (Exp e) = e |
    toExp (other:Result) = raise NotConvertable other;


fun search (empty: Env) (needle: Var) = raise EmptyList |
    search (cons ((varname,content), haystack): Env) (needle: Var) = 
        if varname = needle then content
        else search haystack needle;

(*deve ritornare il nuovo env*)
fun replace (empty: Env) (needle:Var) (newVal: Result) = raise EmptyList |
    replace (cons ((name,oldVal), haystack): Env) (needle:Var) (newVal:Result) = 
        if name = needle 
            then (oldVal,cons ((name,newVal),haystack))
        else 
        let val (oldVal, newEnv) = replace haystack needle newVal;
        in (oldVal,(cons ((name,oldVal),haystack))) end;