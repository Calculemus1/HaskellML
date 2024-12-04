datatype 'a List = empty | cons of ('a * 'a List);

type Var = string;
type Val = int;

type 'a GenericEnv = (Var*'a) List;
type 'a GenericCtx = (Var*'a) List;

exception EmptyList;
exception NotConvertable;
exception TODO;

fun top (empty) = raise EmptyList |
    top (cons (item,haystack)) = item;

fun push (list) (item) = cons (item, list);

fun pop (empty) = raise EmptyList |
    pop (cons (item, haystack)) = item;