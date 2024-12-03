datatype 'a List = empty | cons of ('a * 'a List);

type Var = string;
type Val = int;

type 'a GenericEnv = (Var*'a) List;
type 'a GenericCtx = (Var*'a) List;

exception EmptyList;
exception NotConvertable;
exception TODO;

fun top (empty) = raise EmptyList |
    top (cons (i,restoDellaLista)) = i;