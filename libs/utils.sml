datatype 'a List = empty | cons of ('a * 'a List);

type Var = string;
type Val = int;

exception EmptyList;
exception NotConvertable;
exception TODO;

fun top (empty) = raise EmptyList |
    top (cons (i,restoDellaLista)) = i;