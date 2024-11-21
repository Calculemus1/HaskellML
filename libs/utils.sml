datatype 'a List = empty | cons of ('a * 'a List);

exception EmptyList of string;

exception TODO;

fun top (empty) = raise EmptyList "the list is empty" |
    top (cons (i,restoDellaLista)) = i;

type var = string;

