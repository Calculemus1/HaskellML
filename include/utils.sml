datatype 'a List = empty | cons of ('a * 'a List);

exception EmptyList of string;

fun top (empty) = raise EmptyList "the list is empty" |
    top (cons (i,restoDellaLista)) = i;
type Env = (string*int) List;
(*
    (("x",2),(("y",3),empty))
*)

fun search (empty: Env) (needle: string) = raise EmptyList "unbound variable" |
    search (cons ((varname,content), haystack): Env) (needle: string) = 
        if varname = needle then content
        else search haystack needle;