datatype 'a List = empty | cons of 'a* 'a List;

exception EmptyList of string;

fun top empty = raise EmptyList "the list is empty" |
    top (cons (i,_)) = i;

(*TODO: mettere dentro la stringa il nome non trovato*) 
type Env = (string*int) List;

fun search empty needle = raise EmptyList "unbound variable" |
    search (cons ((i,content), haystack)) needle = 
        if i = needle then content
        else search haystack needle;