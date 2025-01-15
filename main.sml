use "libs/eval.sml";

val myEnv: Env = empty;
(* (fn x => x + 1) 5 -> 6 *)
eval (myEnv, (Call ((Fn ("x",(Plus(Variable ("x"),K 1)))),(K 5))));

val myEnv: Env = empty;
(* let x = 1 in let y = x in let x = 2 in y -> 2*)
eval (myEnv, (Let ("x",K 1,(Let ("y",Variable "x", (Let ("x",K 2, Variable "y")))))));

val myEnv: Env = empty;
(* let x = 1 in let y = x in let x = 2 in y + x -> 4*)
eval (myEnv, (Let ("x",K 1,(Let ("y",Variable "x", (Let ("x",K 2, Plus (Variable "y",Variable "x"))))))));

(*let x = M in N*)
(*(fn x => N) M*)
(* (fn x => (fn y => (fn x => y) 2) x) 1 *)
val myEnv: Env = empty; (*TODO: capire se questa riassegnazione serva effettivamente*)
eval (myEnv, (Call ((Fn ("x",(Call((Fn("y", (Call(Fn("x",(Variable("y"))),K 2)) )),(Variable("x")))))),(K 1))));