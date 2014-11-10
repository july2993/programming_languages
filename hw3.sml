
exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun is_first_cap s =
let val c = String.sub (s,0)
in
  if Char.isUpper c
  then true
  else false
end

(*
fun only_capitals xs = List.filter ((fn x => Char.isUpper(String.sub (x, 0) )), xs)
  *)
fun only_capitals xs = List.filter is_first_cap xs

fun bigger (x,y) =
  if String.size x > String.size y
  then x
  else y

fun biggerOrEQ (x,y)=
  if String.size x >= String.size y
  then x
  else y


fun longest_string1 xs = List.foldl bigger  ""  xs

fun longest_string2 xs = List.foldl biggerOrEQ  ""  xs

fun longest_string_helper(f, xs) =
  foldl (fn (x, acc) => if f(String.size x,String.size acc) then x else acc)  ""  xs

fun longest_string3 (xs) = longest_string_helper((fn (x, y)  =>   x > y),xs)

fun longest_string4 xs = longest_string_helper((fn (x, y)  =>  x >= y),xs)


fun longest_capitalized(xs) = longest_string3 (only_capitals xs)

fun rev_string(s) =
let val len = size s
in
  case len of
       0 => s
     | _ => (Char.toString(String.sub(s, len - 1))) ^ rev_string (String.substring(s, 0, len - 1))
end

fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' => let val ans = f(x)
                 in
                   case ans of
                      NONE => first_answer f  xs'
                    | SOME v => v
                 end

fun all_answer_help(xs) =
  case xs of
       [] => SOME []
     | NONE::xs' => NONE
     | x::xs' => let val y = all_answer_help(xs')
                 in
                   if y = NONE
                   then NONE
                   else SOME (valOf x @ valOf y)
                 end

fun all_answers f  xs  =
  all_answer_help(List.map f  xs)

(*
fun count_wildcards(p) =
  case p of
       Wildcard => 1
     | Variable v => 0
     | UnitP => 0
     | ConstP v => 0
     | TupleP ps => foldl((fn x acc = > x + acc), map(count_wildcards,ps), 0)
     | ConstructorP (s, pp) =>  count_wildcards(pp)
     *)

fun count_wildcards p = g (fn () => 1) (fn (x) => 0) p

fun count_wild_and_variable_lengths(p) = g (fn () => 1)  String.size p

fun count_some_var (s,p) =
    g (fn () => 0) (fn (vs) => if vs = s then 1 else 0) p


fun get_string(p) =
  case p of
       Variable x => [x]
     | TupleP ps => let val str_list = map (fn (p) => get_string(p)) ps
                    in
                      List.foldl (fn (ls, acc) => (ls @ acc)) [] str_list
                    end
     | ConstructorP(_, p) => get_string(p)
     | _ => []

fun has_repeat(xs) =
  case xs of
       [] => false
     | x::xs' => if List.exists (fn y => if x = y then true else false) xs'
                 then true
                 else has_repeat(xs')

fun check_pat(p) =
  let val xs = get_string p
  in
    if has_repeat xs
    then false
    else true
  end

fun match(v,p) =
  case (v,p) of
       (_, Wildcard) => SOME []
     | (_, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (_, UnitP) => NONE
     | (Const x, ConstP y) => if x = y then SOME [] else NONE
     | (Tuple xs, TupleP ys) => let val pr = ListPair.zip (xs, ys)
                                    val ans_list = map match pr
                                in
                                  if List.length xs = List.length ys
                                  then all_answer_help ans_list
                                  else NONE
                                end
     | (Constructor(s,v), ConstructorP(s2, p2)) => if s = s2 
                                                   then match(v, p2)
                                                   else NONE

     | (_, _) => NONE

fun first_match v ps  =
let val mp = List.map (fn p => match (v,p)) ps
in
  List.foldl (fn (ls, acc) => if acc = NONE andalso ls <> NONE then ls else acc)
  NONE mp
end
