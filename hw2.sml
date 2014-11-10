fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(x : string, ys: string list) =
  case ys of
       [] => NONE
     | y::ys' =>  if same_string(x, y)
                  then SOME x
                  else all_except_option(x, ys')

(*self util*)
fun delete_id(sl: string list, s string) =
  case sl of
       [] => []
     | fst::sl' => if same_string(fst, s)
                   then delete_id(sl',s)
                   else fst::delete_id(sl', s)

fun get_substitutions1(xss: (string list) list, y : string) =
  case xss of
       [] => []
     | xs::xss' => if all_except_option(y, xs)
                   then delete_it(xs, y) @ all_except_option(xss', y)
                   else all_except_option(xss', y)

fun get_substitutions1(xss: (string list) list, y : string) = 
  let fun f (xss: (string list) list, y : string, acc : string) =
          case xss of
               [] => acc
             | xs::xss' => if all_except_option(y, xs)
                           then all_except_option(xss', delete_id(xs, y) @ acc)
                           else all_except_option(xss', y, acc)
  in
    f(xss, y, [])
  end

  (*
fun similar_names(xss : (string list) list, name : (string*string*string)) =
  *)





(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c : card) =
  case c of
       Spades => Black
     | Clubs => Blace
     | _ => Red


fun card_value(c : card) =
  case c of
       (s, r) = > case r of
                       Num x => x
                       Ace => 11
                       _ => 10

fun remove_card(cs : card list, y : card, e : exception) =
  case cs of
       [] => raise e
     | c::cs' => if c = y
                 then cs'
                 else c::remove_card(cs', y, e)

fun all_same_color(cs : card list) =
  case cs of
       [] => true
     | [x] => true
     | x::(y::cs') => if x = y andalso all_same_color(y::cs')
                      then true
                      else false

fun sum_cards(cs : card list) = 
  let fun f(cs : card list, acc : int) =
  case cs of
       [] => acc
     | c::cs' => f(cs', acc + card_value(c))
  in
    f(cs, 0)
  end

(*unfinished*)



