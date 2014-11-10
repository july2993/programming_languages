fun f(x,y) = x * y 

(* Do not change these: They should be correct after fixing the code above *)

fun double x = f(x,2)

fun triple x = f(3,x)

fun is_older (p : (int*int*int), q : (int*int*int)) =
  if (#1 p) > (#1 q)
  then true
  else if (#1 p) = (#1 q) andalso (#2 p) > (#2 q)
  then true
  else if (#1 p) = (#1 q) andalso (#2 p) = (#2 q) andalso (#3 p) > (#3 q)
  then true
  else false

fun number_in_month(xs : (int*int*int) list, y : int) = 
  if null xs
  then 0
  else let val same = ((#2 (hd xs)) = y)
       in
         if same
         then 1 + number_in_month(tl xs, y)
         else number_in_month(tl xs, y)
       end

fun number_in_months(xs : (int*int*int) list, ys : int list) = 
  if null ys
  then 0
  else number_in_month(xs, hd ys) + number_in_months(xs, tl ys)

fun dates_in_month(xs : (int*int*int) list, y : int) =
  if null xs
  then []
  else let val same = ((#2 (hd xs)) = y)
       in
         if same
         then hd xs :: dates_in_month(tl xs, y)
         else dates_in_month(tl xs, y)
       end


fun dates_in_months(xs : (int*int*int) list, ys : int list) = 
  if null ys
  then []
  else dates_in_month(xs, hd ys) @ dates_in_months(xs, tl ys)


fun get_nth(xs: string list, y: int) =
  if y = 1
  then hd xs
  else get_nth(tl xs, y - 1)

fun date_to_string(d: (int*int*int)) =
  let val ds = ["January", "February", "March", "April", "May", "June", "July", "August",
  "September", "October", "November", "December"]
  in
    get_nth(ds,(#2 d)) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end


fun number_before_reaching_sum(sum: int, xs: int list) = 
  let val fst = hd xs
  in
    if sum - fst <= 0
    then 1
    else 1 + number_before_reaching_sum(sum - fst, tl xs)
  end

fun what_month(d: int) =
  let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(d, months)
  end


fun month_range(d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1 + 1, d2)


fun oldest(xs: (int*int*int) list) =
  if null xs
  then NONE
  else
    let val tl_ans = oldest(tl xs)
    in if isSome tl_ans andalso is_older(valOf tl_ans, hd xs)
       then tl_ans
       else SOME (hd xs)
    end




