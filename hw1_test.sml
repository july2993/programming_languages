use "hw1.sml";

val test1 = double 17 = 34

val test2 = double 0 = 0

val test3 = triple ~4 = ~12

val test4 = triple 0 = 0

val test5 = f(12,27) = 324

val test_is_older1 = is_older((3,3,3), (3,3,2)) = true

val test_number_in_month= number_in_month([(1988, 7, 26), (1988, 7, 22)], 7) = 2

val test_number_in_months = number_in_months([(1988, 7, 26), (1988, 8, 22)], [7, 8, 9]) = 2

val test_dates_in_month = dates_in_month([(1988, 7, 26), (1988, 7, 22)], 7) = [(1988, 7, 26),
(1988, 7, 22)]

val test_dates_in_months = dates_in_months([(1988, 7, 26), (1988, 8, 22)], [7, 8, 9]) = [(1988, 7,
26), (1988, 8, 22)]

val test_get_nth = get_nth(["fst", "se", "tri"], 2) = "se"

val test_date_to_string = date_to_string((1988, 7, 26)) = "July 26, 1988"

val test_number_before_reaching_sum = number_before_reaching_sum(3, [1, 2, 3]) = 2

val test_what_month = what_month(59) = 2

val test_month_range = month_range(59, 60) = [2, 3]

val test_oldest = oldest([(1, 1, 1), (2, 2, 2)]) = SOME (2, 2, 2)

val test_oldest2 = oldest([]) = NONE
