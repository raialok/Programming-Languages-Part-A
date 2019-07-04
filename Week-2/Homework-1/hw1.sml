(* Homework 1 *)

(* 1 *)
(* (int * int * int) * (int * int * int) -> bool *)
fun is_older (date_one: int * int * int, date_two: int * int * int) =
    (#1 date_one < #1 date_two) 
    orelse (#1 date_one = #1 date_two andalso #2 date_one < #2 date_two)
    orelse (#1 date_one = #1 date_two andalso #2 date_one = #2 date_two
            andalso #3 date_one < #3 date_two)

(* 2 *)
(* (int * int * int) list * int -> int *) 
fun number_in_month (dates: (int * int * int) list, month: int) =
	if null dates
	then 0
	else
		let fun date_in_month (date: (int * int * int)) =
			#2 date = month
		in if date_in_month (hd dates)
			then 1 + number_in_month (tl dates, month)
			else number_in_month (tl dates, month)
		end

(* 3 *)
(* (int * int * int) list * int list -> int *)
fun number_in_months (dates: (int * int * int) list, months: int list) =
	if null dates orelse null months
	then 0
	else
		let val total_dates_in_a_month = number_in_month (dates, hd months)
		in total_dates_in_a_month + number_in_months (dates, tl months)
	end

(* 4 *)
(* (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (dates: (int * int * int) list, month: int) =
	if null dates
	then []
	else
		if #2 (hd dates) = month
		then (hd dates) :: dates_in_month (tl dates, month)
		else dates_in_month (tl dates, month)

(* 5 *)
(* (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
	if null dates orelse null months
	then []
	else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

(* 6 *)
(* string list * int -> string *)
fun get_nth (strings_list: string list, n: int) =
	if n = 1
	then hd strings_list
	else get_nth (tl strings_list, n - 1)

(* 7 *)
(* int * int * int -> string *)
fun date_to_string (date: int * int * int) =
	let val months = ["January", "February", "March", "April" , "May" , 
		"June" , "July", "August", "September", "October", "November", 
		"December"]
	in get_nth (months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ 
		Int.toString(#1 date)
	end

(* 8 *)
(* int * int list -> int *)
fun number_before_reaching_sum (sum: int, number_list: int list) =
	let fun helper_before_sum (n, num_list: int list, current_sum: int) =
		if current_sum + hd num_list >= sum
		then n
		else 
			helper_before_sum (n + 1, tl num_list, current_sum + hd num_list)
	in helper_before_sum (0, number_list, 0)
	end

(* 9 *)
(* int -> int *)
fun what_month (day: int) =
	let val days_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in number_before_reaching_sum (day, days_months) + 1
	end

(* 10 *)
(* int * int -> int list *)
fun month_range (day1: int, day2: int) =
	if day1 > day2
	then []
	else what_month (day1) :: month_range (day1 + 1, day2)

(* 11 *)
(* (int * int * int) list -> (int * int * int) option *)
fun oldest (dates: (int * int * int) list) =
	if null dates
	then NONE
	else 
		let fun oldest_non_empty 
			(non_empty_dates: (int * int * int) list) =
			if null (tl non_empty_dates)
			then hd non_empty_dates
			else
				let val oldest_tl = oldest_non_empty (tl non_empty_dates)
				in
					if is_older (hd non_empty_dates, oldest_tl)
					then hd non_empty_dates
					else oldest_tl
				end
		in SOME (oldest_non_empty (dates))
		end

(* 12 *)
(* int list -> int list *)
fun remove_duplicates (nums: int list) =
	if null nums
	then []
	else 
		let fun member (num: int, lst: int list) =
			if null lst
			then false
			else if (hd lst) = num 
                then true
                else member (num, tl lst)
		in
			if not (member (hd nums, tl nums))
			then (hd nums) :: remove_duplicates (tl nums)
			else remove_duplicates (tl nums)
		end

(* (int * int * int) list * int list -> int *)
fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months (dates, remove_duplicates (months))

(* (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months (dates, remove_duplicates (months))

(* 13 *)
(* int list * int -> int option *)
fun get_nth_list_item (lst: int list, n: int) =
    if lst = []
    then NONE
    else if n = 1
        then SOME (hd lst)
        else get_nth_list_item (tl lst, n - 1)

(* int * int * int -> int *)
fun max_days_in_month (date: int * int * int) =
    let val days_months_regular = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
        val days_months_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        let fun is_leap (year: int) =
            (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        in
            if is_leap (#1 date)
            then valOf (get_nth_list_item (days_months_leap, (#2 date)))
            else valOf (get_nth_list_item (days_months_regular, (#2 date)))
        end
    end

(*  int * int * int -> bool *)
fun reasonable_date (date: int * int * int) =
    if (#1 date) < 1 orelse (#2 date) < 1 orelse (#2 date) > 12 orelse 
    (#3 date < 1) orelse (#3 date > 31)
    then false
    else
        (#3 date) <= max_days_in_month (date)