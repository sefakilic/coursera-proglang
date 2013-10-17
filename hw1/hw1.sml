
fun year (date:int*int*int) =
    #1 date

fun month (date:int*int*int) =
    #2 date

fun day (date:int*int*int) =
    #3 date

fun is_older (da:int*int*int, db:int*int*int) =
    if year(da) < year(db)
    then true
    else if year(da) > year(db)
    then false
    else if month(da) < month(db)
    then true
    else if month(da) > month(db)
    then false
    else if day(da) < day(db)
    then true
    else false

fun number_in_month (dates: (int*int*int) list, mon:int) =
    if null dates
    then 0
    else let val rest = number_in_month(tl dates, mon)
         in
             rest + (if month(hd dates) = mon
                     then 1
                     else 0)
         end
             

fun number_in_months (dates: (int*int*int) list, mons: int list) =
    if null mons
    then 0
    else number_in_month(dates, hd mons) + number_in_months(dates, tl mons)

fun dates_in_month (dates: (int*int*int) list, mon: int) =
    if null dates
    then []
    else let val rest = dates_in_month(tl dates, mon)
         in
             if month(hd dates) = mon
             then (hd dates) :: rest
             else rest
         end

fun dates_in_months (dates: (int*int*int) list, mons: int list) =
    if null mons
    then []
    else dates_in_month(dates, hd mons) @ dates_in_months(dates, tl mons)

fun get_nth (xs: string list, n: int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

fun date_to_string (date:int*int*int) =
    let val months = ["January",
                     "February",
                     "March",
                     "April",
                     "May",
                     "June",
                     "July",
                     "August",
                     "September",
                     "October",
                     "November",
                     "December"]
    in
        get_nth(months, month(date)) ^ " " ^ Int.toString(day(date)) ^ ", " ^ Int.toString(year(date))
end
    
fun number_before_reaching_sum (sum:int, xs:int list) =
    if sum - hd xs <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month (n:int) =
    number_before_reaching_sum(n, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

fun month_range (day1:int, day2:int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun oldest_helper (ds : (int*int*int) list) =
                 (* ds must be non empty *)
                 if null (tl ds)
                 then hd ds
                 else let val oldest_rest = oldest_helper(tl ds)
                      in 
                          if is_older(hd ds, oldest_rest)
                          then hd ds
                          else oldest_rest
                      end
         in SOME(oldest_helper(dates))
         end


(* Challenge problems *)

fun elem (x : int, xs : int list) =
    if null xs
    then false
    else if (x = hd xs)
    then true
    else elem (x, tl xs)

fun remove_duplicates (xs : int list) =
    if null xs
    then []
    else if elem(hd xs, tl xs)
    then remove_duplicates(tl xs)
    else hd xs :: remove_duplicates(tl xs)

fun number_in_months_challenge (dates: (int*int*int) list, mons: int list) =
    (* same with number_in_months except having a month in the second argument
    multiple times has no more effect than having it once. *)
    number_in_months(dates, remove_duplicates(mons))

fun dates_in_months_challenge (dates: (int*int*int) list, mons: int list) = 
    dates_in_months(dates, remove_duplicates(mons))

fun reasonable_date (date : int*int*int) = 
    let fun is_leap (year : int) =
            year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
        fun get_nth (xs : int list, n : int) =
            if n = 1
            then hd xs
            else get_nth(tl xs, n-1)
        val y = year(date)
        val m = month(date)
        val d = day(date)
        val month_lens = [31, if is_leap(y) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in 
        y > 0 andalso m >= 1 andalso m <= 12 andalso d >= 1 andalso d <= get_nth(month_lens, m)
    end
    
