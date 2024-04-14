(* #  Some Functions Associated to Calender.  # *)

type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
type date = { dayofmonth:int; monthnum:int; year:int }

(*1. Write a function date2daynum: date -> int which takes a date and returns the 
corresponding day number of the year it is. Your code should take into consideration 
whether the year in question is a leap year or not. *)
let is_leap_year year =
  (year mod 4 = 0 && year mod 100 <> 0) || (year mod 400 = 0)
(*val is_leap_year : int -> bool = <fun>*)

let days_in_month month year =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31 (*For months with 31 days (January, March, May, July, August, October, December).*)
  | 4 | 6 | 9 | 11 -> 30 (*For months with 30 days (April, June, September, November).*)
  | 2 -> if is_leap_year year then 29 else 28 (*For February*)
  | _ -> failwith "Invalid Month."
(*val days_in_month : int -> int -> int = <fun>*)

let date2daynum date =
  let valid_day date =
    let days_in_current_month = days_in_month date.monthnum date.year in
    date.dayofmonth > 0 && date.dayofmonth <= days_in_current_month
  in
  if date.year <= 0 then
    failwith "Invalid Year."
  else if date.monthnum < 1 || date.monthnum > 12 then
    failwith "Inavlid Month."
  else if not ( valid_day date ) then
    failwith "Invalid Day."
  else
    let rec count_days month day year acc =
      if month = 1 then
        acc + day
      else
        count_days (month - 1) (days_in_month (month - 1) year) year (acc + day)
    in
    count_days date.monthnum date.dayofmonth date.year 0;;
(*val date2daynum : date -> int = <fun>*)

(*Example - *)
date2daynum {dayofmonth = 3; monthnum = 3; year = 2024}
(*- : int = 63*)     (*31 (January) + 29 (February) + 3 (March) = 63.*)


(*2. Write a function weekday_generator:date->weekday->(date->weekday)
which takes a date and a weekday and returns a function which takes a date and returns the 
corresponding weekday. Make sure your returned function also works with leap years. *)
let weekday_generator base_date base_weekday =
  let base_daynum = date2daynum base_date in
  let days_diff date =
    let current_daynum = date2daynum date in
    let rec yeargap year year2 = 
       if year = year2 then 0 
       else if year < year2 then
         (if (is_leap_year year) then 366 + yeargap (year+1) year2 
         else 365 + yeargap (year+1) year2)
       else
         (if (is_leap_year year2) then (- 366 + yeargap year (year2 + 1)) 
         else (- 365 + yeargap year (year2 + 1)))

    in
    ((current_daynum - base_daynum) + yeargap base_date.year date.year) mod 7

  in
  fun current_date ->
    let diff = days_diff current_date in
    let index =
      (match base_weekday with
       | Mon -> 0 | Tue -> 1 | Wed -> 2 | Thu -> 3 | Fri -> 4 | Sat -> 5 | Sun -> 6
      ) in
    let result = ((index + diff) + 7) mod 7 in
    match result with
    | 0 -> Mon | 1 -> Tue | 2 -> Wed | 3 -> Thu | 4 -> Fri | 5 -> Sat | 6 -> Sun
    | _ -> failwith "Invalid Weekday." ;;
(*val weekday_generator : date -> weekday -> date -> weekday = <fun>*)

(*Example - *)
weekday_generator {dayofmonth = 3; monthnum = 3; year = 2024} Sun {dayofmonth = 31; monthnum = 12; year = 2023};;
(*- : weekday = Sun*)
weekday_generator {dayofmonth = 3; monthnum = 3; year = 2024} Sun {dayofmonth = 15; monthnum = 9; year = 2025};;
(*- : weekday = Mon*)


(*3. Write a function calgen: (year:int) -> (date, weekday) Hashtbl.t
which takes a year and returns a Hashtable of size 365 (or 366 if the year is a leap year) 
where the dates in the given year are keys and the corresponding weekdays are the associated values. 
So, practically, this returned hashtable represents the calendar for the given year. *)

let calgen year =
  let calendar = Hashtbl.create (if is_leap_year year then 366 else 365) in
  let rec fill_calendar current_date =
    if current_date.year = year + 1 then
      ()
    else begin
      let weekday = weekday_generator {dayofmonth=1; monthnum=1; year=year} Mon in
      Hashtbl.add calendar current_date weekday;
      let next_date =
        if current_date.dayofmonth < days_in_month current_date.monthnum current_date.year then
          {current_date with dayofmonth = current_date.dayofmonth + 1}
        else if current_date.monthnum < 12 then
          {dayofmonth = 1; monthnum = current_date.monthnum + 1; year = current_date.year}
        else
          {dayofmonth = 1; monthnum = 1; year = current_date.year + 1}
      in
      fill_calendar next_date
    end
  in
  fill_calendar {dayofmonth=1; monthnum=1; year=year};
  calendar
;;
(*val calgen : int -> (date, date -> weekday) Hashtbl.t = <fun>*)

(*Example - *)
let calendar_2024 = calgen 2024
(*val calendar_2024 : (date, date -> weekday) Hashtbl.t = <abstr>*)

let jan_1_2024_weekday = Hashtbl.find calendar_2024 {dayofmonth=1; monthnum=1; year=2024}
(*val jan_1_2024_weekday : date -> weekday = <fun>*)