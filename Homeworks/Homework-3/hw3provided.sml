(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(* 1 *)

fun only_capitals lst = 
	List.filter (fn x => Char.isUpper (String.sub (x, 0))) lst

(* 2 *)
fun longest_string1 lst =
	foldl (fn (x, y) => if String.size y >= String.size x then y else x)
		""
		lst

(* 3 *)
fun longest_string2 lst =
	foldl (fn (x, y) => if String.size x >= String.size y then x else y)
		""
		lst

(* 4 *)
fun longest_string_helper f lst str = foldl  f str lst

fun longest_string3 lst = longest_string_helper 
	(fn (x, y) => if String.size y >= String.size x then y else x)
	lst
	""

fun longest_string4 lst = longest_string_helper 
	(fn (x, y) => if String.size x >= String.size y then x else y)
	lst
	""

(* 5 *)
val  longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f lst =
	if null lst then raise NoAnswer
	else
		case f (hd lst) of
			SOME v => v
			| NONE => first_answer f (tl lst)

(* 8 *)
fun all_answers f lst =
	let fun helper lst result =
		if null lst then result
		else
			let val eval = f (hd lst) 
			in 
				case eval of
				SOME n => helper (tl lst) (n @ result) 
				| NONE => raise NoAnswer
			end
	in 
		SOME (helper lst [])
		handle NoAnswer => NONE
	end

(* 9 *)

(* 9a *)
val count_wildcards = g (fn () => 1) (fn x => 0)

(* 9b *)
fun count_wild_and_variable_lengths lst = count_wildcards lst + g (fn () => 0) 
	(fn x => 1) lst

(* 9c *)
fun count_some_var (str, pat) = g (fn () => 0) (fn x => if x = str then 1 else
	0) pat

(* 10 *)







(* Tests *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test51 = longest_capitalized ["a", "b", "c"] = ""

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1



