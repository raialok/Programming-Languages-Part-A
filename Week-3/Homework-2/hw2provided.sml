(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1 (a) *)
fun all_except_option (toFind, lst) =
	let fun contains (toFind, lst) =
		case lst of
			[] => false
			| hd :: tl => if same_string (hd, toFind)
						  then true
						  else contains (toFind, tl)
	fun remove_element (toRemove, lst) =
		case lst of
			[] => []
			| hd :: tl => if same_string (hd, toRemove)
						  then remove_element (toRemove, tl)
						  else hd :: remove_element (toRemove, tl)
	in
		case contains (toFind, lst) of
			false => NONE
			| true => SOME (remove_element (toFind, lst))
	end

(* 1 (b) *)
fun get_substitutions1 (lstOflst: string list list, s: string) =
	case lstOflst of
		[] => []
		| hd :: tl =>
			case all_except_option (s, hd) of
				NONE => [] @ get_substitutions1 (tl, s)
				| SOME e => e @ get_substitutions1 (tl, s)

(* 1 (c) *)
fun get_substitutions2 (lstOflst: string list list, s: string) =
	let fun substitution_helper (lol: string list list, s: string, result:
		string list) =
		case lol of
			[] => result
			| hd :: tl => case all_except_option (s, hd) of
						  NONE => substitution_helper (tl, s, result)
						  | SOME e => substitution_helper (tl, s, result @ e)
	in
		substitution_helper (lstOflst, s, [])
	end

(* 1 (d) *)
fun similar_names (lstOflst: string list list, {first:string, middle:string, 
	last:string}) =
	let val names = get_substitutions2 (lstOflst, first)
		fun similar_names_helper (list_names, result) =
			case list_names of
				[] => result
				| hd :: tl => similar_names_helper (tl, result @ [{first = hd,
					last = last, middle = middle}])
	in
		similar_names_helper (names, [{first = first, middle = middle,
			last = last}])
	end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (aCard: card) =
	let val (s: suit, r: rank) = aCard
	in
		case s of
			Spades => Black
			| Clubs => Black
			| _ => Red
	end





















