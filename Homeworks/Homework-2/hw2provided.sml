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

(* Solutions for problem 2 *)

(* 2 (a) *)
fun card_color (aCard: card) =
	let val (s: suit, r: rank) = aCard
	in
		case s of
			Spades => Black
			| Clubs => Black
			| _ => Red
	end

(* 2 (b) *)
fun card_value (aCard: card) =
	let val (s: suit, r: rank) = aCard 
	in
		case r of
			Ace => 11
			| Num i => i
			| _ => 10
	end

(* 2 (c) *)
fun remove_card (listOfCards, c, e) =
	case listOfCards of
		[] => raise e
		| [x] => if x = c then [] else remove_card ([], c, e)
		| hd:: tl => if hd = c then tl
					 else hd :: remove_card (tl, c, e)

(* 2 (d) *)
fun all_same_color (listOfCards) =
	case listOfCards of
		[] => true
		| [e] => true
		| hd :: e :: tl => if card_color (hd) = card_color (e)
						   then all_same_color (e :: tl)
						   else false

(* 2 (e) *)
fun sum_cards (listOfCards) =
	let 
		fun sum_cards_helper (lol, result) =
			case lol of
				[] => result
				| hd :: tl => sum_cards_helper (tl, card_value (hd) + result)
	in
		sum_cards_helper (listOfCards, 0)
	end

(* 2 (f) *)
fun score (listOfCards, target) =
	let val total = sum_cards (listOfCards)
	in
		let val preliminary_score = if total > target 
									then 3 * (total - target)
									else (target - total)
		in
			if all_same_color (listOfCards)
			then preliminary_score div 2
			else preliminary_score
		end
	end








