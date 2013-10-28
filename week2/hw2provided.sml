(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (x, ys) =
    let fun helper(hs) = case hs of
                             [] => []
                           | h::hs' => if same_string(x,h)
                                      then helper(hs')
                                      else h :: helper(hs')
        val res = helper(ys)
    in if res = ys then NONE else SOME res
    end

fun get_substitutions1 (xss, y) =
    case xss of
        [] => []
      | xs :: xss' => case all_except_option(y, xs) of
                          NONE => get_substitutions1(xss', y)
                        | SOME z => z @ get_substitutions1(xss', y)

fun get_substitutions2 (xss, y) =
    let fun helper (hss, acc) =
            case hss of
                [] => acc
              | hs :: hss' => case all_except_option(y, hs) of
                                  NONE => helper(hss', acc)
                                | SOME z => helper(hss', acc @ z)
    in helper(xss, [])
    end

fun similar_names (xss, {first=f, middle=m, last=l}) =
    let fun substitute (xs) =
            case xs of
                [] => []
              | x::xs' => {first=x, middle=m, last=l} :: substitute(xs')
    in
        {first=f, middle=m, last=l} :: substitute(get_substitutions2(xss, f))
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
fun card_color (c : card) =
    case c of
        (Clubs,_) => Black
      | (Spades,_) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red

fun card_value (c : card) =
    case c of
        (_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs : card list, c : card, e : exn) =
    case cs of
        [] => raise e
      | x::xs => if x=c then xs else x::remove_card(xs, c, e)

fun all_same_color (cs : card list) =
    case cs of 
        [] => true
      | _::[] => true
      | head::(neck::rest) => (card_color(head)=card_color(neck) andalso 
                               all_same_color(neck::rest))

fun sum_cards (cs : card list) =
    let fun helper (xs, acc) =
            case xs of
                [] => acc
              | x::xs => helper(xs, acc+card_value(x))
    in
        helper(cs, 0)
    end

fun score (cs : card list, goal : int) =
    let val sum = sum_cards(cs)
        val prelim_score = if sum>goal then 3*(sum-goal) else goal-sum
    in
        if all_same_color(cs) then prelim_score div 2 else prelim_score
    end

fun officiate (card_list: card list, move_list: move list, goal: int) =
    let fun helper (cur_card_list: card list,
                    cur_cards_held: card list,
                    cur_move_list: move list) =
            case cur_move_list of
                [] => score(cur_cards_held, goal)
              | Draw::ms => (case cur_card_list of
                                [] => score(cur_cards_held, goal)

                              | c::cs => if sum_cards(c::cur_cards_held)>goal
                                         then score(c::cur_cards_held, goal)
                                         else helper(cs, c::cur_cards_held, ms))
              | (Discard c)::ms => helper(cur_card_list,
                                          remove_card(cur_cards_held, c, IllegalMove),
                                          ms)
    in
        helper(card_list, [], move_list)
    end
                                          
