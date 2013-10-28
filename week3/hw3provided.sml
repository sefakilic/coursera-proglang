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
fun only_capitals strs = List.filter (fn str => Char.isUpper(String.sub(str, 0))) strs

fun longest_string1 strs = List.foldl (fn (a,b) => if String.size a > String.size b then a else b) "" strs

fun longest_string2 strs = List.foldl (fn (a,b) => if String.size a >= String.size b then a else b) "" strs

fun longest_string_helper f strs = List.foldl (fn (a,b) => if f(String.size a, String.size b) then a else b) "" strs

val longest_string3 = longest_string_helper (fn (a,b) => a>b)

val longest_string4 = longest_string_helper (fn (a,b) => a>=b)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

(* ******************* *)

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case (f x) of
                      NONE => first_answer f xs'
                    | SOME v => v

fun all_answers f xs =
    let fun helper acc partial_xs =
            case partial_xs of
                [] => SOME acc
              | p_x::p_xs' => case (f p_x) of
                                  NONE => NONE
                                | SOME v => helper (acc @ v) p_xs'
    in
        helper [] xs
    end

(* Part 2 *)
val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) String.size

fun count_some_var (s,p) = g (fn x => 0) (fn x => if x=s then 1 else 0) p

val check_pat =
    let fun get_all_vars p =
            (* Given pattern p, return the list of all variables *)
            case p of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p, acc_vars) => (get_all_vars p) @ acc_vars) [] ps
              | _ => []
        fun has_repeats vs =
            (* Check if the list vs has any repeats *)
            case vs of
                [] => false
              | v::vs' => List.exists (fn x => x=v) vs' orelse has_repeats vs'
    in
        not o has_repeats o get_all_vars
end

fun match (va, pa) =
    case (va, pa) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x=y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps
                                 then let val x = ListPair.zip(vs,ps)
                                      in all_answers match x
                                      end
                                 else NONE
      | (Constructor (s,v), ConstructorP (s',p)) => if s=s' then match(v,p) else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
