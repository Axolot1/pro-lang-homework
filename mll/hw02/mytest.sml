fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(str, str_list) = 
  case str_list of
   [] => NONE
   | h::t => if same_string(h, str)
             then SOME t
             else 
               case all_except_option(str, t) of
                 NONE => NONE
                |SOME v => SOME(h::v)

fun get_substitutions2(strll, str) = 
  let 
    fun helper(head_lst, tail_lst) =
      case tail_lst of
        [] => head_lst
        |h::t => case all_except_option(str, h) of
                   NONE => helper(head_lst, t)
                   |SOME ls => helper(head_lst@ls, t)
  in
    helper([], strll)
  end

fun similar_names (ll_str, {first=f, middle=m, last=l}) =
  let
    fun helper(l_str) =
      case l_str of
        [] => []
        |h::t => {first=h, middle=m, last=l} :: helper(t)
  in
    {first=f, middle=m, last=l}::helper(get_substitutions2(ll_str, f))
  end

(*val test1 = all_except_option ("string", ["string"]) = SOME []
val test2 = all_except_option ("a", ["a", "b", "c"]) = SOME ["b", "c"]
val test3 = all_except_option ("a", ["b", "c"]) = NONE
val test4 = all_except_option ("a", []) = NONE

val test2_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1 ([["Fred","Fredrick"],
["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]*)

(*val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}];*)

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

all_same_color: Your function returns an incorrect result when the input list is empty. [incorrect answer]
score: Your function returns an incorrect result when the input hand is the empty list. [incorrect answer]
officiate: Your function returns an incorrect result when the move list empty. [incorrect answer]
officiate: the game starts with empty card-list. [incorrect answer]
3a tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
3b tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
