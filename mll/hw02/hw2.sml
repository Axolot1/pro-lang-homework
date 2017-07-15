
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
                  
fun get_substitutions1(losl, str) = 
  case losl of
    [] => []
    |h::t => case all_except_option(str, h) of
               NONE => get_substitutions1(t, str)
               |SOME sub => sub @ get_substitutions1(t, str)

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



datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(suit, _) =
  case suit of
    Clubs => Black
    |Spades => Black
    |Diamonds => Red
    |Hearts => Red

fun card_value(_, rank) =
  case rank of
    Num n => n
    | Ace => 11
    | Jack => 10
    | Queen => 10
    | King => 10

fun remove_card(cs, c, e) =
  case cs of
    [] => raise e
    |h::t => if h = c
             then t
             else h :: remove_card(t, c, e)


fun all_same_color (card_list) =
  case card_list of
    [] => true
    | _::[] => true
    | f::(s::t) => card_color(f) = card_color(s) andalso all_same_color(s::t)


fun sum_cards (card_list) =
  let
    fun helper(sum, list) =
      case list of
        [] => sum
        | h::t => helper(sum + card_value(h), t)
  in
    helper(0, card_list)
  end


fun score(held_cards, goal) = 
  let
    val sum = sum_cards(held_cards)
    val is_same_color = all_same_color(held_cards)
    val preliminary_score = if sum > goal then 3*(sum-goal) else goal - sum
  in
    if is_same_color
    then preliminary_score div 2
    else preliminary_score
  end


fun officiate(card_list, move_list, goal) = 
  let
    fun helper(card_list, move_list, held_list) =
      if sum_cards(held_list) > goal
      then score(held_list, goal)
      else 
        case move_list of
          [] => score(held_list, goal)
          | mh::mt => (case mh of
                        Draw => (case card_list of
                                  [] => score(held_list, goal)
                                  | ch::ct => helper(ct, mt, ch::held_list))
                        | Discard dc => helper(card_list, mt, remove_card(held_list, dc, IllegalMove)))
  in
    helper(card_list, move_list, [])
  end
