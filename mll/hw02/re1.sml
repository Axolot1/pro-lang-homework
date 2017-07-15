(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s,lst)=
case lst of
  []=> NONE
 |l::lst'=> if same_string(s,l)
	    then SOME(lst')
	    else case  all_except_option(s,lst') of
		     NONE=>NONE
			| SOME y =>SOME (l::y) 
				   
fun get_substitutions1 (list_list_str,str)=
  case list_list_str of
      []=>[]
    | h_lls::lls' =>case all_except_option(str,h_lls) of
			SOME y=>y @ get_substitutions1(lls',str)
			     | NONE => get_substitutions1(lls',str)
			    

fun get_substitutions2 (list_list_str,str)=
  let fun aux(lst,acc)=
	case lst of
	    []=>acc
	  | x::xs =>case all_except_option(str,x) of
			NONE=>aux(xs,acc)
		      | SOME z =>aux(xs,acc@z)
  in
      aux(list_list_str,[])
  end
      
fun similar_names (lst:string list list,name:{first:string,last:string,middle:string})=
  let
      val {first=f,last=l,middle=m}=name
      fun helper(xs)=
	case xs of
	    []=>[]
	     | x::xs' => {first=x,last=l,middle=m}::helper(xs')
  in
     name:: helper(get_substitutions2(lst,f))
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
fun card_color card=
  case card of
      (Clubs,_)=>Black
    |(Spades,_) =>Black
    | (_,_) =>Red
      
fun card_value the_card =
  case the_card of
      (_,Ace)=>11
    | (_,Num n) =>n
    | (_,_) =>10

fun remove_card (cs,c,e)=		  
  case cs of
      []=>raise e
    | x::xs =>case x=c of
		  true =>xs
		| false =>x::remove_card(xs,c,e)

fun all_same_color cs	=
  case cs of
      []=>true
    | x::[] =>true
    | x::y::cs =>case card_color x=card_color y of
		     false=>false
		   | ture =>all_same_color(y::cs)

fun sum_cards cs=
  let fun sum (lst,n)=
	case lst of
	    []=>n
	  | x::xs =>sum(xs,card_value(x)+n)
  in
      sum(cs,0)
  end

fun score  (cs,goal)=
  let
      val sum=sum_cards(cs)
      val res=if sum>goal then 3*(sum-goal) else (goal-sum)
      val score=if all_same_color cs then res div 2 else res
  in
      score
  end

fun  officiate (cs,ms,goal)=
  let fun get_state (c::cs',m,mycards)=
	case m of
	    Discard c=>(c::cs',remove_card(mycards,c,IllegalMove))
	  | Draw => (cs',c::mycards)
      fun play (cs,mycards,ms,acc)=
	case (cs,mycards,ms,acc) of
	    (_,_,[],_)=>acc
	  | ([],_,_,_) =>acc
	  | (cs,mycards,m::ms',acc) =>case sum_cards(mycards)>goal of
					  true =>score(mycards,goal)
					| _ =>let
					    val (new_cs,new_mycards)=get_state (cs,m,mycards)
					in
					    play(new_cs,new_mycards,ms',score(new_mycards,goal)+acc)
					end
  in
      play(cs,[],ms,0)
  end

    
	  