(* 
   recursion on numbers and lists 
   23 July 2019
*)


(* ****************** *)
(* recursion on numbers *)
(* ****************** *)

(* add first n numbers *)
let rec my_sum (n:int) : int = 
  match n with
  | 0 -> 0
  | n -> my_sum (n-1) + n

(* compute factorial of a number *)
let rec fact (n:int) : int =
  match n with
  | 0 -> 1
  | n -> fact (n-1) * n

(* ****************** *)
(* recursion on lists *)
(* ****************** *)

(* length of a list *)
let rec length (l:'a list) : int =
  match l with
  | [] -> 0
  | h::t -> 1 + length t

(* sum of all the numbers in a list *)
let rec sum (l:int list) : int =
  match l with
  | [] -> 0
  | h::t -> h + sum t

(* product of all the numbers in a list *)
let rec prod (l:int list) : int =
  match l with
  | [] -> 1
  | h::t -> h * prod t
              
(* sum of all the positive numbers in a list *)
let rec sum_positive (l:int list) : int =
  match l with
  | [] -> 0
  | h::t ->
    if h>0
    then h + sum_positive t
    else sum_positive t

(* variation of previous exercise using when clauses *)
let rec sum_positive' l =
  match l with
  | [] -> 0
  | h::t when h>0 -> h + sum_positive t
  | h::t -> sum_positive t

(* add 1 to each number in a list of numbers *)
let rec bump l =
  match l with
  | [] -> []
  | h::t -> (h+1) :: bump t


(* duplicate each item in a list *)
let rec stutter l =
  match l with
  | [] -> []
  | h::t -> h::h::stutter t


  (* remove all adjacent items in a list that are identical *)
  
(*
let rec remove_adjacent l =
  match l with
  | [] -> []
  | [h] -> ???
  | h1::h2::t -> ???
*)    

(* More exercises on recursion *)


let rec mem (e:'a) (l:'a list) : bool =
  match l with
  | [] -> false
  | h::t ->
    if e=h
    then true
    else mem e t

let rec mem' (e:'a) (l:'a list) : bool =
  match l with
  | [] -> false
  | h::t when e=h -> true
  | h::t -> mem' e t

let rec mem'' (e:'a) (l:'a list) : bool =
  match l with
  | [] -> false
  | h::t -> h=e || mem'' e t

let rec has_dupl (l:'a list) : bool =
  match l with
  | [] -> false
  | h::t when mem h t -> true
  | h::t -> has_dupl t

let rec has_dupl' (l:'a list) : bool =
  match l with
  | [] -> false
  | h::t -> mem h t || has_dupl' t 

let rec remove_dupl (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t ->
    if mem h t
    then remove_dupl t
    else h::remove_dupl t

let rec remove_dupl' (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t when mem h t-> remove_dupl t
  | h::t -> h::remove_dupl t

let rec take (n:int) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t ->
    if n=0
    then []
    else h::take (n-1) t

let rec take' (n:int) (l:'a list) : 'a list =
  match (n,l) with
  | 0,l -> []
  | n,[] -> []
  | n,h::t -> h::take' (n-1) t   (* n>0 *)

let rec drop (n:int) (l:'a list) : 'a list =
  match (n,l) with
  | 0,l -> l
  | n,[] -> []
  | n,h::t -> drop (n-1) t

let split (n:int) (l:'a list) : ('a list * 'a list) =
  (take n l, drop n l)


let rec flatten (l:'a list list) : 'a list =
  match l with
  | [] -> []
  | h::t -> h @ flatten t

let rec add_h_to (e:'a) (l:'a list list) : 'a list list =
  match l with
  | [] -> []
  | h::t -> (e::h) :: add_h_to e t
              
let rec sublists (l:'a list) : 'a list list =
  match l with
  | [] ->  [[]]
  | h::t ->
    let sub_of_tail = sublists t
    in sub_of_tail @ add_h_to h sub_of_tail


let successor i = i+1
let upper c = Char.uppercase_ascii c
let is_zero i = (i=0)
                
let rec succl (l:int list) : int list =
  match l with
  | [] -> []
  | h::t -> successor h :: succl t 
    
let rec to_upperl (l:char list) : char list = 
  match l with
  | [] -> []
  | h::t -> upper h :: to_upperl t 

let rec are_zeros (l:int list) : bool list =
  match l with
  | [] -> []
  | h::t -> is_zero h :: are_zeros t

let rec map (f: 'a -> 'b) (l:'a list) : 'b list =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let succl'= map successor
let to_upperl' = map upper
let are_zeros' = map is_zero
    
    (* motivating filter *)

let is_positive i = i>0
let is_uppercase c = c = Char.uppercase_ascii c
let is_nonempty l = l!=[]
                       
let rec greater_than_zero (l:int list) : int list =
  match l with
  | [] -> []
  | h::t ->
    if is_positive h
    then h :: greater_than_zero t
    else greater_than_zero t

let rec uppercase (l:char list) : char list =
  match l with
  | [] -> []
  | h::t ->
    if is_uppercase h
    then h:: uppercase t
    else uppercase t
    
let rec non_empty (l:'a list list) :'a list list =
  match l with
  | [] -> []
  | h::t ->
    if is_nonempty h
    then h :: non_empty t
    else non_empty t

let rec filter (p:'a -> bool) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | h::t ->
    if p h
    then h:: filter p t
    else filter p t

let greater_than_zero' = filter is_positive
let uppercase'  = filter is_uppercase
let non_empty' = filter is_nonempty
    
(* motivating fold *)

let rec sum_list (l:int list) : int =
  match l with
  | [] -> 0
  | h::t -> h + sum_list t
              
let rec and_list (l: bool list) : bool = 
  match l with
  | [] -> true
  | h::t -> h && and_list t

let rec flatten (l:'a list list) : 'a list =
  match l with
  | [] -> []
  | h::t -> h @ flatten t

let rec foldr (n:'b) (f:'a -> 'b -> 'b) (l:'a list) : 'b =
  match l with
  | [] -> n
  | h::t -> f h (foldr n f t)

let sum_list' = foldr 0 (+)
let and_list' = foldr true (&&)
let flatten'= foldr [] (@)
    
(* playing with maps and folds *)

let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]

let invert_instruction (i:int) : int =
  match i with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | _ -> failwith "invalid instruction"
    
let mirror_image (p:int list) : int list = List.map invert_instruction p

let rotate90_instruction i =
  match i with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> failwith "invalid instruction"


let rotate_90 (p:int list) : int list = List.map rotate90_instruction p

let rec repeat (n:int) (e:'a) : 'a list =
  match n with
  | 0 -> []
  | n -> e :: repeat (n-1) e


let rec pantograph (n:int) (p:int list) : int list =
  match p with
  | [] -> []
  | 0::t -> 0 :: pantograph n t
  | 1::t -> 1 :: pantograph n t
  | h::t -> repeat n h @ pantograph n t

let pantograph' n p =
      List.flatten @@ List.map (fun i -> if i=0||i=1 then [i] else repeat n i) p
      
let pantograph'' n p =
    foldr [] (fun i l -> if i=0||i=1 then i::l else repeat n i @ l) p

let next ((x,y): int*int) (i:int) : int*int =
  match i with
  | 0 -> (x,y)
  | 1 -> (x,y)
  | 2 -> (x,y+1)
  | 3 -> (x+1,y)
  | 4 -> (x,y-1)
  | 5 -> (x-1,y)
  | _ -> failwith "invalid instruction"
  
let rec coverage (coord:int*int) (p:int list) :(int*int) list =
  match p with
  | [] -> [coord]
  | h::t -> coord :: coverage (next coord h) t

let rec compress (r:int) (p:int list) : (int*int) list =
  match p with
  | [] -> []
  | [i] -> [(i,r+1)]
  | h1::h2::t -> 
    if h1=h2
    then compress (r+1) (h2::t)
    else (h1,r+1) :: compress 0 (h2::t)

let rec compress' (r:int) (p:int list) : (int*int) list =
  match p with
  | [] -> []
  | [i] -> [(i,r+1)]
  | h1::h2::t when h1=h2 -> compress' (r+1) (h2::t)
  | h1::h2::t  -> (h1,r+1) :: compress' 0 (h2::t)

let uncompress (l:(int*int) list) : int list =
  List.flatten @@ List.map  (fun (i,r) -> repeat r i) l


(* records *)

type student = {cwid:int; name:string; age:int; friends:int list}
               
let school =
  [
    {cwid = 1; name = "Tom"; age = 20; friends = [2;4]};
    {cwid = 2; name = "Anne"; age = 21; friends = [1;3;5;7]};
    {cwid = 3; name = "Anthony"; age = 22; friends = [1;2]};
    {cwid = 4; name = "Sue"; age = 23; friends = [5;6;7]};
    {cwid = 5; name = "Costas"; age = 24; friends = [1;7]};
    {cwid = 6; name = "Brain"; age = 25; friends = [3;4;5]};
    {cwid = 7; name = "Dante"; age = 26; friends = [1;4;5;7]}
  ]

let rec friends_of (s:student list) (cwid:int) : string list =
  failwith "complete"
    
  

let rec most_popular (s:student list) : string list =
  failwith "complete"

    
(* algebraic data types *)
