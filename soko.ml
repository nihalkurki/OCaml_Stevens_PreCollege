


(* ******************************************** *)
(* Data type declarations *)
(* ******************************************** *)

type guess = char*int

type coordinate = int*int

type boats = { pos: coordinate list list;
               hits: coordinate list;
               misses: coordinate list;
               user_pos: coordinate list list;
               user_hits: coordinate list;
               user_misses: coordinate list;}

type direction = North | East | South | West

let user_boats = {pos = [[(2,3);(2,4);(2,5)];[(4,0);(5,0);(6,0);(7,0)];[(7,6);(7,7);(7,8)];[(0,1);(1,1)];[(9,3);(9,4);(9,5);(9,6);(9,7)]]; 
                  hits = []; 
                  misses = []; 
                  user_pos = [[(1,3);(1,4);(1,5);(1,6)];[(1,0);(2,0);(3,0);(4,0);(5,0)];[(6,2);(7,2);(8,2)];[(5,4);(5,5)];[(6,8);(7,8);(8,8)]]; 
                  user_hits = []; 
                  user_misses = []}

(* ******************************************** *)
(* Start of code that student's should write *)

(* ******************************************** *)


let string_into_int (s:string) : int option =
  match s with
  | "a" -> Some 0
  | "b" -> Some 1
  | "c" -> Some 2
  | "d" -> Some 3
  | "e" -> Some 4
  | "f" -> Some 5
  | "g" -> Some 6
  | "h" -> Some 7
  | "i" -> Some 8
  | "j" -> Some 9
  | _ -> None

let char_into_int (c:char) : int =
  match c with
  | 'a' -> 0
  | 'b' -> 1
  | 'c' -> 2
  | 'd' -> 3
  | 'e' -> 4
  | 'f' -> 5
  | 'g' -> 6
  | 'h' -> 7
  | 'i' -> 8
  | 'j' -> 9
  | _ -> failwith "choose correct guess"


(*
let boat_rand (num:int) (game:board) : coordinate list list=
  let first = (Random.int 10) in
  let second = (Random.int 10) in
  let coord = (first,second) in
  let vert_hor = (Random.int 2) in (*0 is horizontal; 1 is vertical *)
  if((vert_hor = 0 && first > 9-num) || (vert_hor = 1 && second > 9-num) || is_free_coord ) then
  (
    boat_rand num game
  )
  else
  (
    for i=1 to num do    
      coord::game.pos
      if(vert_hor = 0) then 
      (
        first = first + 1;
      )
      else
      (
        second = second + 1
      )
    done
  )


let rec randomize_board (game:board) : coordinate list list = 
  (boat_rand 2 game)::(boat_rand 3 game)::(boat_rand 3 game)::(boat_rand 4 game)::(boat_rand 5 game)

*)


let is_valid_int (n:int) : int option =
  if ((n < 11) && (n > 0)) then Some n else None
  


let next_pos ((c,y):guess) (d:direction) : coordinate =
  let num = char_into_int c
  in
  match d with
  | North -> (num,y+1)
  | East -> (num+1,y)
  | South -> (num,y-1)
  | West -> (num-1,y)
            
(*let move_worker (lev:level) (dir:direction) : level =
  { lev with pos = next_pos lev.pos dir}*)
  
(*let move_worker' lev dir =
  { pos = next_pos lev.pos dir;
    points = lev.points;
    walls = lev.walls;
    places = lev.places;
    boxes = lev.boxes }*)

let rec hits_boat (count: int) (l:coordinate list list) (c: coordinate) : (int * bool) =
  match l with
  | [[]] -> ((-1),false)
  | (h::t) when List.mem c h -> (count, true)
  | h::t -> hits_boat (count + 1) t c
  | _ -> ((-1),false)


let get_first ((x,y):(int*bool)) : int =
  x 

let get_second ((x,y):(int*bool)) : bool =
  y

let get_first_int ((x,y):(int*int)) : int =
  x 

let get_second_int ((x,y):(int*int)) : int =
  y

let rec remove_elem (l:'a list) (elem: 'a) : 'a list = 
  match l with
  | [] -> []
  | h::t when h = elem -> t
  | h::t -> h::remove_elem t elem
   
let rec remove_hit (index: int) (l:coordinate list list) (c: coordinate) : coordinate list list =
  match l with
  | [[]] -> l
  | (h::t) when index = 0 -> (remove_elem h c)::t 
  | (h::t) -> h::(remove_hit (index -1) t c)
  | _ -> l
  






let is_free_coord (c:coordinate) (boats:boats) =
     not (List.mem c boats.user_misses) && not (List.mem c boats.user_hits) && not ((get_first_int c < 0 || get_first_int c > 10)|| (get_second_int c < 0 || get_second_int c > 10))

let is_free_coord_cp (c:coordinate) (boats:boats) =
     not (List.mem c boats.misses) && not (List.mem c boats.hits) && not ((get_first_int c < 0 || get_first_int c > 10)|| (get_second_int c < 0 || get_second_int c > 10))
    
(*let rec move_box boxes before after = 
  match boxes with
  | [] -> []
  | h::t when h=before -> after::t
  | h::t -> h::move_box t before after*)
              
   


(*      
let step (lev:level) (dir:direction):level =
  let np = next_pos lev.pos dir
  in let nnp = next_pos np dir
  in if List.mem np lev.walls
  then lev
  else (if List.mem np lev.boxes
        then (if is_free_coord nnp lev
              then { (move_worker lev dir) with
                     boxes = move_box lev.boxes np nnp  }
              else lev)
        else move_worker lev dir )
  
let process_key lev key =
  match key with
  | 'w' -> step lev North
  | 'd' -> step lev East
  | 's' -> step lev South
  | 'a' -> step lev West
  | _ -> lev




*)

(* ******************************************** *)
(* End of code that students should write *)
(* ******************************************** *)

(*let erase_level {pos=(x,y); boxes= bs; places = ps}  =
  Grid.erase_coordinates (bs@ps@[(x,y)])*)

(*let draw_state {pos=(x,y); boxes= bs; places = ps}  =
  Grid.draw_string_in_box_at (x,y) "8";
  List.iter (fun c -> Grid.draw_string_in_box_at c "[]") bs;
  List.iter (fun c -> Grid.draw_string_in_box_at c "x") ps*)


(*

let winning_state {boxes= bs; places = ps} =
  List.for_all (fun c -> List.mem c ps) bs 

*)




(*

let draw_level_without_walls {pos=(x,y); points=p; walls = ws; boxes= bs; places = ps}  =
  Grid.draw_string_in_box_at (x,y) "8"; (* draw worker *)
  List.iter (fun c -> Grid.draw_string_in_box_at c "[]") bs; (* draw
                                                              * boxes *)
  List.iter (fun c -> Grid.draw_string_in_box_at c "x") ps




*)


(*let draw_level_without_walls {pos=(x,y); points=p; walls = ws; boxes= bs; places = ps}  =
  Grid.draw_string_in_box_at (x,y) "8"; (* draw worker *)
  List.iter (fun c -> Grid.draw_string_in_box_at c "[]") bs; (* draw
                                                              * boxes *)
  List.iter (fun c -> Grid.draw_string_in_box_at c "x") ps (* draw
                                                            * places *)*)

(*let draw_level (lev:level)  =
  List.iter (fun c -> Grid.color_box c Graphics.black) lev.walls; (* draw
                                                                   * walls *)
  draw_level_without_walls lev  (* draw the rest *)*)


(*

let rec main_loop (level:level) (copy:level) :unit =
  if winning_state level
  then
    begin
      (Grid.draw_string_at (200,510) "You won!");
      ignore @@ Graphics.wait_next_event [Graphics.Key_pressed]
    end
  else
    begin
      draw_level_without_walls level;
      let s = Graphics.wait_next_event [Graphics.Key_pressed]
      in match s.Graphics.key with
      | 'q' -> ()
      | 'r' ->  (Grid.draw_grid ();
                 draw_level copy;   
                 main_loop copy copy)
      | key -> (erase_level level;
                main_loop (process_key level key) copy)
    end


*)

(*
let play = 
  let alpha = read_line() and
  let num = int_of_string read_line () and
  let user_guess = (alpha * num)
  is_free_coord user_guess boats
*)
let rec printlst (lst: coordinate list) =
  match lst with
  | [] -> ()
  | (x,y)::t -> print_string "("; print_int x; print_string ", "; print_int y; print_string ") "; printlst t

let rec printlstlst (lst: coordinate list list) = 
  ignore @@ List.map (fun l -> print_string "["; printlst l; print_string "] ") lst


let rec computer_move (game:boats) : boats =
  (*print_string "misses ";
  printlst game.misses;
  print_string "hits ";
  printlst game.hits;
  print_string "user boats ";
  printlstlst game.user_pos;*)
  let first = (Random.int 10) in
  let second = (Random.int 10) in
  let guess = (first, second) in
  let isHit = get_second (hits_boat 0 game.user_pos guess) in
  let whichIndex = get_first (hits_boat 0 game.user_pos guess) in
  if is_free_coord_cp guess game then 
    (if isHit then 
      (Grid.color_box_user guess Graphics.red;
      computer_move {game with hits = guess::game.hits; user_pos = remove_hit whichIndex game.user_pos guess} )
    else 
      (Grid.color_box_user guess Graphics.blue;
      {game with misses = guess::game.misses})
    )
  else
  (computer_move game
  )

(*let rec color_user_boats_2 (lst: coordinate list) =
   match lst with
  | [] -> ()
  | h::t -> Grid.color_box_user h Graphics.black; color_user_boats_2 t*)

let rec color_user_boats (lst: coordinate list list) =
  List.iter (fun l -> color_user_boats_2 l) lst   
and color_user_boats_2 (lst: coordinate list) =
   match lst with
  | [] -> ()
  | h::t -> Grid.color_box_user h Graphics.black; color_user_boats_2 t

let rec is_empty (lst: coordinate list list) : bool =
  match lst with
  | [] -> true
  | h::t when h = [] -> is_empty t
  | h::t -> false


let end_state (game:boats) : string=
  if is_empty game.user_pos then "User Lost"
  else if is_empty game.pos then "YOU WON"
  else "continue playing"



  (*printlstlst game.pos;*)

let rec user_input (game: boats) : unit = 
  if (end_state game = "continue playing") then
    (Graphics.set_color 0xffffff;
    Graphics.fill_rect 45 495 750 100;
    Grid.draw_string_at (50,525) "Fire a Missile using the format 'char,int'";
    let input = read_line() in
    let  points = String.split_on_char ',' input in
    match string_into_int (List.nth points 0) with 
    | Some x -> let first = x in 
                  (match is_valid_int (int_of_string (List.nth points 1)) with 
                  | Some n -> let second = n - 1 in 
                    let guess = (first, second) in
                      if is_free_coord guess game then 
                        (let isHit = get_second (hits_boat 0 game.pos guess) in
                         let whichIndex = get_first (hits_boat 0 game.pos guess) in
                         if isHit 
                          then 
                          (Graphics.set_color 0xffffff;
                          Graphics.fill_rect 45 495 750 100;
                          Grid.draw_string_at (50,525) "You Hit! Fire Again";
                          Grid.color_box_cpu guess Graphics.red;
                          user_input {game with user_hits = guess::game.user_hits; pos = remove_hit (whichIndex) game.pos guess})
                         else 
                          (Grid.color_box_cpu guess Graphics.blue;
                          (let cm = computer_move {game with user_misses = guess::game.user_misses} in 
                          user_input cm))
                        )
                      else user_input game
                  | None ->  
                          Graphics.set_color 0xffffff;
                          Graphics.fill_rect 45 495 750 100;
                          Grid.draw_string_at (50,525) "Try Again. Invalid Choice";
                          user_input game)

    | None ->  
            Graphics.set_color 0xffffff;
            Graphics.fill_rect 45 495 750 100;
            Grid.draw_string_at (50,525) "Try Again. Invalid Choice";
            user_input game)
  else 
    (if end_state game = "User Lost" then 
      (Graphics.set_color 0xffffff;
      Graphics.fill_rect 45 495 750 100;
      Grid.draw_string_at (50,525) "You Lost :("; ())
    else if end_state game = "YOU WON" then 
      (Graphics.set_color 0xffffff;
      Graphics.fill_rect 45 495 750 100;
      Grid.draw_string_at (50,525) "You Won!!!"; ()))




  



(*
  let input = read_line()
  in match input with
  | ""
*)




(* Main *)

let main () = 
  Graphics.open_graph " 1200x600";
  Random.self_init ();
  (*Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";*)
  Grid.draw_string_at (50,550) "Ready to play Battleship! Your board will be on the left and a record of your missile hits will be on the right";
  Grid.draw_string_at (50,525) "Select a certain char to place that ship";
  Grid.draw_string_at (50,500) "'a' for Aircraft Carrier(5) || 'b' for Battleship(4) || 'c' for Cruiser(3) || 's' for Submarine(3) || 'd' for Destroyer(2)";
  Grid.draw_string_at (105,20) "a      b      c      d      e      f      g      h      i      j                                   a      b      c      d      e      f      g      h      i      j";
  Grid.draw_string_at (60,438) "10                                                                                                   10";
  Grid.draw_string_at (60,396) "9                                                                                                    9";
  Grid.draw_string_at (60,354) "8                                                                                                    8";
  Grid.draw_string_at (60,312) "7                                                                                                    7";
  Grid.draw_string_at (60,270) "6                                                                                                    6";
  Grid.draw_string_at (60,228) "5                                                                                                    5";
  Grid.draw_string_at (60,186) "4                                                                                                    4";
  Grid.draw_string_at (60,144) "3                                                                                                    3";
  Grid.draw_string_at (60,102) "2                                                                                                    2";
  Grid.draw_string_at (60,60) "1                                                                                                    1";
  Grid.draw_grid ();
  Grid.draw_grid_cpu ();
  (*let user_guess = (read_line() * String.split_on_char ' ' (read_line ()))*)
  (*is_free_coord (1,3) user_boats*)
  color_user_boats user_boats.user_pos;
  user_input user_boats;
  (*draw_level level_1;   
  main_loop level_1 level_1;
  Graphics.close_graph ()*)