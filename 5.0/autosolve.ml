type block_position = {x : int; y : int}
(*convert list into an integer list*)
let rec convertlist acc ls =
  match ls with
    |(name,position)::tl-> 
      begin    
      match String.sub name 0 1 with
        |"A"->convertlist (4::acc) tl
        |"B"->convertlist (2::acc) tl
        |"C"->convertlist (3::acc) tl
        |"D"->convertlist (1::acc) tl
        |"E"->convertlist (0::acc) tl
      end
    |[]-> List.rev acc
(*compare two lists*)
let rec compare ls1 ls2 = 
  match ls1 with 
    |h1::t1-> 
      begin
      match ls2 with
        |h2::t2->if h1=h2 then compare t1 t2 
                else false
        |[]->false
      end
    |[]->true
(*make sure ls2 exists in ls1*)
let rec exist ls1 b=
  match ls1 with
  |h::t->if (compare (convertlist [] h) (convertlist [] b)) then true (*exist*)
         else exist t b
  |[]->false 

let rec swap acc p1 s1 p2 s2 ls = 
	match ls with
		|(a,b)::t->	
					begin
						if b.x=p1.x&&b.y=p1.y then swap ((s2,b)::acc) p1 s1 p2 s2 t
						else if b.x=p2.x&&b.y=p2.y then swap ((s1,b)::acc) p1 s1 p2 s2 t
						else swap ((a,b)::acc) p1 s1 p2 s2 t
					end
    |[]->List.rev acc
(*find name*)
let rec find_position x ls=
	match ls with  
	| [] -> {x=5; y=5} (*bug*)
	| (a,b):: t ->if x = a then b 
								else find_position x t
(*find the name via position each block*)				
let rec findname p puz=
	match puz with
	|(a,b)::t->if b.x=p.x&&b.y=p.y then a 
						 else findname p t
  |[]-> "NotFound"
  
let checkup e1 e2 ls= 
  let contentup = findname {x=e1.x;y=e1.y+1} ls in
  match contentup with
    |"D1"|"D2"|"D3"|"D4"->Some (swap [] e1 "E1" {x=e1.x;y=e1.y+1} contentup ls)
    |"B1"|"B2"|"B3"|"B4"->Some (swap [] e1 "E1" {x=e1.x;y=e1.y+2} (findname {x=e1.x;y=e1.y+2} ls) ls)
    |"A"->if ((e1.y=e2.y&&e1.x+1=e2.x)||(e1.y=e2.y&&e1.x-1=e2.x))&&((findname {x=e2.x;y=e2.y+1} ls)=contentup) then
            begin
              let temp = swap [] e1 "E1" {x=e1.x;y=e1.y+2} (findname {x=e1.x;y=e1.y+2} ls) ls in 
              Some (swap [] e2 "E2" {x=e2.x;y=e2.y+2} (findname {x=e2.x;y=e2.y+2} ls) temp)
            end
          else None
    |"C"->if ((e1.y=e2.y&&e1.x+1=e2.x)||(e1.y=e2.y&&e1.x-1=e2.x))&&((findname {x=e2.x;y=e2.y+1} ls)=contentup) then
            begin
              let temp = swap [] e1 "E1" {x=e1.x;y=e1.y+1} contentup ls in
              Some (swap [] e2 "E2" {x=e2.x;y=e2.y+1} (findname {x=e2.x;y=e2.y+1} ls) temp)
            end
          else None
    |_->None

let checkdown e1 e2 ls=
  let contentdown = findname {x=e1.x;y=e1.y-1} ls in
  match contentdown with
    |"D1"|"D2"|"D3"|"D4"->Some (swap [] e1 "E1" {x=e1.x;y=e1.y-1} contentdown ls)
    |"B1"|"B2"|"B3"|"B4"->Some (swap [] e1 "E1" {x=e1.x;y=e1.y-2} (findname {x=e1.x;y=e1.y-2} ls) ls)
    |"A"->if ((e1.y=e2.y&&e1.x+1=e2.x)||(e1.y=e2.y&&e1.x-1=e2.x))&&((findname {x=e2.x;y=e2.y-1} ls)=contentdown) then
            begin
              let temp = swap [] e1 "E1" {x=e1.x;y=e1.y-2} (findname {x=e1.x;y=e1.y-2} ls) ls in 
              Some (swap [] e2 "E2" {x=e2.x;y=e2.y-2} (findname {x=e2.x;y=e2.y-2} ls) temp)
            end
          else None
    |"C"->if ((e1.y=e2.y&&e1.x+1=e2.x)||(e1.y=e2.y&&e1.x-1=e2.x))&&((findname {x=e2.x;y=e2.y-1} ls)=contentdown) then
            begin
              let temp = swap [] e1 "E1" {x=e1.x;y=e1.y-1} contentdown ls in
              Some (swap [] e2 "E2" {x=e2.x;y=e2.y-1} (findname {x=e2.x;y=e2.y-1} ls) temp)
            end
           else None
    |_->None

let checkright e1 e2 ls=
  let contentright = findname {x=e1.x+1;y=e1.y} ls in
  match contentright with
    |"D1"|"D2"|"D3"|"D4"->Some (swap [] e1 "E1" {x=e1.x+1;y=e1.y} contentright ls)
    |"C"->Some (swap [] e1 "E1" {x=e1.x+2;y=e1.y} (findname {x=e1.x+2;y=e1.y} ls) ls)
    |"B1"|"B2"|"B3"|"B4"->if ((e1.y+1=e2.y&&e1.x=e2.x)||(e1.y-1=e2.y&&e1.x=e2.x))&&((findname {x=e2.x+1;y=e2.y} ls)=contentright) then
                            begin
                              let temp = swap [] e1 "E1" {x=e1.x+1;y=e1.y} contentright ls in
                              Some (swap [] e2 "E2" {x=e2.x+1;y=e2.y} (findname {x=e2.x+1;y=e2.y} ls) temp)
                            end
                          else None
    |"A"->if ((e1.y+1=e2.y&&e1.x=e2.x)||(e1.y-1=e2.y&&e1.x=e2.x))&&((findname {x=e2.x+1;y=e2.y} ls)=contentright) then
            begin
              let temp = swap [] e1 "E1" {x=e1.x+2;y=e1.y} (findname {x=e1.x+2;y=e1.y} ls) ls in 
              Some (swap [] e2 "E2" {x=e2.x+2;y=e2.y} (findname {x=e2.x+2;y=e2.y} ls) temp)
            end
          else None
    |_->None

let checkleft e1 e2 ls=
  let contentleft = findname {x=e1.x-1;y=e1.y} ls in
  match contentleft with
    |"D1"|"D2"|"D3"|"D4"->Some (swap [] e1 "E1" {x=e1.x-1;y=e1.y} contentleft ls)
    |"C"->Some (swap [] e1 "E1" {x=e1.x-2;y=e1.y} (findname {x=e1.x-2;y=e1.y} ls) ls)
    |"B1"|"B2"|"B3"|"B4"->if ((e1.y+1=e2.y&&e1.x=e2.x)||(e1.y-1=e2.y&&e1.x=e2.x))&&((findname {x=e2.x-1;y=e2.y} ls)=contentleft) then
                            begin
                              let temp = swap [] e1 "E1" {x=e1.x+1;y=e1.y} contentleft ls in
                              Some (swap [] e2 "E2" {x=e2.x+1;y=e2.y} (findname {x=e2.x+1;y=e2.y} ls) temp)
                            end
                          else None
    |"A"->if ((e1.y+1=e2.y&&e1.x=e2.x)||(e1.y-1=e2.y&&e1.x=e2.x))&&((findname {x=e2.x-1;y=e2.y} ls)=contentleft) then
            begin
              let temp = swap [] e1 "E1" {x=e1.x-2;y=e1.y} (findname {x=e1.x-2;y=e1.y} ls) ls in 
              Some (swap [] e2 "E2" {x=e2.x-2;y=e2.y} (findname {x=e2.x-2;y=e2.y} ls) temp)
            end
          else None
    |_->None
(*filterour None and repitivties*)
let rec filter_main gacc acc ls=
  match ls with
    |(Some h)::t->if (exist acc h)||(exist gacc h) then filter_main gacc acc t 
                  else filter_main gacc (h::acc) t
    |None::t->filter_main gacc acc t
    |[]->List.rev acc

let rec victory ls=
  match ls with
  |a::b-> begin 
	         if (find_name {x=1;y=0} ls = "A")&&(find_name {x=2;y=0} ls = "A")&&
             (find_name {x=1;y=1} ls = "A")&&(find_name {x=2;y=1} ls = "A") then true		
           else 
             victory b
          end
  |[]-> false
  
(*check can be transform into what ls*)
let rec checkmove gacc pathacc ls=
  match ls with 
  |h::t-> begin
            let e1=find_position "E1" h in
            let e2=find_position "E2" h in
            let e1_up=checkup e1 e2 h in
            let e1_down=checkdown e1 e2 h in
            let e1_right=checkright e1 e2 h in
            let e1_left=checkleft e1 e2 h in
            let e2_up=checkup e2 e1 h in
            let e2_down=checkdown e2 e1 h in
            let e2_right=checkright e2 e1 h in
            let e2_left=checkleft e2 e1 h in
            (*filterout None, repetitive things and globalacc*)
            let result=filter_main gacc [] [e1_up;e1_down;e1_right;e1_left;e2_up;e2_down;e2_right;e2_left;]
            (*check victory*)
            if victory result then pathacc
            else begin

                 end          
          end
  |[]->print_string "No Solution"
  
let rec print_list ls=
  match ls with
    |(a,b)::t-> print_string a;
                print_list t
    |[]->()

let () =
  let stage_one=
   [("E1",{x=0; y=0});
    ("D1",{x=1; y=0});
    ("D2",{x=2; y=0});
    ("E2",{x=3; y=0});
    ("B1",{x=0; y=1});
    ("A",{x=1; y=1});
    ("A",{x=2; y=1});
    ("B2",{x=3; y=1});
    ("B1",{x=0; y=2});
    ("A", {x=1; y=2});
    ("A",{x=2; y=2});
    ("B2",{x=3; y=2});
    ("B3",{x=0; y=3});
    ("C", {x=1; y=3});
    ("C",{x=2; y=3});
    ("B4",{x=3; y=3});
    ("B3",{x=0; y=4});
    ("D3",{x=1; y=4});
    ("D4",{x=2; y=4}); 
    ("B4",{x=3; y=4});] in
  
  let result=checkmove (stage_one::[]) (stage_one::[]) stage_one in
  print_int (List.length result);
  print_endline " ";
  print_list (List.nth result 0);
  print_endline " ";
  print_list (List.nth result 1);
  print_endline " ";
  print_list (List.nth result 2);
  print_endline " ";
  print_list (List.nth result 3)

    


