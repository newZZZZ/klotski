(*
	window size is 800*600
	please see readme.doc for graphics setup
	note the button_length is 183 and button_height is 96
*)
let board_start_x=20 
let board_start_y=20 
let board_length=450
let board_height=560
let block_length=100
let block_small_gap=10
let block_gap=block_small_gap+block_length (*default is 110*)
let button_length=183
let button_height=96
let button_start_x=(800-board_start_x-block_small_gap-board_length-button_length)/2+board_length+board_start_x (*default is 542*)
let button_start_y=board_start_y (*default is 20*)
let button_gap=button_height+board_start_y(*default is 116*)

type block_property = {x:int;y:int;w:int;h:int;c:int}
type direction=Up|Down|Right|Left|Fail 
let stage_one=[("D1",{x=0; y=0;w=1;h=1;c=0x5cf442 (*green*)});						
							 ("D2",{x=3; y=0;w=1;h=1;c=0x5cf442 (*green*)});
							 ("B1",{x=0; y=1;w=1;h=2;c=0xf79618 (*orange*)});
							 ("D3",{x=1; y=1;w=1;h=1;c=0x5cf442 (*green*)});
							 ("D4",{x=2; y=1;w=1;h=1;c=0x5cf442 (*green*)});
							 ("B2",{x=3; y=1;w=1;h=2;c=0xf79618 (*orange*)});
							 ("C", {x=1; y=2;w=2;h=1;c=0xff6464 (*red*)});
							 ("B3",{x=0; y=3;w=1;h=2;c=0xf79618 (*orange*)});
							 ("A", {x=1; y=3;w=2;h=2;c=0x0f64ff (*blue*)});
							 ("B4",{x=3; y=3;w=1;h=2;c=0xf79618 (*orange*)})]						
let stage_two=[("C", {x=0; y=0;w=2;h=1;c=0xff6464 (*red*)});
							 ("D1",{x=2; y=0;w=1;h=1;c=0x5cf442 (*green*)});
							 ("D2",{x=3; y=0;w=1;h=1;c=0x5cf442 (*green*)});
							 ("D3",{x=0; y=1;w=1;h=1;c=0x5cf442 (*green*)});
							 ("B1",{x=1; y=1;w=1;h=2;c=0xf79618 (*orange*)});
							 ("B2",{x=2; y=1;w=1;h=2;c=0xf79618 (*orange*)});
							 ("D4",{x=3; y=1;w=1;h=1;c=0x5cf442 (*green*)});
							 ("B3",{x=0; y=2;w=1;h=2;c=0xf79618 (*orange*)});
							 ("A", {x=1; y=3;w=2;h=2;c=0x0f64ff (*blue*)});
							 ("B4",{x=3; y=2;w=1;h=2;c=0xf79618 (*orange*)})]
let stage_three=[("B1",{x=1; y=0;w=1;h=2;c=0xf79618 (*orange*)});
								 ("B2",{x=2; y=0;w=1;h=2;c=0xf79618 (*orange*)});
								 ("B3",{x=0; y=1;w=1;h=2;c=0xf79618 (*orange*)});
								 ("D1",{x=3; y=1;w=1;h=1;c=0x5cf442 (*green*)});
								 ("C", {x=1; y=2;w=2;h=1;c=0xff6464 (*red*)});								
								 ("D2",{x=3; y=2;w=1;h=1;c=0x5cf442 (*green*)});
								 ("B4",{x=0; y=3;w=1;h=2;c=0xf79618 (*orange*)});
								 ("A", {x=1; y=3;w=2;h=2;c=0x0f64ff (*blue*)});						
								 ("D3",{x=3; y=3;w=1;h=1;c=0x5cf442 (*green*)});
								 ("D4",{x=3; y=4;w=1;h=1;c=0x5cf442 (*green*)})]
												
(*find the position via name of each block*)
let rec find_position x ls=
	match ls with  
		| (a,b):: t ->if x = a then b 
									else find_position x t
		| [] -> failwith "find_position bug" 
					
(*find the name via position each block*)				
let rec find_name (o,p) puz=
	if (o<0||o>3||p<0||p>4) then "Out of board"
	else 
		match puz with
			|(a,b)::t->if (b.x=o||(b.x+b.w-1)=o)&&(b.y=p||(b.y+b.h-1)=p) then a
								 else find_name (o,p) t
			|[]-> "E" 
(*function to translate box_number to actualy coordinates*)
let pos_trasition a b=
	(a*block_gap+block_small_gap+board_start_x,b*block_gap+block_small_gap+board_start_y)

(*function to draw each block*)
let rec drawblock ls=
	match ls with
		|(a,b)::t->
			let (posx,posy)=pos_trasition b.x b.y in
			let w=b.w*block_gap-block_small_gap in
			let h=b.h*block_gap-block_small_gap in
			Graphics.set_color b.c;
			Graphics.fill_rect posx posy w h;
			drawblock t
		|[]->()
(*draw the board*)
let draw p =
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 0 0 (Graphics.size_x()) (Graphics.size_y());
	Graphics.set_color 0x202020;
	Graphics.fill_rect board_start_x board_start_y board_length board_height;
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect (board_start_x+block_small_gap*2+block_length) board_start_y (block_length*2+block_small_gap) (block_small_gap/2);
	drawblock p;
 	Graphics.draw_image (Graphics.make_image (Button.load_array "img/Exit.png")) button_start_x button_start_y;
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/AutoSolve.png")) button_start_x (button_start_y+button_gap);
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageThree.png")) button_start_x (button_start_y+button_gap*2);
 	Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageTwo.png")) button_start_x (button_start_y+button_gap*3);
 	Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageOne.png")) button_start_x (button_start_y+button_gap*4);
	Graphics.synchronize()
(*replace *)
let rec replace acc name d ls = 
	let x= if d= Left then (-1) 
				 else if d=Right then 1
				 else 0 in
	let y= if d= Down then (-1)
				 else if d= Up then 1
				 else 0 in	
	match ls with
		|(a,b)::t->	
					begin
						if a=name then replace ((a,{x=b.x+x;y=b.y+y;w=b.w;h=b.h;c=b.c})::acc) name d t
						else replace ((a,b)::acc) name d t
					end
		|[]->List.rev acc	

let triangle_check x y (a,b)= (*check mouse click within the triangle block*)
	let (posx,posy)=pos_trasition a b in
	if x>posx&&y>posy&&x<(posx+block_length)&&y<(posy+block_length) then true
	else false
let rec double_true_help acc ls pos=
	let (x,y)=pos_trasition pos.x pos.y in
	let h=pos.h*block_gap in
	let w=pos.w*block_gap in
	Graphics.set_color 0xfcf646; (*yellow*)
		match ls with
		|hd::t->	begin
							match hd with
								|Up-> 
										Graphics.fill_poly [|(x,y+h);(x+block_length,y+h);(x+(block_length/2),y+h+block_length)|];
										double_true_help ((pos.x,pos.y+pos.h)::acc) t pos
								|Down-> Graphics.fill_poly [|(x,y-block_small_gap);(x+block_length,y-block_small_gap);(x+(block_length/2),y-block_gap)|];
										double_true_help ((pos.x,pos.y-1)::acc) t pos
								|Left-> Graphics.fill_poly [|(x-block_small_gap,y);(x-block_small_gap,y+block_length);(x-block_gap,y+(block_length/2))|];
										double_true_help ((pos.x-1,pos.y)::acc) t pos
								|Right-> Graphics.fill_poly [|(x+w,y);(x+w,y+block_length);(x+w+block_length,y+(block_length/2))|];
										double_true_help ((pos.x+pos.w,pos.y)::acc) t pos
								|_->failwith "double_true_help bug"
						end
		|[]->begin
					match List.rev acc with
						|h1::h2::t-> (h1,h2)
						|_->failwith "double_true_help acc bug"
				 end
(*function to solve double true*)
let double_true judge name pos ls = (*d1 and d2 are direction*)
	match judge with
		|d1::d2::[]->begin	
								let (p1,p2)=double_true_help [] judge pos in
								let e_t=Graphics.wait_next_event [Button_down] in 
  							Printf.printf "triangle mouse is: %d \t%d\n" e_t.mouse_x e_t.mouse_y;
								if (triangle_check e_t.mouse_x e_t.mouse_y p1) then replace [] name d1 ls  
								else if (triangle_check e_t.mouse_x e_t.mouse_y p2) then replace [] name d2 ls
								else ls
						  end
		|_->failwith "double_true bug"
(*four directions check.Note it is different from autosolve_four_direction_check*)
let main_four_direction_check x y w h ls= 
	let up_check= if (find_name (x,y+h) ls)="E"&&(find_name (x+w-1,y+h) ls)="E" then Up else Fail in 
	let down_check= if (find_name (x,y-1) ls)="E"&&(find_name (x+w-1,y-1) ls)="E" then Down else Fail in
	let right_check= if (find_name (x+w,y) ls)="E"&&(find_name (x+w,y+h-1) ls)="E" then Right else Fail in
	let left_check= if (find_name (x-1,y) ls)="E"&&(find_name (x-1,y+h-1) ls)="E" then Left else Fail in 
	[up_check;down_check;right_check;left_check]

let rec main_check_moveable i ls=
	let name = find_name i ls in
	Printf.printf "name is: %s\n" name;	
	if name="E" then ls
	else 
		begin
			let pos=find_position name ls in
			Printf.printf "pos is: %d\t%d\n" pos.x pos.y;	
			(*10 directions check*)
			let judge=List.filter (fun x -> x<>Fail) (main_four_direction_check pos.x pos.y pos.w pos.h ls) in
			if List.length judge =2 then double_true judge name pos ls
			else if List.length judge =1 then replace [] name (List.hd judge) ls
			else ls
		end
let get_box_number x y =
	if (x<(board_start_x+block_small_gap)||x>(board_start_x+block_gap*4)||y<(board_start_y+block_small_gap)||y>(board_start_y+block_gap*5)) then (5,5) (*outside*)
	else if ((x-(board_start_x-block_length))/block_gap<>(x-board_start_x-1)/block_gap)&&((y-(board_start_y-block_length))/block_gap<>(y-board_start_y-1)/block_gap) then ((x-(board_start_x-block_length))/block_gap-1,(y-(board_start_y-block_length))/block_gap-1) 
	else (5,5) (*oustide*) 
(*Set.setup*)
let rec convertlist acc ls =
  match ls with
    |(name,position)::tl-> 
      begin    
        match String.sub name 0 1 with
          |"A"->convertlist ((4*100+position.x*10+position.y)::acc) tl
          |"B"->convertlist ((3*100+position.x*10+position.y)::acc) tl
          |"C"->convertlist ((2*100+position.x*10+position.y)::acc) tl
          |"D"->convertlist ((1*100+position.x*10+position.y)::acc) tl
          |_->failwith "converlist bug"
      end
    |[]-> List.sort compare acc
module Set = Set.Make( 
  struct
    let compare a b = Pervasives.compare (convertlist [] a) (convertlist [] b)
    type t =  (string * block_property) list 
	end )
(*auto_four_direction_check. Note it is different from main_four_direction_check*)
let autosolve_four_direction_check (a,b) ls =
	let four_direction_result = main_four_direction_check b.x b.y b.w b.h ls in
	let judge=List.filter (fun x -> x<>Fail) four_direction_result in
  match judge with
    |[f;s]-> (replace [] a f ls)::(replace [] a s ls)::[]
    |[f]->(replace [] a f ls)::[]
    |[]->[]
    |_->failwith"autosolve_four_direction_check bug"
(*autosolve_checkmove*)
let rec autosolve_checkmove acc ls un_change_ls=
  match ls with
    |hd::tl->
        let result=autosolve_four_direction_check hd un_change_ls in
        if List.length result=0 then autosolve_checkmove acc tl un_change_ls
        else autosolve_checkmove (List.append result acc) tl un_change_ls 
    |[]->List.rev acc
let rec autosolve_victory ls =
  match ls with
		|h::d-> 
						let v=find_position "A" h in 
						if v.x=1&&v.y=0 then Some h
						else autosolve_victory d 
    |[]->None
(*generating new queue and globalset*)
let rec print_list ls=
  match ls with
    |(a,b)::t->	Printf.printf "%s-%d,%d \t" a b.x b.y;	
                print_list t
    |[]->() 
let rec solve globalset path ls =
  if ls =[] then []
  else 
  begin
    let intake=List.hd ls in
    let result2= List.filter (fun x->not (Set.mem x globalset)) (autosolve_checkmove [] intake intake) in
    if (autosolve_victory result2) <> None then ((intake,result2)::path) 
    else
      begin    
        let new_globalset=Set.union globalset (Set.of_list result2) in 
        let new_list=List.append (List.tl ls) result2 in
        solve (new_globalset) ((intake,result2)::path) (new_list)
      end
  end         
let rec reversetrack_help x ls=
  match ls with
    |h::d->if h=x then true
           else reversetrack_help x d 
    |[]->false
let rec reversetrack acc x ls unchange_ls=
  match ls with
    |(a,b)::d->begin
                if reversetrack_help x b then reversetrack (a::acc) a unchange_ls unchange_ls 
                else reversetrack acc x d unchange_ls  
            end
    |[]->acc
let autosolve puzzle=
  let solution_set=Set.add puzzle Set.empty in
  let finalresult=solve solution_set [] (puzzle::[]) in 
  (*
  match (List.nth (List.rev finalresult) 6) with
  |(a,b)->print_list a;
          print_endline " ";
          print_list (List.nth b 0);
          print_endline " ";
          print_list (List.nth b 1);
          print_endline " ";
          print_list (List.nth b 2);
          print_endline " ";
          print_list (List.nth b 3);
  print_endline " "
  *)
  let realpath =
    if finalresult=[] then []
    else 
    begin
     match (List.hd finalresult) with
      |(a,b)->begin
               match (autosolve_victory b) with
                |Some t->(reversetrack (a::t::[]) a finalresult finalresult) 
                |_-> failwith "() 1 bug"
            end
    end
	in
	realpath
(*
	print_int (List.length realpath);
  print_endline " ";
  print_list (List.nth realpath 0);
  print_endline " ";
  print_list (List.nth realpath 1);
  print_endline " ";
  print_list (List.nth realpath 2);
  print_endline " ";
  print_list (List.nth realpath 3);
  print_endline " ";
*)
let rec animation path=
	match path with
	|h::d-> draw h;
			Unix.sleepf 0.5;
			animation d
	|h->()
let make_new_ls x y puzzle=
	Printf.printf "mouse is: %d\t%d\n" x y;	
	if x>(board_start_x+block_gap)&&x<(board_start_x+block_gap*3+block_small_gap)&&y>(board_start_y+block_gap)&&y<(board_start_y+block_gap*4+block_small_gap)&&((x-board_start_x-1)/block_gap<>(x-(board_start_x+block_small_gap))/block_gap)&&((y-board_start_y-1)/block_gap<>(y-(board_start_y+block_small_gap))/block_gap)(*cross section*)
		then begin	
					Printf.printf "%s\n" "cross section";
					let left_down = get_box_number (x-11) (y-11) in
					let right_up = get_box_number (x+11) (y+11) in
					Printf.printf "%d\t%d\n" (fst right_up) (snd right_up);
					if (find_name left_down puzzle)=(find_name right_up puzzle) then Some (main_check_moveable left_down puzzle)
					else Some puzzle 
		   	 end    
	else if x>(board_start_x+block_gap)&&x<(board_start_x+block_gap*3+block_small_gap)&&y>(board_start_y+block_small_gap)&&y<(board_start_y+block_gap*5)&&((x-board_start_x-1)/block_gap<>(x-(board_start_x+block_small_gap))/block_gap) (*straight lines*) 
		then begin 
					Printf.printf "%s\n" "straight lines";
					let left = get_box_number (x-11) y in
					let right = get_box_number (x+11) y in
					Printf.printf "%d\t%d\n" (fst left) (snd left);
					if (find_name left puzzle = find_name right puzzle) then Some (main_check_moveable left puzzle)
					else Some puzzle 
		 		 end  			
	else if x>(board_start_x+block_small_gap)&&x<(board_start_x+block_gap*3+block_small_gap)&&y>(board_start_y+block_gap)&&y<(board_start_y+block_gap*4+block_small_gap)&&((y-board_start_y-1)/block_gap<>(y-(board_start_y+block_small_gap))/block_gap)(*horizontal lines*)
		then begin
					Printf.printf "%s\n" "horizontal lines";
					let down = get_box_number x (y-11) in
					Printf.printf "%s\n" "horizontal lines";
					let up = get_box_number x (y+11) in
					Printf.printf "%d\t%d\n" (fst down) (snd down);
					if (find_name down puzzle) = (find_name up puzzle) then Some (main_check_moveable down puzzle)
					else Some puzzle 
				 end
	else if (x>=button_start_x&&x<=(button_start_x+button_length)&&y>=(button_start_y+button_gap*4)&&y<=button_gap*5) (*Stage One*)then Some stage_one 
	else if (x>=button_start_x&&x<=(button_start_x+button_length)&&y>=(button_start_y+button_gap*3)&&y<=button_gap*4) (*Stage Two*)then Some stage_two
	else if (x>=button_start_x&&x<=(button_start_x+button_length)&&y>=(button_start_y+button_gap*2)&&y<=button_gap*3) (*Stage Three*)then Some stage_three
	else if (x>=button_start_x&&x<=(button_start_x+button_length)&&y>=(button_start_y+button_gap)&&y<=button_gap*2) (*Stage AutoSolve*)then 
																																			begin
																																				let path= autosolve puzzle in 
																																				(*print_int (List.length (path));*)
																																				animation path;
																																				Some (List.hd (List.rev path))
																																			end 
	else if (x>=button_start_x&&x<=(button_start_x+button_length)&&y>=button_start_y&&y<=button_gap)	(*Exit*) then None
	else begin
				let (a,b) = get_box_number x y in (*everything else*)
				Printf.printf "box_number is %d\t%d\n" a b;
				if a=5&&b=5 then Some puzzle 
				else Some (main_check_moveable (a,b)  puzzle)
			 end

let main_victory ls=
	let v=find_position "A" ls in 
	if v.x=1&&v.y=0 then true
	else false

let () =
	Graphics.open_graph " 800x600+100+50"; (*relate to graph_x and graph_y*)
	Graphics.set_window_title "Klotski";
	let rec loop puzzle=
		draw puzzle;
		(*check for victory for main*)
		if main_victory puzzle then
		(*if true then*) 
			begin
				Graphics.draw_image (Graphics.make_image (Button.load_array "img/Victory.png")) 100 75; 
				let e_v=Graphics.wait_next_event [Button_down] in  
				if (e_v.button) then ()
			end
		else 
			begin
				let e=Graphics.wait_next_event [Button_down] in     
				let new_puzzle=make_new_ls e.mouse_x e.mouse_y puzzle in 
					match new_puzzle with
						|None->()
						|Some new_puzzle-> loop new_puzzle
			end
	in
		loop stage_one
