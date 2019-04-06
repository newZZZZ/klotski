type block_property = {x:int;y:int;w:int;h:int;c:int}
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
							 ("B3",{x=0; y=2;w=1;h=1;c=0x5cf442 (*green*)});
							 ("A", {x=1; y=3;w=2;h=2;c=0x0f64ff (*blue*)});
							 ("B4",{x=3; y=3;w=1;h=2;c=0xf79618 (*orange*)})]
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
	match puz with
	|(a,b)::t->if (b.x=o||(b.x+b.w-1)=o)&&(b.y=p||(b.y+b.h-1)=p) then a
						 else find_name (o,p) t
	|[]-> "E" 
	
(*function to draw each block*)
let rec drawblock ls=
	match ls with
		|(a,b)::t->
			let posx=b.x*110+10+20 in 
			let posy=b.y*110+10+20 in
			let w=b.w*110-10 in
			let h=b.h*110-10 in
			Graphics.set_color b.c;
			Graphics.fill_rect posx posy w h;
			drawblock t
		|[]->()

let draw p =
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 0 0 (Graphics.size_x()) (Graphics.size_y());
	Graphics.set_color 0x202020;
	Graphics.fill_rect 20 20 450 560;
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 140 20 210 5;
	drawblock p;
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/Exit.png")) 543 20;
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/AutoSolve.png")) 543 136;
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageThree.png")) 543 252;
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageTwo.png")) 543 368;
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageOne.png")) 543 484;
	Graphics.synchronize()

let rec replace acc name (d::[]) ls = 
	let x= if d= "left" then (-1) 
				 else if d="right" then 1
				 else 0 in
	let y= if d= "down" then (-1)
				 else if d="up" then 1
				 else 0 in	
	match ls with
		|(a,b)::t->	
					begin
						if a=name then replace ((a,{x=b.x+x;y=b.y+y;w=b.w;h=b.h;c=b.c})::acc) name (d::[]) t
						else replace ((a,b)::acc) name (d::[]) t
					end
		|[]->List.rev acc

let triangle_check x y (a,b) d= (*check mouse click within the triangle block*)
	let posx=a*110+10+20 in 
	let posy=b*110+10+20 in
	if x>posx&&y>posy&&x<(posx+100)&&y<(posy+100) then true
	else false
let rec double_true_help acc ls pos=
	let x=pos.x*110+10+20 in 
	let y=pos.y*110+10+20 in
	Graphics.set_color 0xfcf646; (*yellow*)
	match ls with
		|h::t->	begin
							match h with
								|"up"-> Graphics.fill_poly [|(x,y);(x+100,y);(x+50,y+100)|];
										double_true_help ((pos.x,pos.y+pos.h)::acc) t pos
								|"down"-> Graphics.fill_poly [|(x,y+100);(x+100,y+100);(x+50,y)|];
										double_true_help ((pos.x,pos.y-1)::acc) t pos
								|"left"-> Graphics.fill_poly [|(x+100,y);(x+100,y+100);(x,y+50)|];
										double_true_help ((pos.x-1,pos.y)::acc) t pos
								|"right"-> Graphics.fill_poly [|(x,y);(x,y+100);(x+100,y+50)|];
										double_true_help ((pos.x+pos.w,pos.y)::acc) t pos
						end
		|[]->acc
(*function to solve double true*)
let double_true [d1;d2] name pos ls = (*d1 and d2 are direction*)
	let [p1;p2]=double_true_help [] [d1;d2] pos in
	let e_t=Graphics.wait_next_event [Button_down] in     
  Printf.printf "triangle mouse is: %d \t%d\n" e_t.mouse_x e_t.mouse_y;
	if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1) then replace [] name (d1::[]) ls  
	else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2) then replace [] name (d2::[]) ls
	else ls
(*four directions check*)
let four_direction_check x y w h ls= 
	let up_check= if (find_name (x,y+h) ls)="E"&&(find_name (x+w-1,y+h) ls)="E" then "up" else "f" in 
	let down_check= if (find_name (x,y-h) ls)="E"&&(find_name (x+w-1,y-h) ls)="E" then "down" else "f" in
	let right_check= if (find_name (x+w,y) ls)="E"&&(find_name (x+w,y+h-1) ls)="E" then "right" else "f" in
	let left_check= if (find_name (x-w,y) ls)="E"&&(find_name (x-w,y+h-1) ls)="E" then "left" else "f" in 
	[up_check;down_check;right_check;left_check]

let rec check_moveable i ls=
	let name = find_name i ls in
	let pos=find_position name ls in
	(*10 directions check*)
	let judge=List.filter (fun x -> x<>"f") (four_direction_check pos.x pos.y pos.w pos.h ls) in
	if List.length judge =2 then double_true judge name pos ls
	else if List.length judge =1 then replace [] name judge ls
	else ls

let get_box_number x y =
	if (x<30||x>460||y<30||y>570) then (5,5) (*outside*)
	else if ((x-30)/110<>(x-131)/110)&&(y-30/110<>(y-131)/110) then ((x-30)/110,(y-30)/110) 
	else (5,5) (*oustide*) 

let make_new_ls x y puzzle=
	Printf.printf "mouse is: %d\t%d\n" x y;	
	if (x>130&&x<360&&y>130&&y<470&&((x-130)/110<>(x-140)/110)) (*cross section*)
		then begin	
					let left_down = get_box_number (x-11) (y-11) in
					let right_up = get_box_number (x+11) (y+11) in
					if (find_name left_down puzzle)=(find_name right_up puzzle) then Some (check_moveable left_down puzzle)
					else Some puzzle 
		   	 end    
	else if x>130&&x<360&&y>30&&y<570&&((x-130)/110<>(x-140)/110) (*straight lines*) 
		then begin 
					let left = get_box_number (x-11) y in
					let right = get_box_number (x+11) y in
					if (find_name left puzzle = find_name right puzzle) then Some (check_moveable left puzzle)
					else Some puzzle 
		 		 end  			
	else if x>30&&x<360&&y>130&&y<470&&((y-130)/110<>(y-140)/110)(*horizontal lines*)
		then begin
					let down = get_box_number x (y-11) in
					let up = get_box_number x (y+11) in
					if (find_name down puzzle) = (find_name up puzzle) then Some (check_moveable down puzzle)
					else Some puzzle 
				 end
	else if x>543&&x<726&&y>484&&y<580 (*Stage One*)then Some stage_one 
	else if (x>543&&x<726&&y>368&&y<464) (*Stage Two*)then Some stage_two
	else if (x>543&&x<726&&y>252&&y<348) (*Stage Three*)then Some stage_three
	else if (x>543&&x<726&&y>136&&y<232) (*Stage AutoSolve*)then Some stage_one  
	else if (x>543&&x<726&&y>20&&y<116)	(*Exit*) then None
	else begin
				let (a,b) = get_box_number x y in (*everything else*)
				if a=5&&b=5 then Some puzzle 
				else Some (check_moveable (a,b)  puzzle)
			 end

let victory ls=
	let v=find_position "A" ls in 
	if v.x=1&&v.y=0 then true
	else false

let () =
	Graphics.open_graph " 800x600+100+50"; (*relate to graph_x and graph_y*)
	Graphics.set_window_title "Klotski";
	let rec loop puzzle=
		draw puzzle;
		(*check for victory*)
		if victory puzzle then
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
