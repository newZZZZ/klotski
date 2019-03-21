let graph_x=800
let graph_y=600
(*function to find the index*)
let rec findindex x ls=
	match ls with  
	| [] -> 100
	| h :: t -> if x = h then 0 else (1 + findindex x t)
	
(*function to draw each block*)
let drawblock b p d= (*d for triangle direction*)
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	let posx=((p mod 4)*110+10+20)*ratiox in 
	let posy=((p/4)*110+10+20)*ratioy in
	match String.sub b 0 1 with
		|"D"-> begin
					Graphics.set_color 0x5cf442; (*green*)
					Graphics.fill_rect posx posy (100*ratiox) (100*ratioy)
				end		
		|"E"-> ()
		|"B"-> begin 
					Graphics.set_color 0xf79618; (*orange*)
					Graphics.fill_rect posx posy (100*ratiox) (210*ratioy)	
				end											
		|"C"-> begin
					Graphics.set_color 0xff0f0f; (*red*)
					Graphics.fill_rect posx posy (210*ratiox) (100*ratioy)
				end
		|"A"->begin
					Graphics.set_color 0x1b0fff; (*blue*)
					Graphics.fill_rect posx posy (210*ratiox) (210*ratioy)
				end
		|"T"->begin	
					Graphics.set_color 0xfcf646; (*yellow*)
					match d with
						|"up"->Graphics.fill_poly [|(posx,posy);(posx+100*ratiox,posy);(posx+50*ratiox,posy+100*ratioy)|]
						|"down"->Graphics.fill_poly [|(posx,posy+100*ratioy);(posx+100*ratiox,posy+100*ratioy);(posx+50*ratiox,posy)|]
						|"left"->Graphics.fill_poly [|(posx+100*ratiox,posy);(posx+100*ratiox,posy+100*ratioy);(posx,posy+50*ratioy)|]
						|"right"->Graphics.fill_poly [|(posx,posy);(posx,posy+100*ratioy);(posx+100*ratiox,posy+50*ratioy)|]
						|_->Printf.printf "Triangle Bug\n"
				end
		|_->print_string "Draw block bug\n"
		
(* draw the background *)
let draw ls=
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 0 0 (Graphics.size_x()) (Graphics.size_y());
	Graphics.set_color 0x202020;
	Graphics.fill_rect (20*ratiox) (20*ratioy) (ratiox*450) (ratioy*560);
	drawblock "D1" (findindex "D1" ls) "not";
	drawblock "D2" (findindex "D2" ls) "not";
	drawblock "D3" (findindex "D3" ls) "not";
	drawblock "D4" (findindex "D4" ls) "not";
	drawblock "B1" (findindex "B1" ls) "not";
	drawblock "B2" (findindex "B2" ls) "not";
	drawblock "B3" (findindex "B3" ls) "not";
	drawblock "B4" (findindex "B4" ls) "not";
	drawblock "C" (findindex "C" ls) "not";
	drawblock "A" (findindex "A" ls) "not";
	Graphics.synchronize()

let judge_true_times i ls= (*return int*)
	if (i<0||i>19) then false (*out of board*)
	else if ((List.nth ls i) = "E1")||((List.nth ls i) = "E2") then true
	else false

let rec swap acc p1 s1 p2 s2 ls = 
	let d = List.length acc in
	match ls with
		|a::b->	begin
							if d=p1 then swap (s2::acc) p1 s1 p2 s2 b
							else if d = p2 then swap (s1::acc) p1 s1 p2 s2 b
							else swap (a::acc) p1 s1 p2 s2 b 
						end
		|[]->
			(
				List.map print_string (List.rev acc);
				Printf.printf "\n";
				List.rev acc  
			)

let triangle_check x y p d= (*check mouse click within the triangle block*)
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	let posx=((p mod 4)*110+10+20)*ratiox in 
	let posy=((p/4)*110+10+20)*ratioy in
	if x>posx&&y>posy&&x<(posx+100*ratiox)&&y<(posy+100*ratioy) then true
	else false

(*function to solve double true*)
let double_true_f i content p1 d1 p2 d2 ls= (*d1 and d2 are direction*)
	drawblock "T" p1 d1;
	drawblock "T" p2 d2;
	let e_t=Graphics.wait_next_event [Button_down] in     
   	Printf.printf "triangle mouse is: %d \t%d\n" e_t.mouse_x e_t.mouse_y;
	match String.sub content 0 1 with
		|"D"->	if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1)
   				then swap [] i content p1 (List.nth ls p1) ls
   			else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2)
   				then swap [] i content p2 (List.nth ls p2) ls 
   			else ls
		|"B"-> if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1)
   				then swap [] i content p1 (List.nth ls p1) ls
   			else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2)
   				then swap [] (i+4) content p2 (List.nth ls p2) ls 
   			else ls
		|"C"->if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1)
   				then swap [] i content p1 (List.nth ls p1) ls
   			else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2)
   				then swap [] (i+1) content p2 (List.nth ls p2) ls 
   			else ls
   	|_->ls

let rec check_moveable i ls=
	let content = List.nth ls i in
	begin
		match content with
			|"D1"|"D2"|"D3"|"D4"->
					if (judge_true_times (i+1) ls) && (judge_true_times (i+4) ls) (*right and up*)
						then double_true_f i content (i+1) "right" (i+4) "up" ls 
					else if (judge_true_times (i+1) ls) && (judge_true_times (i-4) ls) (*right and down*)
						then double_true_f i content (i+1) "right" (i-4) "down" ls 
					else if (judge_true_times (i+1) ls) && (judge_true_times (i-1) ls) (*right and left*)
						then double_true_f i content (i+1) "right" (i-1) "left" ls
					else if (judge_true_times (i+4) ls) && (judge_true_times (i-1) ls) (*up and left*)
						then double_true_f i content (i+4) "up" (i-1) "left" ls	
					else if (judge_true_times (i+4) ls) && (judge_true_times (i-4) ls) (*up and down*)
						then double_true_f i content (i+4) "up" (i-4) "down" ls
					else if (judge_true_times (i-1) ls) && (judge_true_times (i-4) ls) (*left and down*)
						then double_true_f i content (i-1) "left" (i-4) "down" ls						
					else if (judge_true_times (i+1) ls)  (*right*)
						then swap [] i content (i+1) (List.nth ls (i+1)) ls  	
					else if (judge_true_times (i+4) ls)	(*up*)
						then swap [] i content (i+4) (List.nth ls (i+4)) ls
					else if (judge_true_times (i-1) ls) (*left*)
						then swap [] i content (i-1) (List.nth ls (i-1)) ls
					else if (judge_true_times (i-4) ls) (*down*)
						then swap [] i content (i-4) (List.nth ls (i-4)) ls
					else ls
			|"B1"|"B2"|"B3"|"B4"->
					if (i <> findindex content ls) then check_moveable (findindex content ls) ls
					else
					( 
						if (judge_true_times (i+8) ls) && (judge_true_times (i-4) ls) (*up and down*)
							then double_true_f i content (i+8) "up" (i-4) "down" ls (*up one must be the fist argument*)
						else if (judge_true_times (i+5) ls) && (judge_true_times (i+1) ls) (*right*)
							then ( let r1 = swap [] i content (i+1) (List.nth ls (i+1)) ls in
								swap [] (i+4) (List.nth ls (i+4)) (i+5) (List.nth ls (i+5)) r1 )
						else if (judge_true_times (i+3) ls) && (judge_true_times (i-1) ls)(*left*)
							then (let r1 = swap [] i content (i-1) (List.nth ls (i-1)) ls in
								swap [] (i+4) (List.nth ls (i+4)) (i+3) (List.nth ls (i+3)) r1 )
						else if (judge_true_times (i+8) ls) (*up*)
							then swap [] i content (i+8) (List.nth ls (i+8)) ls	
						else if (judge_true_times (i-4) ls) (*down*)
							then swap [] (i+4) (List.nth ls (i+4)) (i-4) (List.nth ls (i-4)) ls
						else ls		
					)
			|"C"->
					if (i <> findindex content ls) then check_moveable (findindex content ls) ls
					else
					( 
						if (judge_true_times (i-1) ls) && (judge_true_times (i+2) ls) (*right and left*)
							then double_true_f i content (i+2) "right" (i-1) "left" ls (*right one must be the fist argument*)
						else if (judge_true_times (i+4) ls) && (judge_true_times (i+5) ls) (*up*)
							then ( let r1 = swap [] i content (i+4) (List.nth ls (i+4)) ls in
								swap [] (i+1) (List.nth ls (i+1)) (i+5) (List.nth ls (i+5)) r1) 
						else if (judge_true_times (i-4) ls) && (judge_true_times (i-3) ls) (*down*)
							then( let r1 = swap [] i content (i-4) (List.nth ls (i-4)) ls in
								swap [] (i+1) (List.nth ls (i+1)) (i-3) (List.nth ls (i-3)) r1)
						else if (judge_true_times (i+2) ls) (*right*)
							then( Printf.printf "%d XXXXX\n" i;
									swap [] i content (i+2) (List.nth ls (i+2)) ls)
						else if (judge_true_times (i-1) ls) (*left*)
							then swap [] (i+1) (List.nth ls (i+1)) (i-1) (List.nth ls (i-1)) ls
						else ls 
					)
			|"A"->
					if (i <> findindex content ls) then check_moveable (findindex content ls) ls
					else
					( 
						if (judge_true_times (i+8) ls) && (judge_true_times (i+9) ls) (*up*)
							then( let r1 = swap [] i content (i+8) (List.nth ls (i+8)) ls in
								swap [] (i+1) (List.nth ls (i+1)) (i+9) (List.nth ls (i+9)) r1 )
						else if (judge_true_times (i-4) ls) && (judge_true_times (i-3) ls)  (*down*)
							then( let r1 = swap [] (i+4) (List.nth ls (i+4)) (i-4) (List.nth ls (i-4)) ls in
								swap [] (i+5) (List.nth ls (i+5)) (i-3) (List.nth ls (i-3)) r1 )
						else if (judge_true_times (i-1) ls) && (judge_true_times (i+3) ls)  (*left*)
							then( let r1 = swap [] (i+1) (List.nth ls (i+1)) (i-1) (List.nth ls (i-1)) ls in
								swap [] (i+5) (List.nth ls (i+5)) (i+3) (List.nth ls (i+3)) r1 )
						else if (judge_true_times (i+2) ls) && (judge_true_times (i+6) ls)  (*right*)
							then( let r1 = swap [] i content (i+2) (List.nth ls (i+2)) ls in
								swap [] (i+4) (List.nth ls (i+4)) (i+6) (List.nth ls (i+6)) r1 )
						else ls 
					)
			|_->ls
	end		

let get_box_number x y =
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	if (x>=30*ratiox&&x<=130*ratiox&&y>=30*ratioy&&y<=130*ratioy) then 0
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=30*ratioy&&y<=130*ratioy) then 1
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=30*ratioy&&y<=130*ratioy) then 2
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=30*ratioy&&y<=130*ratioy) then 3
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=140*ratioy&&y<=240*ratioy) then 4
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=140*ratioy&&y<=240*ratioy) then 5
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=140*ratioy&&y<=240*ratioy) then 6
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=140*ratioy&&y<=240*ratioy) then 7
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=250*ratioy&&y<=350*ratioy) then 8
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=250*ratioy&&y<=350*ratioy) then 9
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=250*ratioy&&y<=350*ratioy) then 10
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=250*ratioy&&y<=350*ratioy) then 11
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=360*ratioy&&y<=460*ratioy) then 12
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=360*ratioy&&y<=460*ratioy) then 13
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=360*ratioy&&y<=460*ratioy) then 14
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=360*ratioy&&y<=460*ratioy) then 15
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=470*ratioy&&y<=570*ratioy) then 16
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=470*ratioy&&y<=570*ratioy) then 17
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=470*ratioy&&y<=570*ratioy) then 18
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=470*ratioy&&y<=570*ratioy) then 19 
	else 20 (*oustide*) 

let make_new_ls x y ls=
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	Printf.printf "mouse is: %d\t%d\n" x y;	
	if begin (*cross section*)
		 (x>130*ratiox&&x<140*ratiox&&y>130*ratioy&&y<140*ratioy)||(x>240*ratiox&&x<250*ratiox&&y>130*ratioy&&y<140*ratioy)||
		 (x>350*ratiox&&x<360*ratiox&&y>130*ratioy&&y<140*ratioy)||(x>130*ratiox&&x<140*ratiox&&y>240*ratioy&&y<250*ratioy)||
		 (x>240*ratiox&&x<250*ratiox&&y>240*ratioy&&y<250*ratioy)||(x>350*ratiox&&x<360*ratiox&&y>240*ratioy&&y<250*ratioy)||
		 (x>130*ratiox&&x<140*ratiox&&y>350*ratioy&&y<360*ratioy)||(x>240*ratiox&&x<250*ratiox&&y>350*ratioy&&y<360*ratioy)||
		 (x>350*ratiox&&x<360*ratiox&&y>350*ratioy&&y<360*ratioy)||(x>130*ratiox&&x<140*ratiox&&y>470*ratioy&&y<480*ratioy)||
		 (x>240*ratiox&&x<250*ratiox&&y>470*ratioy&&y<480*ratioy)||(x>350*ratiox&&x<360*ratiox&&y>470*ratioy&&y<480*ratioy) 
	    end then (	let left_down = get_box_number (x-11*ratiox) (y-11*ratioy) in
			let right_up = get_box_number (x+11*ratiox) (y+11*ratioy) in
			if (List.nth ls left_down = List.nth ls right_up) then check_moveable left_down ls
			else ls  
		      )   
   
	else if begin (*straight lines*)  
		(x>130*ratiox&&x<140*ratiox&&y>30*ratioy&&y<570*ratioy)||(x>240*ratiox&&x<250*ratiox&&y>30*ratioy&&y<570*ratioy)||
		(x>350*ratiox&&x<360*ratiox&&y>30*ratioy&&y<570*ratioy) 
		end then (	let left = get_box_number (x-11*ratiox) y in
				let right = get_box_number (x+11*ratiox) y in
				if (List.nth ls left = List.nth ls right ) then check_moveable left ls
				else ls  
		      	)  
			
	else if begin (*horizontal lines*)
 		(x>30*ratiox&&x<460*ratiox&&y>130*ratioy&&y<140*ratioy)||(x>30*ratiox&&x<460*ratiox&&y>240*ratioy&&y<250*ratioy)||
		(x>30*ratiox&&x<460*ratiox&&y>350*ratioy&&y<360*ratioy)||(x>30*ratiox&&x<460*ratiox&&y>460*ratioy&&y<470*ratioy)
		end then (	let down = get_box_number x (y-11*ratioy) in
				let up = get_box_number x (y+11*ratioy) in
				if (List.nth ls down = List.nth ls up ) then check_moveable down ls
				else ls  
		      	)
	
	else begin
			let i = get_box_number x y in (*everythign else*)
				if i=20 then ls
				else check_moveable i ls
		 end

let () =
  Graphics.open_graph " 800x600+100+50"; (*relate to graph_x and graph_y*)
  Graphics.set_window_title "Klotski";
  at_exit Graphics.close_graph;
  let rec loop ls=
   	draw ls;
    let e=Graphics.wait_next_event [Button_down] in     
	let new_ls=make_new_ls e.mouse_x e.mouse_y ls in 
    (* call the loop again *)
   	loop new_ls
  in
  let lis="D1"::"E1"::"E2"::"D2"::"B1"::"D3"::"D4"::"B2"::"B1"::"C"::"C"::"B2"::"B3"::"A"::"A"::"B4"::"B3"::"A"::"A"::"B4"::[] in
  loop lis
  
