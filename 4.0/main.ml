let graph_x=800
let graph_y=600
type block_position = {x : int; y : int}

let stage_one=[("D1",{x=0; y=0});
						("E1",{x=1; y=0});
						("E2",{x=2; y=0});
						("D2",{x=3; y=0});
						("B1",{x=0; y=1});
						("D3",{x=1; y=1});
						("D4",{x=2; y=1});
						("B2",{x=3; y=1});
						("B1",{x=0; y=2});
						("C", {x=1; y=2});
						("C",{x=2; y=2});
						("B2",{x=3; y=2});
						("B3",{x=0; y=3});
						("A", {x=1; y=3});
						("A",{x=2; y=3});
						("B4",{x=3; y=3});
						("B3",{x=0; y=4});
						("A",{x=1; y=4});
						("A",{x=2; y=4});
						("B4",{x=3; y=4});]
						
let stage_two=[("C",{x=0; y=0});
						("C",{x=1; y=0});
						("D1",{x=2; y=0});
						("D2",{x=3; y=0});
						("D3",{x=0; y=1});
						("B1",{x=1; y=1});
						("B2",{x=2; y=1});
						("D4",{x=3; y=1});
						("B3",{x=0; y=2});
						("B1", {x=1; y=2});
						("B2",{x=2; y=2});
						("B4",{x=3; y=2});
						("B3",{x=0; y=3});
						("A", {x=1; y=3});
						("A",{x=2; y=3});
						("B4",{x=3; y=3});
						("E1",{x=0; y=4});
						("A",{x=1; y=4});
						("A",{x=2; y=4});
						("E2",{x=3; y=4});]
						
let stage_three=[("E1",{x=0; y=0});
						("B1",{x=1; y=0});
						("B2",{x=2; y=0});
						("E2",{x=3; y=0});
						("B3",{x=0; y=1});
						("B1",{x=1; y=1});
						("B2",{x=2; y=1});
						("D1",{x=3; y=1});
						("B3",{x=0; y=2});
						("C", {x=1; y=2});
						("C",{x=2; y=2});
						("D2",{x=3; y=2});
						("B4",{x=0; y=3});
						("A", {x=1; y=3});
						("A",{x=2; y=3});
						("D3",{x=3; y=3});
						("B4",{x=0; y=4});
						("A",{x=1; y=4});
						("A",{x=2; y=4});
						("D4",{x=3; y=4});]
												
(*find the position via name of each block*)
let rec find_position x ls=
	match ls with  
	| [] -> {x=5; y=5} (*bug*)
	| (a,b):: t ->if x = a then b 
								else find_position x t
					
(*find the name via position each block*)				
let rec find_name p puz=
	match puz with
	|(a,b)::t->if b.x=p.x&&b.y=p.y then a 
						 else find_name p t
	|[]-> "BUG"
	
(*function to draw each block*)
let drawblock b p d= (*d for triangle direction*)
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	let posx=(p.x*110+10+20)*ratiox in 
	let posy=((p.y)*110+10+20)*ratioy in
	match String.sub b 0 1 with
		|"D"->begin
					Graphics.set_color 0x5cf442; (*green*)
					Graphics.fill_rect posx posy (100*ratiox) (100*ratioy)
				  end		
		|"E"->()
		|"B"->begin 
					Graphics.set_color 0xf79618; (*orange*)
					Graphics.fill_rect posx posy (100*ratiox) (210*ratioy)	
					end											
		|"C"->begin
					Graphics.set_color 0xff6464; (*red*)
					Graphics.fill_rect posx posy (210*ratiox) (100*ratioy)
					end
		|"A"->begin
					Graphics.set_color 0xf64ff; (*blue*)
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

let draw p =
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 0 0 (Graphics.size_x()) (Graphics.size_y());
	Graphics.set_color 0x202020;
	Graphics.fill_rect (20*ratiox) (20*ratioy) (ratiox*450) (ratioy*560);
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 140 20 210 5;
	drawblock "D1" (find_position "D1" p) "not";
	drawblock "D2" (find_position "D2" p) "not";
	drawblock "D3" (find_position "D3" p) "not";
	drawblock "D4" (find_position "D4" p) "not";
	drawblock "B1" (find_position "B1" p) "not";
	drawblock "B2" (find_position "B2" p) "not";
	drawblock "B3" (find_position "B3" p) "not";
	drawblock "B4" (find_position "B4" p) "not";
	drawblock "C" (find_position "C" p) "not";
	drawblock "A" (find_position "A" p) "not";	
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/Exit.png")) (543*ratiox) (20*ratioy);
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/AutoSolve.png")) (543*ratiox) (136*ratioy);
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageThree.png")) (543*ratiox) (252*ratioy);
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageTwo.png")) (543*ratiox) (368*ratioy);
  Graphics.draw_image (Graphics.make_image (Button.load_array "img/StageOne.png")) (543*ratiox) (484*ratioy);
	Graphics.synchronize()

let judge_true p ls= (*return int*)
	if (p.x<0||p.y<0||p.x>3||p.y>4) then false (*out of board*)
	else if ((find_name p ls) = "E1")||((find_name p ls) = "E2") then true
	else false

let rec swap acc p1 s1 p2 s2 ls = 
	match ls with
		|(a,b)::t->	
					begin
						if b.x=p1.x&&b.y=p1.y then swap ((s2,b)::acc) p1 s1 p2 s2 t
						else if b.x=p2.x&&b.y=p2.y then swap ((s1,b)::acc) p1 s1 p2 s2 t
						else swap ((a,b)::acc) p1 s1 p2 s2 t
					end
		|[]->List.rev acc

let triangle_check x y p d= (*check mouse click within the triangle block*)
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	let posx=(p.x*110+10+20)*ratiox in 
	let posy=((p.y)*110+10+20)*ratioy in
	if x>posx&&y>posy&&x<(posx+100*ratiox)&&y<(posy+100*ratioy) then true
	else false

(*function to solve double true*)
let double_true_f i content p1 d1 p2 d2 ls= (*d1 and d2 are direction*)
	drawblock "T" p1 d1;
	drawblock "T" p2 d2;
	let e_t=Graphics.wait_next_event [Button_down] in     
   	Printf.printf "triangle mouse is: %d \t%d\n" e_t.mouse_x e_t.mouse_y;
	match String.sub content 0 1 with
		|"D"->if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1) then swap [] i content p1 (find_name p1 ls) ls
   				else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2) then swap [] i content p2 (find_name p2 ls) ls 
   				else ls
		|"B"->if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1) then swap [] i content p1 (find_name p1 ls) ls
   				else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2) then swap [] {x=i.x;y=(i.y+1)} content p2 (find_name p2 ls) ls 
   				else ls
		|"C"->if (triangle_check e_t.mouse_x e_t.mouse_y p1 d1) then swap [] i content p1 (find_name p1 ls) ls
   				else if (triangle_check e_t.mouse_x e_t.mouse_y p2 d2) then swap [] {x=(i.x+1);y=i.y} content p2 (find_name p2 ls) ls 
   				else ls
   	|_->ls

let rec check_moveable i ls=
	let content = find_name i ls in
	begin
		match content with
			|"D1"|"D2"|"D3"|"D4"->
					if (judge_true {x=i.x+1;y=i.y} ls) && (judge_true {x=i.x;y=i.y+1} ls) (*right and up*)
						then double_true_f i content {x=i.x+1;y=i.y} "right" {x=i.x;y=i.y+1} "up" ls 
					else if (judge_true {x=i.x+1;y=i.y} ls) && (judge_true {x=i.x;y=i.y-1} ls) (*right and down*)
						then double_true_f i content {x=i.x+1;y=i.y} "right" {x=i.x;y=i.y-1} "down" ls 
					else if (judge_true {x=i.x+1;y=i.y} ls) && (judge_true {x=i.x-1;y=i.y} ls) (*right and left*)
						then double_true_f i content {x=i.x+1;y=i.y} "right" {x=i.x-1;y=i.y} "left" ls
					else if (judge_true {x=i.x;y=i.y+1} ls) && (judge_true {x=i.x-1;y=i.y} ls) (*up and left*)
						then double_true_f i content {x=i.x;y=i.y+1} "up" {x=i.x-1;y=i.y} "left" ls	
					else if (judge_true {x=i.x;y=i.y+1} ls) && (judge_true {x=i.x;y=i.y-1} ls) (*up and down*)
						then double_true_f i content {x=i.x;y=i.y+1} "up" {x=i.x;y=i.y-1} "down" ls
					else if (judge_true {x=i.x-1;y=i.y} ls) && (judge_true {x=i.x;y=i.y-1} ls) (*left and down*)
						then double_true_f i content {x=i.x-1;y=i.y} "left" {x=i.x;y=i.y-1} "down" ls						
					else if (judge_true {x=i.x+1;y=i.y} ls)  (*right*)
						then swap [] i content {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) ls  	
					else if (judge_true {x=i.x;y=i.y+1} ls)	(*up*)
						then swap [] i content {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) ls
					else if (judge_true {x=i.x-1;y=i.y} ls) (*left*)
						then swap [] i content {x=i.x-1;y=i.y} (find_name {x=i.x-1;y=i.y} ls) ls
					else if (judge_true {x=i.x;y=i.y-1} ls) (*down*)
						then swap [] i content {x=i.x;y=i.y-1} (find_name {x=i.x;y=i.y-1} ls) ls
					else ls
			|"B1"|"B2"|"B3"|"B4"->
					if (i <> find_position content ls) then check_moveable (find_position content ls) ls
					else
					( 
						if (judge_true {x=i.x;y=i.y+2} ls) && (judge_true {x=i.x;y=i.y-1} ls) (*up and down*)
							then double_true_f i content {x=i.x;y=i.y+2} "up" {x=i.x;y=i.y-1} "down" ls (*up one must be the fist argument*)
						else if (judge_true {x=i.x+1;y=i.y+1} ls) && (judge_true {x=i.x+1;y=i.y} ls) (*right*)
							then ( let r1 = swap [] i content {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) ls in
								swap [] {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) {x=i.x+1;y=i.y+1} (find_name {x=i.x+1;y=i.y+1} ls) r1 )
						else if (judge_true {x=i.x-1;y=i.y+1} ls) && (judge_true {x=i.x-1;y=i.y} ls)(*left*)
							then (let r1 = swap [] i content {x=i.x-1;y=i.y} (find_name {x=i.x-1;y=i.y} ls) ls in
								swap [] {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) {x=i.x-1;y=i.y+1} (find_name {x=i.x-1;y=i.y+1} ls) r1 )
						else if (judge_true {x=i.x;y=i.y+2} ls) (*up*)
							then swap [] i content {x=i.x;y=i.y+2} (find_name {x=i.x;y=i.y+2} ls) ls	
						else if (judge_true {x=i.x;y=i.y-1} ls) (*down*)
							then swap [] {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) {x=i.x;y=i.y-1} (find_name {x=i.x;y=i.y-1} ls) ls
						else ls		
					)
			|"C"->
					if (i <> find_position content ls) then check_moveable (find_position content ls) ls
					else
					( 
						if (judge_true {x=i.x-1;y=i.y} ls) && (judge_true {x=i.x+2;y=i.y} ls) (*right and left*)
							then double_true_f i content {x=i.x+2;y=i.y} "right" {x=i.x-1;y=i.y} "left" ls (*right one must be the fist argument*)
						else if (judge_true {x=i.x;y=i.y+1} ls) && (judge_true {x=i.x+1;y=i.y+1} ls) (*up*)
							then ( let r1 = swap [] i content {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) ls in
								swap [] {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) {x=i.x+1;y=i.y+1} (find_name {x=i.x+1;y=i.y+1} ls) r1) 
						else if (judge_true {x=i.x;y=i.y-1} ls) && (judge_true {x=i.x+1;y=i.y-1} ls) (*down*)
							then( let r1 = swap [] i content {x=i.x;y=i.y-1} (find_name {x=i.x;y=i.y-1} ls) ls in
								swap [] {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) {x=i.x+1;y=i.y-1} (find_name {x=i.x+1;y=i.y-1} ls) r1)
						else if (judge_true {x=i.x+2;y=i.y} ls) (*right*)
							then( Printf.printf "%d %d XXXXX\n" i.x i.y;
									swap [] i content {x=i.x+2;y=i.y} (find_name {x=i.x+2;y=i.y} ls) ls)
						else if (judge_true {x=i.x-1;y=i.y} ls) (*left*)
							then swap [] {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) {x=i.x-1;y=i.y} (find_name {x=i.x-1;y=i.y} ls) ls
						else ls 
					)
			|"A"->
					if (i <> find_position content ls) then check_moveable (find_position content ls) ls
					else
					( 
						if (judge_true {x=i.x;y=i.y+2} ls) && (judge_true {x=i.x+1;y=i.y+2} ls) (*up*)
							then( let r1 = swap [] i content {x=i.x;y=i.y+2} (find_name {x=i.x;y=i.y+2} ls) ls in
								swap [] {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) {x=i.x+1;y=i.y+2} (find_name {x=i.x+1;y=i.y+2} ls) r1 )
						else if (judge_true {x=i.x;y=i.y-1} ls) && (judge_true {x=i.x+1;y=i.y-1} ls)  (*down*)
							then( let r1 = swap [] {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) {x=i.x;y=i.y-1} (find_name {x=i.x;y=i.y-1} ls) ls in
								swap [] {x=i.x+1;y=i.y+1} (find_name {x=i.x+1;y=i.y+1} ls) {x=i.x+1;y=i.y-1} (find_name {x=i.x+1;y=i.y-1} ls) r1 )
						else if (judge_true {x=i.x-1;y=i.y} ls) && (judge_true {x=i.x-1;y=i.y+1} ls)  (*left*)
							then( let r1 = swap [] {x=i.x+1;y=i.y} (find_name {x=i.x+1;y=i.y} ls) {x=i.x-1;y=i.y} (find_name {x=i.x-1;y=i.y} ls) ls in
								swap [] {x=i.x+1;y=i.y+1} (find_name {x=i.x+1;y=i.y+1} ls) {x=i.x-1;y=i.y+1} (find_name {x=i.x-1;y=i.y+1} ls) r1 )
						else if (judge_true {x=i.x+2;y=i.y} ls) && (judge_true {x=i.x+2;y=i.y+1} ls)  (*right*)
							then( let r1 = swap [] i content {x=i.x+2;y=i.y} (find_name {x=i.x+2;y=i.y} ls) ls in
								swap [] {x=i.x;y=i.y+1} (find_name {x=i.x;y=i.y+1} ls) {x=i.x+2;y=i.y+1} (find_name {x=i.x+2;y=i.y+1} ls) r1 )
						else ls 
					)
			|_->ls
	end	

let get_box_number x y =
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	if (x>=30*ratiox&&x<=130*ratiox&&y>=30*ratioy&&y<=130*ratioy) then {x=0;y=0}
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=30*ratioy&&y<=130*ratioy) then {x=1;y=0}
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=30*ratioy&&y<=130*ratioy) then {x=2;y=0}
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=30*ratioy&&y<=130*ratioy) then {x=3;y=0}
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=140*ratioy&&y<=240*ratioy) then {x=0;y=1}
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=140*ratioy&&y<=240*ratioy) then {x=1;y=1}
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=140*ratioy&&y<=240*ratioy) then {x=2;y=1}
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=140*ratioy&&y<=240*ratioy) then {x=3;y=1}
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=250*ratioy&&y<=350*ratioy) then {x=0;y=2}
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=250*ratioy&&y<=350*ratioy) then {x=1;y=2}
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=250*ratioy&&y<=350*ratioy) then {x=2;y=2}
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=250*ratioy&&y<=350*ratioy) then {x=3;y=2}
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=360*ratioy&&y<=460*ratioy) then {x=0;y=3}
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=360*ratioy&&y<=460*ratioy) then {x=1;y=3}
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=360*ratioy&&y<=460*ratioy) then {x=2;y=3}
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=360*ratioy&&y<=460*ratioy) then {x=3;y=3}
	else if  (x>=30*ratiox&&x<=130*ratiox&&y>=470*ratioy&&y<=570*ratioy) then {x=0;y=4}
	else if  (x>=140*ratiox&&x<=240*ratiox&&y>=470*ratioy&&y<=570*ratioy) then {x=1;y=4}
	else if  (x>=250*ratiox&&x<=350*ratiox&&y>=470*ratioy&&y<=570*ratioy) then {x=2;y=4}
	else if  (x>=360*ratiox&&x<=460*ratiox&&y>=470*ratioy&&y<=570*ratioy) then {x=3;y=4}
	else {x=5;y=5} (*oustide*) 

let make_new_ls x y puzzle=
	let ratiox=Graphics.size_x()/graph_x in
	let ratioy=Graphics.size_y()/graph_y in
	Printf.printf "mouse is: %d\t%d\n" x y;	
	if begin (*cross section*)
		 (x>130*ratiox&&x<140*ratiox&&y>130*ratioy&&y<140*ratioy)||(x>240*ratiox&&x<250*ratiox&&y>130*ratioy&&y<140*ratioy)||
		 (x>350*ratiox&&x<360*ratiox&&y>130*ratioy&&y<140*ratioy)||(x>130*ratiox&&x<140*ratiox&&y>240*ratioy&&y<250*ratioy)||
		 (x>240*ratiox&&x<250*ratiox&&y>240*ratioy&&y<250*ratioy)||(x>350*ratiox&&x<360*ratiox&&y>240*ratioy&&y<250*ratioy)||
		 (x>130*ratiox&&x<140*ratiox&&y>350*ratioy&&y<360*ratioy)||(x>240*ratiox&&x<250*ratiox&&y>350*ratioy&&y<360*ratioy)||
		 (x>350*ratiox&&x<360*ratiox&&y>350*ratioy&&y<360*ratioy)||(x>130*ratiox&&x<140*ratiox&&y>460*ratioy&&y<470*ratioy)||
		 (x>240*ratiox&&x<250*ratiox&&y>460*ratioy&&y<470*ratioy)||(x>350*ratiox&&x<360*ratiox&&y>460*ratioy&&y<470*ratioy) 
	   end 
		then begin	
			let left_down = get_box_number (x-11*ratiox) (y-11*ratioy) in
			let right_up = get_box_number (x+11*ratiox) (y+11*ratioy) in
			if (find_name left_down puzzle)=(find_name right_up puzzle) then Some (check_moveable left_down puzzle)
			else Some puzzle 
		   end   
   
	else if begin (*straight lines*)  
		(x>130*ratiox&&x<140*ratiox&&y>30*ratioy&&y<570*ratioy)||(x>240*ratiox&&x<250*ratiox&&y>30*ratioy&&y<570*ratioy)||
		(x>350*ratiox&&x<360*ratiox&&y>30*ratioy&&y<570*ratioy) 
		 end 
		then begin 
			let left = get_box_number (x-11*ratiox) y in
			let right = get_box_number (x+11*ratiox) y in
			if (find_name left puzzle = find_name right puzzle) then Some (check_moveable left puzzle)
			else Some puzzle 
		 end  
			
	else if begin (*horizontal lines*)
 		(x>30*ratiox&&x<460*ratiox&&y>130*ratioy&&y<140*ratioy)||(x>30*ratiox&&x<460*ratiox&&y>240*ratioy&&y<250*ratioy)||
		(x>30*ratiox&&x<460*ratiox&&y>350*ratioy&&y<360*ratioy)||(x>30*ratiox&&x<460*ratiox&&y>460*ratioy&&y<470*ratioy)
		end 
		then begin
			let down = get_box_number x (y-11*ratioy) in
			let up = get_box_number x (y+11*ratioy) in
			if (find_name down puzzle) = (find_name up puzzle) then Some (check_moveable down puzzle)
			else Some puzzle 
		 end
	else if (x>543*ratiox&&x<726*ratiox&&y>484*ratioy&&y<580*ratioy) (*Stage One*)then Some stage_one 
	else if (x>543*ratiox&&x<726*ratiox&&y>368*ratioy&&y<464*ratioy) (*Stage Two*)then Some stage_two
	else if (x>543*ratiox&&x<726*ratiox&&y>252*ratioy&&y<348*ratioy) (*Stage Three*)then Some stage_three
	else if (x>543*ratiox&&x<726*ratiox&&y>136*ratioy&&y<232*ratioy) (*Stage AutoSolve*)then Some stage_one  
	else if (x>543*ratiox&&x<726*ratiox&&y>20*ratioy&&y<116*ratioy)	(*Exit*) then None
	else begin
			let i = get_box_number x y in (*everything else*)
				if i.x=5&&i.y=5 then  Some puzzle 
				else Some (check_moveable i  puzzle)
			 end

let victory ls=
	if (find_name {x=1;y=0} ls = "A")&&(find_name {x=2;y=0} ls = "A")&&
	   (find_name {x=1;y=1} ls = "A")&&(find_name {x=2;y=1} ls = "A") then true		
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
				let ratiox=Graphics.size_x()/graph_x in
				let ratioy=Graphics.size_y()/graph_y in
				Graphics.draw_image (Graphics.make_image (Button.load_array "img/Victory.png")) (100*ratiox) (75*ratioy); 
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
