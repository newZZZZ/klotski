(* Game state *)

module State = struct

end

type event = 
  Up | Down | Left | Right | Exit
   
let get_event () =
  if Graphics.key_pressed () then 
  begin
    let ch = Graphics.read_key () in
    
    match ch with
    | 'w' | 'W' -> Some Up
    | 'a' | 'A' -> Some Left
    | 's' | 'S' -> Some Down
    | 'd' | 'D' -> Some Right 
    | 'q' | 'Q' -> Some Exit
    | _ -> None

  end 
  else None

(*function to draw each block*)
let drawblock b p =
	let posx=(p mod 4)*110+10+20 in 
	let posy=(p/4)*110+10+20 in
	match String.sub b 0 1 with
		|"D"-> begin
						Graphics.set_color 0x5cf442; (*green*)
						Graphics.fill_rect posx posy 100 100
						end		
		|"E"-> ()
		|"B"-> begin 
						Graphics.set_color 0xf79618; (*orange*)
						Graphics.fill_rect posx posy 100 210	
					end											
		|"C"-> begin
						Graphics.set_color 0xff0f0f; (*red*)
						Graphics.fill_rect posx posy 210 100
					end
		|"A"->begin
						Graphics.set_color 0x1b0fff; (*blue*)
						Graphics.fill_rect posx posy 210 210
					end
		|_->print_string "Draw block bug\n"	
		
let rec findindex x ls=
	match ls with  
	| [] -> 999
  | h :: t -> if x = h then 0 else (1 + findindex x t)

let draw ls=
	(* draw the background *)
	Graphics.set_color 0xFFFFFF;
	Graphics.fill_rect 0 0 (Graphics.size_x()) (Graphics.size_y());
	Graphics.set_color 0x202020;
	Graphics.fill_rect 20 (Graphics.size_y()-580) 450 560;
	drawblock "D1" (findindex "D1" ls);
	drawblock "D2" (findindex "D2" ls);
	drawblock "D3" (findindex "D3" ls);
	drawblock "D4" (findindex "D4" ls);
	drawblock "B1" (findindex "B1" ls);
	drawblock "B2" (findindex "B2" ls);
	drawblock "B3" (findindex "B3" ls);
	drawblock "B4" (findindex "B4" ls);
	drawblock "C" (findindex "C" ls);
	drawblock "A" (findindex "A" ls);
	Graphics.synchronize()

let () =
  Graphics.open_graph " 800x600+100+50";
  Graphics.set_window_title "Klotski";
  at_exit Graphics.close_graph;

  let rec loop time_prev ls=
    (* sleep *)
    Unix.sleepf 0.01;
    let time_cur = Unix.gettimeofday () in 
    (* print frame rate *)
    Printf.printf "FPS: %g\n%!" (1.0 /. (time_cur -. time_prev));
   
    match get_event () with
    | Some Exit -> ()
    | opt ->
    			(*   
          match opt with
          | None -> ()
          | Some Left -> ()
          | Some Up -> ()
          | Some Right -> ()
          | Some Down -> ()
          | Some _ -> ()
					*)
					
	 (* draw *)
   	draw ls;
      (* call the loop again *)
   	loop time_cur ls
   
  in
  let lis="D1"::"E"::"E"::"D2"::"B1"::"D3"::"D4"::"B2"::"B1"::"C"::"C"::"B2"::"B3"::"A"::"A"::"B4"::"B3"::"A"::"A"::"B4"::[] in
  loop (Unix.gettimeofday ()) lis
  
