let przelewanka arr = 
  let arr_len    = Array.length arr     in
  let visited    = Hashtbl.create 10000 in
  let todo_queue = Queue.create ()      in
  let answer     = ref (-1)             in

  let is_solution x = 
    let ret = ref true in
      for i = 0 to arr_len-1 do
        ret := !ret && x.(i) = snd arr.(i)
      done; !ret
  in

  let rec gcd x y = if y = 0 then x else gcd y (x mod y) in 

  let check_not_gcd x = 
    let size_gcd = Array.fold_left (fun acc (size, _) -> gcd acc size) 0 x in
      size_gcd <> 0 && 
      Array.fold_left (fun acc (_, end_val) -> acc || end_val mod size_gcd <> 0) false x
  in

  let check_not_empty_or_full x = 
    arr_len <> 0 && 
    Array.fold_left (fun acc (size, end_val) -> acc && end_val <> 0 && end_val <> size) true x
  in

  let add_if_not_visited (x, dst) = 
    if not ( Hashtbl.mem visited x ) then begin
      if is_solution x then begin
        answer := dst
      end;
      Hashtbl.add visited x true; 
      Queue.push (x, dst) todo_queue
    end
  in

  let glass_change i v x = let new_x = Array.copy x in ( new_x.(i) <- v; new_x ) in

  let glass_move i j x = (* Moves water from glass i to j *)
    let new_x      = Array.copy x in
    let free_space = fst arr.(j) - x.(j) in
      if free_space >= x.(i) then begin
        new_x.(j) <- x.(j) + x.(i);
        new_x.(i) <- 0
      end else begin
        new_x.(j) <- fst arr.(j);
        new_x.(i) <- x.(i) - free_space;
      end;
      new_x
  in

    if check_not_gcd arr || check_not_empty_or_full arr then
      -1 
    else begin
      add_if_not_visited (Array.make arr_len 0, 0);
      while !answer = -1 && not (Queue.is_empty todo_queue) do 
        let (x, dst) = Queue.pop todo_queue in
            for i = 0 to arr_len - 1 do
	      if !answer = -1 then begin
              	add_if_not_visited (glass_change i 0             x, dst+1);
              	add_if_not_visited (glass_change i (fst arr.(i)) x, dst+1);
              	for j = 0 to arr_len - 1 do
                  if not (i = j) && !answer = -1 then
                    add_if_not_visited (glass_move i j x, dst+1)
              	done
	      end
            done
      done; 
      !answer
    end
