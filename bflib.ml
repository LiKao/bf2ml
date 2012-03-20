type stack = S of (unit -> int * stack);;
let rec e = S (fun () -> 0,e);;
type tape = { left : stack ; curr : int; right: stack};;

let put stack num = S (fun () -> num,stack);;
let get (S f) = f ();;


let left tape =
   let (curr,left) = get tape.left in
   {left = left; curr = curr; right = put tape.right tape.curr}
   ;;
let right tape = 
   let (curr,right) = get tape.right in
   {left = put tape.left tape.curr; curr = curr; right = right}
   ;;

let single_step =
   if Array.length Sys.argv > 1 then
      true
   else
      false
   ;;

let debug tape =
   Printf.printf "State of tape is now: ";
   let rec rewind (tape,n) =
      if tape.left != e then
         rewind (left tape,n-1)
      else tape,n
   in
   let rec play (tape,n) =
      if n == 0 then
         Printf.printf "|>%i<|" tape.curr
      else
         Printf.printf "| %i |" tape.curr;
      if tape.right != e then
         play (right tape,n+1)
      else
         ()
   in play (rewind (tape,0));
   print_newline ();
   tape
   ;;

let (==>) f g =
      fun env ->
         if not single_step then
            g (f env)
         else
            g (debug (f env))
      ;;
      
let return = 
   fun x -> x;;
   
let bind f =
   f;;
   
let unbind f =
   f;;

let do_lft  = bind left;;
let do_rgt = bind right;;


let inc tape = {tape with curr = tape.curr +1};;
let dec tape = {tape with curr = tape.curr - 1};;

let do_inc = bind inc;;
let do_dec = bind dec;;
   

let do_get  = 
   bind (fun env -> {env with curr = Char.code (input_char stdin)});;
let do_put =
   bind (fun env -> print_char (Char.chr env.curr);flush stdout; env);;


let rec loop body env = 
   if env.curr != 0 then
      loop body (body env)
   else
      env
   ;;

let rec do_lop = 
   fun body -> bind (loop (unbind body));;
   
let do_dbg =
   bind debug
   ;;
   
let exec cprog =
   unbind cprog {left = e; curr = 0; right = e};;
