(*
Created by Bradley Kovacs
COP 4020 - Summer 2016
Homework 4 - CAM VM
*)

type instruction = 
	
		  STOP
        | LOAD of int
        | QUOTE of int
        | CUR of int
        | DUPL | SWAP | ROT3 | IROT3
        | PLUS | MULT
        | FST | SND | SETFST | SETSND
        | CONS | SPLIT
        | BRANCH of (int * int)
        | APP | RETURN
        | INIT
;;

(*
	Since the rubric didn't say otherwise, I hard coded the instructions.
	Pick one test case below and delete the other when grading.
*)

(*TEST CASE 1*)
let instructions = [
	INIT; LOAD 2; QUOTE 4; CONS; DUPL; FST; SWAP; SND; PLUS; STOP;
];;

(*TEST CASE 2*)
let instructions = [
	INIT; LOAD 0; QUOTE 1; CONS; QUOTE 4; CONS; CUR 8; APP; 
	STOP; DUPL; FST; SWAP; SND; FST; PLUS; RETURN; STOP;
];;

let instructions = Array.of_list instructions;;


let addOne v = 
	v := !v + 1
;;

let subtractOne v = 
	v := !v - 1
;;

let printArray arr = 
	Printf.printf "[";
	for i = 0 to Array.length arr - 1 do
    	Printf.printf "%d;" arr.(i)
  	done;
  	Printf.printf "]"
;;

(*This is the number that signifies the n-th index is empty*)
let emptyEle = ref (-1);;

(*GRADER: changes these values based on your test cases*)
let maxStackHeight = ref 10;;
let maxMemLength = ref 10;;
let stack = Array.make !maxStackHeight !emptyEle;;
let mem = Array.make !maxMemLength !emptyEle;;
let sr = ref 0;;
let mr = ref 0;;
let cr = ref 0;;
let ic = ref 0;;

let load v = 
	Array.set stack !sr v
;;

let quote v = 
	Array.set stack (!sr + 1) v;
	addOne sr
;;

let cur l = 
	Array.set stack (!sr + 1) l;
	addOne sr;
;;

let dupl () = 
	Array.set stack (!sr + 1) stack.(!sr);
	addOne sr
;;

let swap () = 
	let temp1 = stack.(!sr - 1) in 
	let temp2 = stack.(!sr) in 
	Array.set stack !sr temp1;
	Array.set stack (!sr - 1) temp2
;;

let rot3 () = 
	let temp1 = stack.(!sr - 1) in 
	let temp2 = stack.(!sr - 2) in 
	let temp3 = stack.(!sr) in 
	Array.set stack !sr temp1;
	Array.set stack (!sr - 1) temp2;
	Array.set stack (!sr - 2)  temp3
;;

let irot3 () = 
	let temp1 = stack.(!sr - 2) in 
	let temp2 = stack.(!sr) in 
	let temp3 = stack.(!sr - 1) in 
	Array.set stack !sr temp1;
	Array.set stack (!sr - 1) temp2;
	Array.set stack (!sr - 2) temp3
;;

let plus () = 
	let temp1 = stack.(!sr) in 
	let temp2 = stack.(!sr - 1) in 
	let sum = temp1 + temp2 in
	Array.set stack (!sr - 1) sum;
	Array.set stack (!sr) !emptyEle;
	subtractOne sr
;;

let mult () = 
	let temp1 = stack.(!sr) in 
	let temp2 = stack.(!sr - 1) in 
	let product = temp1 * temp2 in
	Array.set stack (!sr - 1) product;
	subtractOne sr
;;

let fst () = 
	let ele = stack.(!sr) in 
	let memEle = mem.(ele) in 
	Array.set stack !sr memEle
;;

let snd () = 
	let idx = (stack.(!sr) + 1) in 
	Array.set stack !sr (mem.(idx))
;;

let setfst () = 
	let idx = stack.(!sr - 1) in 
	Array.set mem idx stack.(!sr);
	subtractOne sr
;;

let setsnd () = 
	let idx = (stack.(!sr - 1) + 1) in 
	Array.set mem idx stack.(!sr);
	subtractOne sr
;;

let cons () = 
	Array.set mem !mr stack.(!sr);
	Array.set stack !sr !emptyEle;
	Array.set mem (!mr + 1) stack.(!sr - 1);
	Array.set stack (!sr - 1) !mr;
	addOne mr;
	addOne mr;
	subtractOne sr
;;

let split () = 
	let ele = mem.(stack.(!sr)) in 
	Array.set stack (!sr + 1) ele;
	let ele2 = mem.(stack.(!sr) + 1) in 
	Array.set stack !sr ele2;
	subtractOne sr
;;

let branch a1 a2 = 
	Array.set stack (!sr) (
		if stack.(!sr) == 1 then a1 
		else a2
	)
;;

let app () = 
	let temp = stack.(!sr) in 
	Array.set stack !sr stack.(!sr - 1);
	Array.set stack (!sr - 1) !cr;
	cr := temp
;;

let return () = 
	cr := stack.(!sr - 1);
	Array.set stack (!sr - 1) stack.(!sr);
	Array.set stack (!sr) !emptyEle;
	subtractOne sr
;;

let outPutArrays () = 
	Printf.printf "cr=%d\tmr=%d\tsr=%d\tMEM" !cr !mr !sr;
	printArray mem;
	Printf.printf "\tSTACK";
	printArray stack;
	Printf.printf "\n"
;;

let exec func arg = 
	func arg;
	outPutArrays ();
	cr := !cr + 1;
	ic := !ic + 1
;;

let exec2 func arg1 arg2 = 
	func arg1 arg2;
	outPutArrays ();
	cr := !cr + 1;
	ic := !ic + 1
;;

let processInstruction instr = match instr with
          STOP 		-> Printf.printf "%d STOP\t\t" !ic; outPutArrays (); cr := (Array.length(instructions) * 2)
        | LOAD v 	-> Printf.printf "%d LOAD %d\t" !ic v; exec load v
        | QUOTE v 	-> Printf.printf "%d QUOTE %d\t" !ic v; exec quote v
        | CUR l 	-> Printf.printf "%d CUR %d\t\t" !ic l; exec cur l
        | DUPL 		-> Printf.printf "%d DUPL\t\t" !ic; exec dupl ()
        | SWAP 		-> Printf.printf "%d SWAP\t\t" !ic;exec swap ()
        | ROT3 		-> Printf.printf "%d ROT3\t" !ic; exec rot3 ()
        | IROT3 	-> Printf.printf "%d IROT3\t" !ic; exec irot3 ()
        | PLUS 		-> Printf.printf "%d PLUS\t\t" !ic; exec plus ()
        | MULT 		-> Printf.printf "%d MULT\t\t" !ic; exec mult ()
        | FST 		-> Printf.printf "%d FST\t\t" !ic; exec fst ()
        | SND 		-> Printf.printf "%d SND\t\t" !ic; exec snd ()
        | SETFST 	-> Printf.printf "%d SETFST\t" !ic; exec setfst ()
        | SETSND 	-> Printf.printf "%d SETSND\t" !ic; exec setsnd ()
        | CONS 		-> Printf.printf "%d CONS\t\t" !ic; exec cons ()
        | SPLIT 	-> Printf.printf "%d SPLIT\t" !ic; exec split ()
        | BRANCH(a1, a2) -> Printf.printf "%d BRANCH %d %d\t" !ic a1 a2; exec2 branch a1 a2
        | APP 		-> Printf.printf "%d APP\t\t" !ic; addOne ic; exec app ()
        | RETURN 	-> Printf.printf "%d RETURN\t" !ic; exec return ()
        | INIT 		-> Printf.printf "Initial setup\t"; addOne cr; outPutArrays (); Printf.printf "\n";
;;

(*Not necessary, but it makes me feel at home*)
let main () = 
	while !cr < Array.length(instructions) do
        processInstruction instructions.(!cr)
	done
;;

main();
