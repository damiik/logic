(* type and4 = { in0: int; in1: int ; in2: int; in3: int; output: int } *)

open Table;;
open Array;;

type lineStateT = LS_1 | LS_0 | LS_X 

let l = ("L", LS_0)
let h = ("H", LS_1)


type unit = (string * lineStateT) list
type error = {desc: string;}

type 'a linesResult = (('a, error) result) 

(* type mess = SUnit of signal list | MUnit of ((unit, error) result) list *)
type mess =  SUnit of unit linesResult | MUnit of (unit linesResult list) 
(* | (unit linesResult)  list) *)

type 'a solver = {

  solve: 'a linesResult -> 'a linesResult; (* type: result of (input * 'a, error) *)
}


let unitToStr (out: unit) : string =

    let rec f = fun l i ->
      match l with 
      | [] -> ""
      | (s, LS_1)::xs -> Printf.sprintf "%s [ %s ]%d: <1>\n" (f xs (i+1)) s i;
      | (s, LS_0)::xs -> Printf.sprintf "%s [ %s ]%d: <0>\n" (f xs (i+1)) s i;
      | (s, LS_X)::xs -> Printf.sprintf "%s [ %s ]%d: <*>\n" (f xs (i+1)) s i;

    in
    f out 0 (* ((List.length out) - 1) *)

(* short ver. *)
let unitToStr2 (out: unit) : string =

    let rec f = fun l i ->
      match l with 
      | [] -> ""
      | (_, LS_1)::xs -> Printf.sprintf "%s1" (f xs (i+1));
      | (_, LS_0)::xs -> Printf.sprintf "%s0" (f xs (i+1));
      | (_, LS_X)::xs -> Printf.sprintf "%s*" (f xs (i+1));

    in
    f out 0 

let resultToStr (out: unit linesResult) : string =

  match out with
  | Error error -> error.desc
  | Ok o -> unitToStr o

let rec pow(x, n) =
    if n=0 then 1 else x * pow(x, n-1)


let unitToNum (out: unit) : int =

    let rec f = fun l i acc ->
      match l with 
      | [] -> acc
      | (s, LS_1)::xs ->  (f xs (i+1) acc + pow (2, i))
      | (s, LS_0)::xs ->  (f xs (i+1) acc)
      | (s, LS_X)::xs ->  (f xs (i+1) acc)

    in
    (f out 0 0) (* ((List.length out) - 1) *)

let resultToNum (out: unit linesResult) : int =

  match out with
  | Error error -> -1
  | Ok o -> unitToNum o



let n2Unit (n : int) lab s : unit = 

  let u : int32 = if n < 0 then Int32.add (Int32.lognot (Int32.of_int (-n))) Int32.one
    else (Int32.of_int n) 
  in (* (if n<0 then (lnot (-n) + 1) else n)  *)

  let rec f = fun (n: int32) (lab: string) (acc: unit) (i:int) : unit ->
    if s > i then acc @ ((f (Int32.shift_right_logical n 1) lab ((Printf.sprintf"%s%d" lab i, if (Int32.logand n (Int32.of_int 0x01)) > Int32.zero then LS_1 else LS_0)::[])) (i + 1))
    else acc
  in
  f u lab [] 0




let ( >>= )  (p: 'a linesResult) (f: 'a -> 'b linesResult)  : 'b linesResult =
  
      match p with
      | Ok a -> f a
      | Error error -> Error error


(* monoid ++ *)
let (++) (u1: unit linesResult ) (u2: unit linesResult) : unit linesResult =

  u1 >>= fun u1' -> u2 >>= fun u2' -> Ok ( u1' @  u2')
  

(*  (us: unit solver) *)

let solve_unit (lr: (unit linesResult) list) (us: unit solver): unit linesResult = 

  let rec f  (l: (unit linesResult) list)  : unit linesResult = 
      (* let x::xs = u in Printf.printf "solve_unit:%s" (resultToStr u); *)
      match l with
        | [] -> Ok []
        | x::xs -> x >>= fun u -> (f xs) >>= fun w -> Ok (u @ w)
  in
  let result = ref []
  in
  (f lr) >>= fun u -> result := u ;
  us.solve (Ok !result)

(* 
let rec compare_unit (p1: unit) (p2: unit) : (string, string) result = 
      match p1 with
      | [] -> begin match p2 with       
                    | [] -> Ok "pass"
                    |(x2::x2s) -> Error "fail -> first unit too short"
                    end
      |(x1::x1s) -> begin match p2 with
                          | [] -> Error "fail -> second unit too short"
                          |(x2::x2s) -> 
                                    let (l_x1, r_x1) = x1 in
                                    let (l_x2, r_x2) = x2 in
                                    if (compare l_x1 l_x2) != 0 then Error (Printf.sprintf "fail -> labes not equal [%s] <> [%s]" l_x1 l_x2)
                                    else if r_x1 != r_x2 then Error (Printf.sprintf "fail -> signal levels not equal [%s] <> [%s]" l_x1 l_x2)
                                    else compare_unit x1s x2s
                    end *)
                 
(* compare without labels *)
let rec test_unit (p1: unit) (p2: unit) : unit linesResult = 
      match p1 with
      | [] -> begin match p2 with       
                    | [] -> Ok []
                    |(x2::x2s) -> Error {desc = "first unit too short"}
                    end
      |(x1::x1s) -> begin match p2 with
                          | [] -> Error {desc = "second unit too short"}
                          |(x2::x2s) -> 
                            let (l_x1, r_x1) = x1 in
                            let (l_x2, r_x2) = x2 in
                            if r_x1 != r_x2 then Error {desc = Printf.sprintf "signal levels not equal [%s] <> [%s]" l_x1 l_x2}
                            else
                            (* if (compare l_x1 l_x2) != 0 then Error {desc = Printf.sprintf "labes not equal [%s] <> [%s]" l_x1 l_x2} *)
                            test_unit x1s x2s
                    end
                 

let rec make_unit (inp : unit linesResult list): unit linesResult =

  let rec f (acc : unit linesResult)  (m : unit linesResult list): unit linesResult  = 
    match m with
    | [] -> acc            
    | x::[] ->   x >>= fun u -> acc >>= fun acc' -> (Ok (acc' @ u))
    | (x::xs) -> x >>= fun u -> acc >>= fun acc' -> (f (Ok (acc' @ u)) xs)
  in
  f (Ok []) inp

let make_unit_test : unit linesResult = 

  let base_signals_a = n2Unit 0b1010 "a" 4 in
  let base_signals_b = n2Unit 0b1111 "b" 4 in

  (* prepare input list of results of signals *)
  let function_input_signals = List.map (fun e -> Ok [e]) ((n2Unit 0b1010 "a" 4 ) @ (n2Unit 0b1111 "b" 4)) in

  (* test make_unit *)
  let res = make_unit function_input_signals >>= fun created_unit ->
                  test_unit (base_signals_a @ base_signals_b) created_unit
                  in
                  match res with
                      |Ok m -> Printf.printf "make_unit_test: PASS\n"; Ok m
                      |Error e ->  Printf.printf "make_unit_test: FAIL - %s\n" e.desc; Error e




let sNeg : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | [] -> Error {desc = "[sNeg] missing input\n"}
      | (_, LS_1)::xs -> Ok (("sNeg", LS_0)::xs)
      | _::xs -> Ok (("sNeg", LS_1)::xs)
  
} 

(* Tri-state negator oe in *)
let sTneg : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | [] -> Error {desc = "[tneg_out] missing input\n"}
      | _::[] -> Error {desc = "[tneg_out] missing input\n"}
      | (_, LS_1)::(_, LS_1)::xs -> Ok (("tneg_out", LS_0)::xs)
      | (_, LS_1)::(_, LS_0)::xs -> Ok (("tneg_out", LS_1)::xs)
      | _::_::xs -> Ok (("tneg_out", LS_X)::xs)
} 

let sBuf : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | [] -> Error {desc = "[sBuf] missing input\n"} 
      | (_, b)::xs -> Ok (("sBuf", b)::xs)
} 

(* Tri-state bufor inputs: oe in *)
let sTbuf : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | [] -> Error {desc = "[tbuf_out] missing input\n"}
      | _::[] -> Error {desc = "[tbuf_out] missing input\n"}
      | (_, LS_1)::(_, LS_0)::xs -> Ok (("tbuf_out", LS_0)::xs)
      | (_, LS_1)::(_, LS_1)::xs -> Ok (("tbuf_out", LS_1)::xs)
      | _::_::xs -> Ok (("tbuf_out", LS_X)::xs)
} 

let sAnd : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_1)::(_, LS_1)::xs -> Ok (("sAnd", LS_1)::xs) 
      | _::_::xs -> Ok (("sAnd", LS_0)::xs) 
      | _ -> Error {desc = "[sAnd] missing input\n"}  
} 

let sAnd3 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_1)::(_, LS_1)::(_, LS_1)::xs -> Ok (("sAnd3", LS_1)::xs) 
      | _::_::_::xs -> Ok (("sAnd3", LS_0)::xs) 
      | _ -> Error {desc = "[sAnd3] missing input\n"}  
} 

let sAnd4 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_1)::(_, LS_1)::(_, LS_1)::(_, LS_1)::xs -> Ok (("sAnd4", LS_1)::xs) 
      | _::_::_::_::xs -> Ok (("sAnd4", LS_0)::xs) 
      | _ -> Error {desc = "[sAnd4] missing input\n"}  
} 

let sAnd5 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_1)::(_, LS_1)::(_, LS_1)::(_, LS_1)::(_, LS_1)::xs -> Ok (("sAnd5", LS_1)::xs) 
      | _::_::_::_::_::xs -> Ok (("sAnd5", LS_0)::xs) 
      | _ -> Error {desc = "[sAnd5] missing input\n"}  
} 



let sNand : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_1)::(_, LS_1)::xs -> Ok (("sNand", LS_0)::xs) 
      | _::_::xs -> Ok (("sNand", LS_1)::xs) 
      | _ -> Error {desc = "[sNand] missing input\n"}  
} 




let sOr : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_0)::(n2, LS_0)::xs -> Ok (("sOr", LS_0)::xs)
      | _::_::xs -> Ok (("sOr", LS_1)::xs)
      | _ -> Error {desc = "[sOr] missing 2 inputs"}   
}  

let sXor : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_0)::(n2, LS_0)::xs -> Ok (("sOr", LS_0)::xs)
      | (_, LS_1)::(n2, LS_1)::xs -> Ok (("sOr", LS_0)::xs)
      | _::_::xs -> Ok (("sOr", LS_1)::xs)
      | _ -> Error {desc = "[sOr] missing 2 inputs"}   
}  

let sNor : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
        | (_, LS_0)::(_, LS_0)::xs -> Ok (("sNor", LS_1)::xs)
        | _::_::xs -> Ok (("sNor", LS_0)::xs)
        | _ -> Error {desc = "[sNor] missing input\n"} 
}   

let sNor3 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> Ok (("sNor3", LS_1)::xs)
      | _::_::_::xs -> Ok (("sNor3", LS_0)::xs)
      | _ -> Error {desc = "[sNor3] missing input\n"} 
}   


let sNor4 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> Ok (("sNor4", LS_1)::xs)
      | _::_::_::_::xs -> Ok (("sNor4", LS_0)::xs)
      | _ -> Error {desc = "[sNor4] missing input\n"} 
}   


let sNor5 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
          match unit' with
          | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> Ok (("sNor5", LS_1)::xs)
          | _::_::_::_::_::xs -> Ok (("sNor5", LS_0)::xs)
          | _ -> Error {desc = "[sNor5] missing input\n"} 
}   


let sNor8 : unit solver = {

  solve = fun result' -> 
      result' >>= fun unit' ->
          match unit' with
        | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> Ok (("sNor8", LS_1)::xs)
        | _::_::_::_::_::_::_::_::xs -> Ok (("sNor8", LS_0)::xs)
        | _ -> Error {desc = "[sNor8] missing input\n"} 
}   

let sOr4 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> Ok (("sNor4", LS_0)::xs)
      | _::_::_::_::xs -> Ok (("sNor4", LS_1)::xs)
      | _ -> Error {desc = "[sNor4] missing input\n"} 
}   


(* 74hc251 multiplexer simulation *)
let s74hc251 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | [] -> Error { desc = "[s74hc251] missing input\n" } ;
      | (oe'::s0'::s1'::s2'::in0::in1::in2::in3::in4::in5::in6::in7::xs) -> 
          let oe = Ok [oe'] in
          let s0 = Ok [s0'] in
          let s1 = Ok [s1'] in
          let s2 = Ok [s2'] in

          let in_oe_neg = sNeg.solve oe in
          let in_s0_neg = sNeg.solve s0 in
          let in_s1_neg = sNeg.solve s1 in
          let in_s2_neg = sNeg.solve s2 in

          let in_x = [| 
              Ok [in0]; 
              Ok [in1]; 
              Ok [in2]; 
              Ok [in3]; 
              Ok [in4]; 
              Ok [in5]; 
              Ok [in6]; 
              Ok [in7] |] in

          (* let in_x = Array.make 8 (Ok [("in", LS_0)]) in *)
          (* Array.iteri (fun x _ -> in_x.(x) <- Ok [(Printf.sprintf "in%d" x, LS_0)]) in_x; *)
          (* in_x.(1) <- Ok [(Printf.sprintf "in%d" 1, LS_0)]; *)
          
          (* Array.iteri (fun i e -> Printf.printf "in_%d:%s" i (unitToStr e)) in_x; *)

          let buf_out = Array.make 8 (Ok [("buf_out", LS_0)]) in
          Array.iteri (fun x _ -> (buf_out.(x) <- sBuf.solve in_x.(x))) buf_out;

          let and4_out = Array.make 8 (Ok [("and4_out", LS_0)]) in
          and4_out.(0) <- buf_out.(0) ++ in_s2_neg ++ in_s1_neg ++ in_s0_neg |> sAnd4.solve;
          and4_out.(1) <- buf_out.(1) ++ in_s2_neg ++ in_s1_neg ++ s0 |> sAnd4.solve;
          and4_out.(2) <- buf_out.(2) ++ in_s2_neg ++ s1 ++ in_s0_neg |> sAnd4.solve;
          and4_out.(3) <- buf_out.(3) ++ in_s2_neg ++ s1 ++ s0 |> sAnd4.solve;
          and4_out.(4) <- buf_out.(4) ++ s2 ++ in_s1_neg ++ in_s0_neg |> sAnd4.solve;
          and4_out.(5) <- buf_out.(5) ++ s2 ++ in_s1_neg ++ s0 |> sAnd4.solve;
          and4_out.(6) <- buf_out.(6) ++ s2 ++ s1 ++ in_s0_neg |> sAnd4.solve;
          and4_out.(7) <- buf_out.(7) ++ s2 ++ s1 ++ s0 |> sAnd4.solve;

          (* Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (unitToStr e)) and4_out; *)

          let nor8_out = and4_out.(0) ++ 
                         and4_out.(1) ++ 
                         and4_out.(2) ++ 
                         and4_out.(3) ++ 
                         and4_out.(4) ++ 
                         and4_out.(5) ++ 
                         and4_out.(6) ++ 
                         and4_out.(7) |> sNor8.solve in
          (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out]))  *)
          
          let out_y = nor8_out ++ in_oe_neg |> sTbuf.solve in
          let neg_y = nor8_out ++ in_oe_neg |> sTneg.solve in
          (out_y ++ neg_y ++ (Ok xs))

      | _ -> Error { desc = "[s74hc251] missing input\n" } ;
      
}

(* s74hc153 multiplexer simulation *)
let s74hc153 : unit solver = {

  solve = fun result' -> 
      result' >>= fun unit' ->
          begin match unit' with
          | [] -> Error { desc = "[s74hc153] missing input\n" } ;
          | (ea::eb::s0::s1::in0a::in1a::in2a::in3a::in0b::in1b::in2b::in3b::xs) -> 

              sNeg.solve (Ok [ea]) >>= fun in_ea_neg -> 
              sNeg.solve (Ok [eb]) >>= fun in_eb_neg -> 
              sNeg.solve (Ok [s0]) >>= fun in_s0_neg -> 
              sNeg.solve (Ok [s1]) >>= fun in_s1_neg -> 
  
              Ok (in0a :: in_s1_neg @ in_s0_neg @ in_ea_neg)  |> sAnd4.solve >>= fun a0 ->
              Ok (in1a :: in_s1_neg @ s0 :: in_ea_neg)        |> sAnd4.solve >>= fun a1 ->
              Ok (in2a :: s1 :: in_s0_neg @ in_ea_neg)        |> sAnd4.solve >>= fun a2 ->
              Ok (in3a :: s1 :: s0 :: in_ea_neg)              |> sAnd4.solve >>= fun a3 ->
              Ok (in0b :: in_s1_neg @ in_s0_neg @ in_eb_neg)  |> sAnd4.solve >>= fun a4 ->
              Ok (in1b :: in_s1_neg @ s0 :: in_eb_neg)        |> sAnd4.solve >>= fun a5 ->
              Ok (in2b :: s1 :: in_s0_neg @ in_eb_neg)        |> sAnd4.solve >>= fun a6 ->
              Ok (in3b :: s1 :: s0 :: in_eb_neg)              |> sAnd4.solve >>= fun a7 ->

              Ok (a0 @ a1 @ a2 @ a3)                          |> sOr4.solve >>= fun or4a_out ->
              Ok (a4 @ a5 @ a6 @ a7)                          |> sOr4.solve >>= fun or4b_out ->
                                
              Ok (or4a_out @ or4b_out @ xs)               

              (* Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (resultToStr e)) and4_out;  *)
            
          | _ -> Error { desc = "[s74hc153] missing input\n" } ;
         end
}

let s74hc153_test : unit linesResult = 

  let in_a = n2Unit 0b0011 "a" 4 in
  let in_b = n2Unit 0b0110 "b" 4 in
  let ea_eb = n2Unit 0b00 "ea_eb" 2 in

  let res = 
  Ok (ea_eb @ [l; l] @ in_a @ in_b) |> s74hc153.solve >>= fun test01 ->
  Ok (ea_eb @ [h; l] @ in_a @ in_b) |> s74hc153.solve >>= fun test02 ->
  Ok (ea_eb @ [l; h] @ in_a @ in_b) |> s74hc153.solve >>= fun test03 ->
  Ok (ea_eb @ [h; h] @ in_a @ in_b) |> s74hc153.solve >>= fun test04 ->

  test_unit (test01 @ test02 @ test03 @ test04) (n2Unit 0b00101101 "exp_val" 8)
  in
  match res with
      |Ok m -> Printf.printf "s74hc153_test: PASS\n"; Ok m
      |Error e ->  Printf.printf "s74hc153_test: FAIL - %s\n" e.desc; Error e



(* 74hc283 adder simulation *)
let s74hc283 : unit solver = {

  solve = fun result' -> 
      result' >>= fun unit' ->
          begin match unit' with
          | [] -> Error { desc = "[s74hc283] missing input\n" } ;
          | (cin::a1::a2::a3::a4::b1::b2::b3::b4::xs) -> 

              Ok [cin] |> sNeg.solve >>= fun cin_neg -> 

              Ok [a1; b1] |> sNand.solve >>= fun nand20 ->
              Ok [a1; b1] |> sNor.solve >>= fun nor20 ->
              Ok [a2; b2] |> sNand.solve >>= fun nand21 ->
              Ok [a2; b2] |> sNor.solve >>= fun nor21 ->
              Ok [a3; b3] |> sNand.solve >>= fun nand22 ->
              Ok [a3; b3] |> sNor.solve >>= fun nor22 ->
              Ok [a4; b4] |> sNand.solve >>= fun nand23 ->
              Ok [a4; b4] |> sNor.solve >>= fun nor23 ->

              Ok nor20 |> sNeg.solve >>= fun nor20n ->
              Ok nor21 |> sNeg.solve >>= fun nor21n ->
              Ok nor22 |> sNeg.solve >>= fun nor22n ->
              Ok nor23 |> sNeg.solve >>= fun nor23n ->

              Ok (nor20n @ nand20) |> sAnd.solve >>= fun and2020 ->
              Ok (nor20n @ nand21) |> sAnd.solve >>= fun and2021 ->
              Ok (nor21n @ nand21) |> sAnd.solve >>= fun and2121 ->
              Ok (nor22n @ nand22) |> sAnd.solve >>= fun and2222 ->
              Ok (nor23n @ nand23) |> sAnd.solve >>= fun and2323 ->
              Ok (nand23 @ nor22) |> sAnd.solve >>= fun and23nor22 ->
              Ok (nand22 @ nor21) |> sAnd.solve >>= fun and22nor21 ->
              Ok (nand22 @ nand23 @ nor21) |> sAnd3.solve >>= fun and32223nor21 ->
              Ok (nand21 @ nand22 @ nor20) |> sAnd3.solve >>= fun and32122nor20 ->
              Ok (nand21 @ nand22 @ nand23 @ nor20) |> sAnd4.solve >>= fun and4 ->

              Ok (cin_neg @ nand20) |> sAnd.solve >>= fun andinn20 ->
              Ok (cin_neg @ nand20 @ nand21) |> sAnd3.solve >>= fun iand2021 ->
              Ok (cin_neg @ nand20 @ nand21 @ nand22) |> sAnd4.solve >>= fun iand202122  ->        
              Ok (cin_neg @ nand20 @ nand21 @ nand22 @ nand23) |> sAnd5.solve >>= fun and5 ->

              Ok (iand2021 @ and2021 @ nor21) |> sNor3.solve >>= fun nor32021 ->
              Ok (andinn20 @ nor20) |> sNor.solve >>= fun nor20in20 ->

              Ok (iand202122 @ and32122nor20 @ and22nor21 @ nor22) |> sNor4.solve >>= fun nor41 ->

              Ok (cin :: and2020) |> sXor.solve >>= fun sum1 ->
              Ok (nor20in20 @ and2121) |> sXor.solve >>= fun sum2 ->
              Ok (nor32021 @ and2222) |> sXor.solve >>= fun sum3 ->
              Ok (nor41 @ and2323) |> sXor.solve >>= fun sum4 ->
              Ok (and5 @ and4 @ and32223nor21 @ and23nor22 @ nor23) |> sNor5.solve >>= fun overflow ->

              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])) ++ *)
              
              Ok (sum1 @ sum2 @ sum3 @ sum4 @ overflow @ xs)

          | _ -> Error { desc = "[s74hc283] missing input\n" } ;
          end
}


(* 2x 74hc283 sumator simulation *)
let s2x74hc283 : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' -> 
    match unit' with
      | [] -> Error { desc = "[s2x74hc283] missing input\n" };
      | carry0::a0::a1::a2::a3::a4::a5::a6::a7::b0::b1::b2::b3::b4::b5::b6::b7::xs -> 
         Ok [carry0; a0; a1; a2; a3; b0; b1; b2; b3] |> 
         s74hc283.solve >>= fun sum1_u -> begin match sum1_u with
            | s1::s2::s3::s4::carry1::sx -> Ok [carry1; a4; a5; a6; a7; b4; b5; b6; b7] |> 
              s74hc283.solve >>= fun sum2_u -> begin match sum2_u with
                  | s5::s6::s7::s8::carry2::sx -> Ok [s1; s2; s3; s4; s5; s6; s7; s8; carry2];
                  | _ -> Error { desc = "[s2x74hc283] missing input\n" } ;
                end
            | _ -> Error { desc = "[s2x74hc283] missing input\n" } ;
          end

      | _ -> Error { desc = "[s2x74hc283] missing input\n" };
                            
}



(* sGigatronALU simulation *)
let sGigatronALU : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' -> 
    match unit' with
      | [] -> Error { desc = "[s2x74hc283] missing input\n" };
      | al::ar0::ar1::ar2::ar3::ac0::ac1::ac2::ac3::ac4::ac5::ac6::ac7::bus0::bus1::bus2::bus3::bus4::bus5::bus6::bus7::xs -> 
         Ok [l; al; ac0; bus0; ar0; ar1; ar2; ar3; l; h; l; h] |> s74hc153.solve >>= fun unit0 -> 
         Ok [l; al; ac1; bus1; ar0; ar1; ar2; ar3; l; h; l; h] |> s74hc153.solve >>= fun unit1 ->
         Ok [l; al; ac2; bus2; ar0; ar1; ar2; ar3; l; h; l; h] |> s74hc153.solve >>= fun unit2 ->  
         Ok [l; al; ac3; bus3; ar0; ar1; ar2; ar3; l; h; l; h] |> s74hc153.solve >>= fun unit3 -> 
         Ok [al; l; bus4; ac4; l; l; h; h; ar0; ar2; ar1; ar3] |> s74hc153.solve >>= fun unit4 ->  
         Ok [al; l; bus5; ac5; l; l; h; h; ar0; ar2; ar1; ar3] |> s74hc153.solve >>= fun unit5 -> 
         Ok [al; l; bus6; ac6; l; l; h; h; ar0; ar2; ar1; ar3] |> s74hc153.solve >>= fun unit6 -> 
         Ok [al; l; bus7; ac7; l; l; h; h; ar0; ar2; ar1; ar3] |> s74hc153.solve >>= fun unit7 ->      
         
          begin match unit0 @ unit1 @ unit2 @ unit3 @ unit4 @ unit5 @ unit6 @ unit7 with
            | za0::zb0::za1::zb1::za2::zb2::za3::zb3::za4::zb4::za5::zb5::za6::zb6::za7::zb7::sx -> 
              Ok [ar0; za0; za1; za2; za3; za4; za5; za6; za7; zb0; zb1; zb2; zb3; zb4; zb5; zb6; zb7] |> s2x74hc283.solve 
            | _ -> Error { desc = "[sGigatronALU] missing input\n" } ;
          end

      | _ -> Error { desc = "[sGigatronALU] missing input\n" };
                            
}

let sGigatronALU_test : unit linesResult = 

  (* AR3 AR2 AR1 - instruction code + AR0 + AL*)
  (* 011 - xor    al=1 *)
  (* 100 - and    al=1 *)
  (* 111 - or     al=1 *)
  (* 110 - ld (b) al=1 *)
  (* 110 - add    al=0 *)
  (* 001 - sub    al=0 c=1 *)
  (* 000 - st (a) al=0 *)
  (* 010 - bcc (-a) al=1 c=1 *)

  let instr_code_xor = n2Unit 0b01101 "cod" 5 in (* xor      al=1 c=0 *)
  let instr_code_and = n2Unit 0b10001 "cod" 5 in (* and      al=1 c=0 *)
  let instr_code_or  = n2Unit 0b11101 "cod" 5 in (* or       al=1 c=0 *)
  let instr_code_ld  = n2Unit 0b11001 "cod" 5 in (* ld (b)   al=1 c=0 *)
  let instr_code_add = n2Unit 0b11000 "cod" 5 in (* add      al=0 c=0 *)
  let instr_code_sub = n2Unit 0b00110 "cod" 5 in (* sub      al=0 c=1 *)
  let instr_code_st  = n2Unit 0b00000 "cod" 5 in (* st (a)   al=0 c=0 *)
  let instr_code_bcc = n2Unit 0b01011 "cod" 5 in (* bcc (-a) al=1 c=1 *)

  let xor_ac  = n2Unit  0b10101010  "ac" 8 in 
  let xor_bus = n2Unit  0b11001100 "bus" 8 in

  let and_ac  = n2Unit 0b10101010 "ac" 8 in
  let and_bus = n2Unit 0b11001100 "bus" 8 in

  let or_ac  = n2Unit 0b10101010 "ac" 8 in
  let or_bus = n2Unit 0b11001100 "bus" 8 in

  let ld_ac = n2Unit 0 "ac" 8 in
  let ld_bus = n2Unit (-10) "bus" 8 in

  let add_ac  = n2Unit 20 "ac" 8 in
  let add_bus = n2Unit 15 "bus" 8 in

  let sub_ac  = n2Unit 0 "ac" 8 in
  let sub_bus = n2Unit 15 "bus" 8 in

  let st_ac  = n2Unit 20 "ac" 8 in
  let st_bus = n2Unit 15 "bus" 8 in

  let bcc_ac  = n2Unit (-7) "ac" 8 in (*twos compl. -> NOT then +1 -> (-5) = 11111010 + 1 = 11111011 *)
  let bcc_bus = n2Unit 0b00000000 "bus" 8 in

  let _ = 
  Ok (instr_code_xor @ xor_ac @ xor_bus) |> sGigatronALU.solve >>= fun test_xor ->
    match test_unit test_xor (n2Unit 0b001100110 "exp_val" 9) with
        |Ok m -> Printf.printf "sGigatronALU_test *xor* %s: PASS\n" (unitToStr2 test_xor); Ok m
        |Error e ->  Printf.printf "sGigatronALU_test *xor* %s: FAIL - %s\n" (unitToStr2 test_xor) e.desc; Error e
  in
  let _ =
  Ok (instr_code_and @ and_ac @ and_bus) |> sGigatronALU.solve >>= fun test_and ->
    match test_unit test_and (n2Unit 0b10001000 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *and* %s: PASS\n" (unitToStr2 test_and); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *and* %s: FAIL - %s\n" (unitToStr2 test_and) e.desc; Error e
  in
  let _ = 
  Ok (instr_code_or  @ or_ac  @ or_bus) |> sGigatronALU.solve >>= fun test_or  ->
    match test_unit test_or (n2Unit 0b11101110 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *or* %s: PASS\n" (unitToStr2 test_or); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *or* %s: FAIL - %s\n" (unitToStr2 test_or) e.desc; Error e
  in
  let _ =
  Ok (instr_code_add @ add_ac @ add_bus) |> sGigatronALU.solve >>= fun test_add ->
    match test_unit test_add (n2Unit 35 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *add* %s: PASS\n" (unitToStr2 test_add); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *add* %s: FAIL - %s\n" (unitToStr2 test_add) e.desc; Error e
  in
  let _ = 
  Ok (instr_code_sub @ sub_ac @ sub_bus) |> sGigatronALU.solve >>= fun test_sub ->
    match test_unit test_sub (n2Unit 15 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *sub* %s: PASS\n" (unitToStr2 test_sub); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *sub* %s: FAIL - %s\n" (unitToStr2 test_sub) e.desc; Error e
  in
  let _ =
  Ok (instr_code_st  @ st_ac @ st_bus) |> sGigatronALU.solve >>= fun test_st  ->
    match test_unit test_st (n2Unit 20 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *st* %s: PASS\n" (unitToStr2 test_st); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *st* %s: FAIL - %s\n" (unitToStr2 test_st) e.desc; Error e
  in
  let _ =
  Ok (instr_code_bcc @ bcc_ac @ bcc_bus) |> sGigatronALU.solve >>= fun test_bcc ->
    match test_unit test_bcc (n2Unit 7 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test %s *bcc* %s: PASS\n" (unitToStr2 bcc_ac) (unitToStr2 test_bcc); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *bcc* %s: FAIL - %s\n" (unitToStr2 bcc_ac) (unitToStr2 test_bcc) e.desc; Error e
  in 
  Ok (instr_code_ld  @ ld_ac  @ ld_bus) |> sGigatronALU.solve >>= fun test_ld  ->
  match test_unit test_ld ((n2Unit (-10) "exp_val" 8) @ l::[])  with (* can't convert 9 bit as sign, by alu result 9 bit is used as carry *)
      |Ok m -> Printf.printf "sGigatronALU_test %s *ld* %s: PASS\n" (unitToStr2 ld_bus)  (unitToStr2 test_ld); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *ld* %s: FAIL - %s\n" (unitToStr2 ld_bus) (unitToStr2 test_ld) e.desc; Error e 
 

type stateT = {

  tick: int;
  units: unit linesResult
}


let sMain : unit solver = {
  solve = fun result' -> 
    result' >>= fun unit' ->
      match unit' with
      | clk::en_clk::xs -> 
      
        sNor.solve (Ok [clk; en_clk]) >>= fun clk' ->

        let xx = clk' @ xs
        in
        (* Printf.printf "xx:%s\n" (unitToStr2 xx);    *)
        begin match xx with 
        | (_, LS_0)::(_, a)::(_, b)::(_, c)::(_, d)::xs' -> 
        
          let (a', ca) = if a = LS_1 then (LS_0, LS_1) 
                          else (LS_1, LS_0) 
          in
          let (b', ca) = if ca = LS_1 then (if b = LS_1 then (LS_0, LS_1) else (LS_1, LS_0))
                          else (b , ca) 
          in
          let (c', ca) = if ca = LS_1 then (if c = LS_1 then (LS_0, LS_1) else (LS_1, LS_0))
                          else (c , ca) 
          in
          let (d', ca) = if ca = LS_1 then (if d = LS_1 then (LS_0, LS_1) else (LS_1, LS_0))
                          else (d , ca)                                                               
          in
          Ok (clk' @ en_clk::("a", a')::("b", b')::("c", c')::("d", d')::xs')
        | (_, LS_1)::(_, a)::(_, b)::(_, c)::(_, d)::xs' -> Ok (clk' @ en_clk::("a", a)::("b", b)::("c", c)::("d", d)::xs')
        | _ -> Error {desc="sMain.solver Err.No.: 2"}
        end 
      | _ -> Error {desc="sMain.solver Err.No.: 1"}
}


let rec test_loop (s: stateT) = 
  (* Printf.printf ">>>...%d\n" s.tick; *)
  sMain.solve s.units >>= fun res ->
    Printf.printf ">>> %s\n" (unitToStr2 res);   
    if s.tick > 0 then 

      test_loop {tick = (s.tick - 1); units = Ok res}
    else Ok []

  
let () = 

  let _ = s74hc153_test in
  let _ = make_unit_test in
  let _ = sGigatronALU_test in

  let _ = match test_loop {tick=32; units = Ok (h::l::l::l::l::l::l::[])} with
        | Ok m -> ()
        | Error e -> Printf.printf "test_loop FAIL - %s\n" e.desc; 
  in
  Printf.printf "Bye, bye."  (* (unitToStr (n2Unit 0xaa "lab" 20)); *)




  (* let al = ("al", LS_1) in (* 0:LS_1 lub akumulator:LS_0 *)
  let ar0 = ("cin", LS_1) in (* AR0, carry in *)

  let reg_a = n2Unit 0b11111111 "bus" 8 in
  let reg_b = n2Unit 0b00000000 "ac" 8 in

  let instr_code = n2Unit 0b010 "cod" 3 in
  let _ = Ok (al :: ar0 :: instr_code @ reg_a @ reg_b) |> sGigatronALU.solve >>= fun sum1 -> 

    Printf.printf ">>> result:\n%s\n" (unitToStr sum1);
    Printf.printf ">>> test: 200 + 57 == %d <<< %s\n" (unitToNum sum1) (if (unitToNum sum1) == 200 + 57 then "PASS" else "FAIL");
    Ok sum1 
  in  *)

  (* let tracks : lineMapT ref = ref ListMap.empty in
  let units : unitMapT ref = ref ListMap.empty in
  "x"::"y"::"z"::[] |> insertLines tracks;
  showLines tracks; *)
  
              (* let test = make_unit [ Ok (to_unit [("b0", LS_0)] ++ 
                             to_unit [("b1", LS_1)] +++
                             to_unit [("b2", LS_1)] +++
                             to_unit [("b3", LS_0)] )] |> s74hc283.solve in *)
  
(*  result: [out]:low [2]:high [3]:low [out]:low [3]:low  mapowanie wyniku i zastosowanie >>= *)
(* brak przypisania etykiety do czegokolwiek eq. można mieć dwie takie same.. *)




(* type unitT = VCC | GND | Not | And2  *)
(* type unitStateT = (unitT, f) *)

(* type lineMapT = (string, lineStateT) ListMap.t

type unitMapT = (string, unitT) ListMap.t *)

(* let lineStatetoStr (state: lineStateT option) : string =

  match state with 
  | Some(LS_1) -> "state:high" 
  | Some(LS_0) -> "state:low"
  | Some(LS_X) -> "state:open"
  | _ -> "?" *)

(* let rec insertLines (t : lineMapT ref) (k : string list) =
  match k with
    | x::xs -> 
      t :=(ListMap.insert x LS_X !t);
      insertLines t xs
    | [] -> ()
  
let showLines (t : lineMapT ref) =

  let rec f = fun m ->
      match m with
        | [] -> ()
        | (k, v)::xs -> 
          Printf.printf "%s -> %s\n" k (lineStatetoStr (Some v));
          f xs
  in
  f (ListMap.bindings !t) *)


    (* rec make_unit (inp : unit linesResult list): unit linesResult *)

  (* let s74hc251_out = Ok [("oe", LS_1); 
                              ("s0", LS_1); 
                              ("s1", LS_0); 
                              ("s2", LS_1);
                              ("in0", LS_1);
                              ("in1", LS_0);
                              ("in2", LS_0);
                              ("in3", LS_0);
                              ("in4", LS_0);
                              ("in5", LS_1);
                              ("in6", LS_0);
                              ("in7", LS_0);                        
                              ] |> s74hc251.solve in *)




(* let ( <*> ) (p1: 'a solver) (p2: 'b solver): 'c solver  = { 
  
  solve = fun input -> 
  
    input |> (p1 >>= fun a -> p2 >>= fun b -> 
      return (a @ b)).solve;
}  *)

 (* let ( <*> ) (p1: 'a solver) (p2: 'b solver): 'c solver  = { 
  
  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit' with
                    |[] -> Error {desc = "<*> missing input\n 0"} 
                    |[x1] -> Error {desc = "<*> missing input\n 1"} 
                    |[x1; x2] -> Error {desc = "<*> missing input\n 2"} 
                    |[x1; x2; x3] -> Error {desc = "<*> missing input\n 3"} 
                    |(x1::x2::x3::x4::xs) -> 
                      match p1.solve (Ok [x1;x2;]) with
                        |Error error -> Error error
                        |Ok unit1 -> 
                          match p2.solve (Ok [x3; x4]) with 
                            |Ok unit2 ->  Ok (unit1@unit2)
                            |Error error -> Error error

} *)


