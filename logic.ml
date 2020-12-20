(* type and4 = { in0: int; in1: int ; in2: int; in3: int; output: int } *)

open Table;;
open Array;;

type lineStateT = LS_1 | LS_0 | LS_X 

let l = [LS_0]
let h = [LS_1]

type unit =  (string * (lineStateT list))
type error = {desc: string;}

type 'a linesResult = (('a, error) result) 


(* type mess = SUnit of signal list | MUnit of ((unit, error) result) list *)
type mess =  SUnit of unit linesResult | MUnit of (unit linesResult list) 
(* | (unit linesResult)  list) *)


type 'a solver = {

  solve: 'a list -> 'a linesResult; (* type: result of (input * 'a, error) *)
}


let unitToStr (out: unit) : string =

  let rec f = fun l i ->
    match l with
    | [] -> ""
    | LS_1::xs -> Printf.sprintf "%d:<1>, %s" i (f xs (i+1));
    | LS_0::xs -> Printf.sprintf "%d:<0>, %s" i (f xs (i+1));
    | LS_X::xs -> Printf.sprintf "%d:<*>, %s" i (f xs (i+1));
  in
  match out with
  | (lab, signals) -> Printf.sprintf "%s [ %s ]\n" lab (f signals 0)


(* let unitToStr (out: unit) : string =

  let rec f = fun l ->
    match l with 
    | [] -> ""
    | (lab, s)::xs -> Printf.sprintf "%s [ %s ]\n%s" lab (signalsToStr s) (f xs);
  in
  f out *)


(* short ver. *)
let unitToStr2 (out: unit) : string =

  let rec f = fun l ->
    match l with
    | [] -> ""
    | LS_1::xs -> Printf.sprintf "1%s" (f xs);
    | LS_0::xs -> Printf.sprintf "0%s" (f xs);
    | LS_X::xs -> Printf.sprintf "*%s" (f xs);
  in
  match out with
  | (lab, signals) -> Printf.sprintf "%s [ %s ]" lab (f signals)



(* short ver. *)
(* let unitToStr2 (out: unit) : string =

  let rec f = fun l ->
    match l with 
    | [] -> ""
    | (lab, s)::xs -> Printf.sprintf "%s: %s, %s" lab (signalsToStr2 s) (f xs);
  in
  f out *)


let resultToStr (out: unit linesResult) : string =

  match out with
  | Error error -> error.desc
  | Ok o -> unitToStr o


let rec pow(x, n) =
    if n=0 then 1 else x * pow(x, n-1)


(* let unitToNum (out: unit) : int =

    let rec f = fun l i acc ->
      match l with 
      | [] -> acc
      | (s, LS_1)::xs ->  (f xs (i+1) acc + pow (2, i))
      | (s, LS_0)::xs ->  (f xs (i+1) acc)
      | (s, LS_X)::xs ->  (f xs (i+1) acc)

    in
    (f out 0 0)  *)
    (* ((List.length out) - 1) *)

(* 
let resultToNum (out: unit linesResult) : int =

  match out with
  | Error error -> -1
  | Ok o -> unitToNum o *)


let n2Unit (n : int) lab s : unit = 

  let u : int32 = if n < 0 then Int32.add (Int32.lognot (Int32.of_int (-n))) Int32.one
    else (Int32.of_int n) 
  in (* (if n<0 then (lnot (-n) + 1) else n)  *)

  let rec f = fun (n: int32) (acc: lineStateT list) (i:int) : lineStateT list ->
    if s > i then (
                      (f (Int32.shift_right_logical n 1) 
                          ((if (Int32.logand n (Int32.of_int 0x01)) > Int32.zero then LS_1 else LS_0)::[]) 
                          (i + 1)
                      ) @ acc
                  )
                  
    else acc
  in
  (lab, (f u [] 0))


let ( >>= )  (p: 'a linesResult) (f: 'a -> 'b linesResult)  : 'b linesResult =
  
      match p with
      | Ok a -> f a
      | Error error -> Error error


(* monoid ++ *)
let ( ++ ) (u1: unit ) (u2: unit) : unit =

    match (u1, u2) with
    | ((lab1, u1signals), (lab2, u2signals)) -> ((lab1 ^ lab2), u1signals @ u2signals)


(* let ( ++ ) (u1: unit linesResult ) (u2: unit linesResult) : unit linesResult =

  u1 >>= fun u1' -> u2 >>= fun u2' -> 
  
    match (u1', u2') with
    | ((lab1, u1signals), (lab2, u2signals)) -> Ok ((lab1 ^ lab2), u1signals @ u2signals) *)
  

(*  (us: unit solver) *)

(* let solve_unit (lr: (unit linesResult) list) (us: unit solver): unit linesResult = 

  let rec f  (l: (unit linesResult) list)  : unit linesResult = 
      (* let x::xs = u in Printf.printf "solve_unit:%s" (resultToStr u); *)
      match l with
        | [] -> Ok []
        | x::xs -> x >>= fun u -> (f xs) >>= fun w -> Ok (u @ w)
  in
  let result = ref []
  in
  (f lr) >>= fun u -> result := u ;
  us.solve (Ok !result) *)

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
let test_unit (p1: unit) (p2: unit) : unit linesResult = 

  match (p1, p2) with
    | ((lab1, u1signals), (lab2, u2signals)) -> 
  

    let rec f (l1: lineStateT list) (l2: lineStateT list) : unit linesResult = 
          match l1 with
          | [] -> begin match l2 with       
                        | [] -> Ok (lab1^lab2, [])
                        | x2::x2s -> Error {desc = Printf.sprintf "unit %s too short compared to %s" lab1 lab2}
                        end
          | x1::x1s -> begin match l2 with
                              | [] -> Error {desc = "second unit too short"}
                              | x2::x2s -> 

                                if x1 != x2 then Error {desc = Printf.sprintf "signal levels not equal [%s] <> [%s]" lab1 lab2}
                                else
                                (* if (compare l_x1 l_x2) != 0 then Error {desc = Printf.sprintf "labes not equal [%s] <> [%s]" l_x1 l_x2} *)
                                f x1s x2s
                        end
    in
    f u1signals u2signals
         

let rec make_unit (inp : unit linesResult list): unit linesResult =

  let rec f (acc : unit linesResult)  (m : unit linesResult list): unit linesResult  = 
    match m with
    | [] -> acc            
    | x::[] -> x >>= fun u -> 
             acc >>= fun acc' -> begin match (u, acc') with
                                        | ((lab1, u1signals), (lab2, u2signals)) -> (Ok (lab1^lab2, u2signals @ u1signals))
                                   end
    | (x::xs) -> x >>= fun u -> 
               acc >>= fun acc' -> begin match (u, acc') with
               | ((lab1, u1signals), (lab2, u2signals)) -> (f (Ok (lab1^lab2,  u2signals @ u1signals)) xs)
               end
  in
  f (Ok ("",[])) inp

let make_unit_test : unit linesResult = 

  (* let base_signals_a = n2Unit 0b1010 "a" 4 in
  let base_signals_b = n2Unit 0b1111 "b" 4 in *)

  (* prepare input list of results of signals *)
  (* let function_input_signals = List.map (fun e -> Ok [e]) ((n2Unit 0b1010 "a" 4 ) @ (n2Unit 0b1111 "b" 4)) in *)

  let function_input_signals = [Ok (n2Unit 0b1010 "a" 4 ); Ok (n2Unit 0b1111 "b" 4)] in 

  (* test make_unit *)
  let res = make_unit function_input_signals >>= fun created_unit ->
                  (* test_unit (base_signals_a @ base_signals_b) created_unit *)
                  Printf.printf "make_unit_test: %s -> %s\n" (unitToStr2 (n2Unit 0b10101111 "a+b" 8)) (unitToStr2 created_unit);
                  test_unit (n2Unit 0b10101111 "a+b" 8) created_unit
                  in
                  match res with
                      |Ok m -> Printf.printf "make_unit_test: PASS\n"; Ok m
                      |Error e ->  Printf.printf "make_unit_test: FAIL - %s\n" e.desc; Error e




let sNeg : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNeg FAIL - %s missing input\n" lab)}
      | [(lab, LS_1::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, _::xs)] -> Ok (lab, LS_1::xs)
      | _ -> Error {desc = "sNeg FAIL - missing input unit\n"}
  
} 

(* Tri-state negator oe in *)
let sTneg : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sTneg FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sTneg FAIL - %s missing 1 input\n" lab)}
      | [(lab, LS_1::LS_1::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, LS_1::LS_0::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_X::xs)
      | _ -> Error {desc = "sTneg FAIL - missing input unit\n"}
} 

let sBuf : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sBuf FAIL - %s missing input\n" lab)}
      | [(lab, b::xs)] ->  Ok (lab, b::xs)
      | _ -> Error {desc = "sBuf FAIL - missing input unit\n"}
} 

(* Tri-state bufor inputs: oe in *)
let sTbuf : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sTbuf FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sTbuf FAIL - %s missing 1 input\n" lab)}
      | [(lab, LS_1::LS_0::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, LS_1::LS_1::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_X::xs)
      | _ -> Error {desc = "sTbuf FAIL - missing input unit\n"}
} 

let sAnd : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_1::LS_1::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sAnd FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sAnd FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sAnd FAIL - missing input unit\n"}
} 

let sAnd3 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_1::LS_1::LS_1::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sAnd3 FAIL - %s missing 3 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sAnd3 FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::y::[])] -> Error {desc = (Printf.sprintf "sAnd3 FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sAnd3 FAIL - missing input unit\n"}
} 

let sAnd4 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_1::LS_1::LS_1::LS_1::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sAnd4 FAIL - %s missing 4 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sAnd4 FAIL - %s missing 3 inputs\n" lab)}
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sAnd4 FAIL - %s missing 2 input\n" lab)}
      | [(lab, _::_::_::[])] -> Error {desc = (Printf.sprintf "sAnd4 FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sAnd4 FAIL - missing input unit\n"}
} 


let sAnd5 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_1::LS_1::LS_1::LS_1::LS_1::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sAnd5 FAIL - %s missing 5 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sAnd5 FAIL - %s missing 4 inputs\n" lab)}
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sAnd5 FAIL - %s missing 3 input\n" lab)}
      | [(lab, _::_::_::[])] -> Error {desc = (Printf.sprintf "sAnd5 FAIL - %s missing 2 input\n" lab)}
      | [(lab, _::_::_::_::[])] -> Error {desc = (Printf.sprintf "sAnd5 FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sAnd5 FAIL - missing input unit\n"}
} 


let sNand : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_1::LS_1::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNand FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sNand FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sNand FAIL - missing input unit\n"}
} 


let sOr : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_0::LS_0::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sOr FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sOr FAIL - %s missing 1 input\n" lab)} 
      | _ -> Error {desc = "sOr FAIL - missing input unit\n"}
}  

let sOr4 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_0::LS_0::LS_0::LS_0::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, _::_::_::_::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sOr4 FAIL - %s missing 4 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sOr4 FAIL - %s missing 3 inputs\n" lab)} 
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sOr4 FAIL - %s missing 2 inputs\n" lab)} 
      | [(lab, _::_::_::[])] -> Error {desc = (Printf.sprintf "sOr4 FAIL - %s missing 1 input\n" lab)} 
      | _ -> Error {desc = "sOr4 FAIL - missing input unit\n"}
}   


let sXor : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_0::LS_0::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, LS_1::LS_1::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sXor FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sXor FAIL - %s missing 1 input\n" lab)} 
      | _ -> Error {desc = "sXor FAIL - missing input unit\n"}
}  


let sNor : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_0::LS_0::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNor FAIL - %s missing 2 inputs\n" lab)}
      | [(lab, x::[])] -> Error {desc = (Printf.sprintf "sNor FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sNor FAIL - missing input unit\n"} 
}   


let sNor3 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_0::LS_0::LS_0::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNor3 FAIL - %s missing 3 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sNor3 FAIL - %s missing 2 inputs\n" lab)} 
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sNor3 FAIL - %s missing 1 input\n" lab)} 
      | _ -> Error {desc = "sNor3 FAIL - missing input unit\n"}
}   


let sNor4 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, LS_0::LS_0::LS_0::LS_0::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNor4 FAIL - %s missing 4 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sNor4 FAIL - %s missing 3 inputs\n" lab)} 
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sNor4 FAIL - %s missing 2 inputs\n" lab)} 
      | [(lab, _::_::_::[])] -> Error {desc = (Printf.sprintf "sNor4 FAIL - %s missing 1 input\n" lab)} 
      | _ -> Error {desc = "sNor4 FAIL - missing input unit\n"}
}   


let sNor5 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
          match result' with
      | [(lab, LS_0::LS_0::LS_0::LS_0::LS_0::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNor5 FAIL - %s missing 5 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sNor5 FAIL - %s missing 4 inputs\n" lab)} 
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sNor5 FAIL - %s missing 3 inputs\n" lab)} 
      | [(lab, _::_::_::[])] -> Error {desc = (Printf.sprintf "sNor5 FAIL - %s missing 2 inputs\n" lab)} 
      | [(lab, _::_::_::_::[])] -> Error {desc = (Printf.sprintf "sNor5 FAIL - %s missing 1 input\n" lab)}
      | _ -> Error {desc = "sNor5 FAIL - missing input unit\n"} 
}   


let sNor8 : unit solver = {

  solve = fun result' -> 
      (* result' >>= fun unit' -> *)
          match result' with
      | [(lab, LS_0::LS_0::LS_0::LS_0::LS_0::LS_0::LS_0::LS_0::xs)] -> Ok (lab, LS_1::xs)
      | [(lab, _::_::_::_::_::_::_::_::xs)] -> Ok (lab, LS_0::xs)
      | [(lab, [])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 8 inputs\n" lab)}
      | [(lab, _::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 7 inputs\n" lab)} 
      | [(lab, _::_::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 6 inputs\n" lab)} 
      | [(lab, _::_::_::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 5 inputs\n" lab)} 
      | [(lab, _::_::_::_::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 4 inputs\n" lab)} 
      | [(lab, _::_::_::_::_::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 3 inputs\n" lab)} 
      | [(lab, _::_::_::_::_::_::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 2 inputs\n" lab)} 
      | [(lab, _::_::_::_::_::_::_::[])] -> Error {desc = (Printf.sprintf "sNor8 FAIL - %s missing 1 input\n" lab)} 
      | _ -> Error {desc = "sNor8 FAIL - missing input unit\n"}
}   



(* 74hc251 multiplexer simulation *)
let s74hc251 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, [])] -> Error { desc = "[s74hc251] missing input\n" } ;
      | [(lab, oe'::s2'::s1'::s0'::in7::in6::in5::in4::in3::in2::in1::in0::xs)] -> 
          let oe = ("oe.", [oe']) in
          let s0 = ("s0.", [s0']) in
          let s1 = ("s1.", [s1']) in
          let s2 = ("s2.", [s2']) in

          sNeg.solve [oe] >>= fun in_oe_neg ->
          sNeg.solve [s0] >>= fun in_s0_neg ->
          sNeg.solve [s1] >>= fun in_s1_neg ->
          sNeg.solve [s2] >>= fun in_s2_neg ->

          sBuf.solve [("in0.", [in0])] >>= fun buf_out_0 -> 
          sBuf.solve [("in1.", [in1])] >>= fun buf_out_1 -> 
          sBuf.solve [("in2.", [in2])] >>= fun buf_out_2 -> 
          sBuf.solve [("in3.", [in3])] >>= fun buf_out_3 -> 
          sBuf.solve [("in4.", [in4])] >>= fun buf_out_4 -> 
          sBuf.solve [("in5.", [in5])] >>= fun buf_out_5 -> 
          sBuf.solve [("in6.", [in6])] >>= fun buf_out_6 -> 
          sBuf.solve [("in7.", [in7])] >>= fun buf_out_7 -> 

          [buf_out_0 ++ in_s2_neg ++ in_s1_neg ++ in_s0_neg] |> sAnd4.solve >>= fun and4_out_0 ->
          [buf_out_1 ++ in_s2_neg ++ in_s1_neg ++ s0] |> sAnd4.solve >>= fun and4_out_1 ->
          [buf_out_2 ++ in_s2_neg ++ s1 ++ in_s0_neg] |> sAnd4.solve >>= fun and4_out_2 ->
          [buf_out_3 ++ in_s2_neg ++ s1 ++ s0] |> sAnd4.solve >>= fun and4_out_3 ->
          [buf_out_4 ++ s2 ++ in_s1_neg ++ in_s0_neg] |> sAnd4.solve >>= fun and4_out_4 ->
          [buf_out_5 ++ s2 ++ in_s1_neg ++ s0] |> sAnd4.solve >>= fun and4_out_5 ->
          [buf_out_6 ++ s2 ++ s1 ++ in_s0_neg] |> sAnd4.solve >>= fun and4_out_6 ->
          [buf_out_7 ++ s2 ++ s1 ++ s0] |> sAnd4.solve >>= fun and4_out_7 ->

          [and4_out_0 ++ 
          and4_out_1 ++ 
          and4_out_2 ++ 
          and4_out_3 ++ 
          and4_out_4 ++ 
          and4_out_5 ++ 
          and4_out_6 ++ 
          and4_out_7] |> sNor8.solve >>= fun nor8_out ->
          (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out]))  *)
          
          [nor8_out ++ in_oe_neg] |> sTbuf.solve >>= fun out_y ->
          [nor8_out ++ in_oe_neg] |> sTneg.solve >>= fun neg_y ->
          Ok (out_y ++ neg_y ++ (".", xs))  (* most imported bits first *)
      | [(lab, _)] -> Error {desc = (Printf.sprintf "s74hc251 FAIL - %s missing inputs\n" lab)}
      | _ -> Error { desc = "s74hc251 FAIL missing unit input\n" } ;
      
}

(* s74hc153 multiplexer simulation *)
let s74hc153 : unit solver = {

  solve = fun result' -> 
      (* result' >>= fun unit' -> *)
          begin match result' with
          | [(lab, [])] -> Error { desc = "[s74hc153] missing input\n" } ;
          | [(lab, ea::eb::s1::s0::in3a::in2a::in1a::in0a::in3b::in2b::in1b::in0b::xs)] -> 
            
            sNeg.solve [("ean", [ea])] >>= fun in_ea_neg' -> let (_, in_ea_neg) = in_ea_neg' in
            sNeg.solve [("ebn", [eb])] >>= fun in_eb_neg' -> let (_, in_eb_neg) = in_eb_neg' in 
            sNeg.solve [("s0n", [s0])] >>= fun in_s0_neg' -> let (_, in_s0_neg) = in_s0_neg' in 
            sNeg.solve [("s1n", [s1])] >>= fun in_s1_neg' -> let (_, in_s1_neg) = in_s1_neg' in 
(* Printf.printf "MUX153 IN %s %s %s\n" (unitToStr2 ("es.eb.s1.s0", ea::eb::s1::s0::[])) (unitToStr2 ("ina", in3a::in2a::in1a::in0a::[])) (unitToStr2 ("inb", in3b::in2b::in1b::in0b::[])); *)
            [("a0", in0a :: in_s1_neg @ in_s0_neg @ in_ea_neg)]  |> sAnd4.solve >>= fun a0' -> let (_, a0) = a0' in
            [("a1", in1a :: in_s1_neg @ s0 :: in_ea_neg)]        |> sAnd4.solve >>= fun a1' -> let (_, a1) = a1' in
            [("a2", in2a :: s1 :: in_s0_neg @ in_ea_neg)]        |> sAnd4.solve >>= fun a2' -> let (_, a2) = a2' in
            [("a3", in3a :: s1 :: s0 :: in_ea_neg)]              |> sAnd4.solve >>= fun a3' -> let (_, a3) = a3' in
            
            [("a4", in0b :: in_s1_neg @ in_s0_neg @ in_eb_neg)]  |> sAnd4.solve >>= fun a4' -> let (_, a4) = a4' in
            [("a5", in1b :: in_s1_neg @ s0 :: in_eb_neg)]        |> sAnd4.solve >>= fun a5' -> let (_, a5) = a5' in
            [("a6", in2b :: s1 :: in_s0_neg @ in_eb_neg)]        |> sAnd4.solve >>= fun a6' -> let (_, a6) = a6' in
            [("a7", in3b :: s1 :: s0 :: in_eb_neg)]              |> sAnd4.solve >>= fun a7' -> let (_, a7) = a7' in

            [("a0123", a0 @ a1 @ a2 @ a3)]                       |> sOr4.solve >>= fun or4a_out' -> let (_, or4a_out) = or4a_out' in
            [("a4567", a4 @ a5 @ a6 @ a7)]                       |> sOr4.solve >>= fun or4b_out' -> let (_, or4b_out) = or4b_out' in
            (* Printf.printf "MUX153 OUT %s %s\n" (unitToStr2 ("or4a_out", or4a_out)) (unitToStr2 ("or4b_out", or4b_out)); *)
            Ok (lab, or4a_out @ or4b_out @ xs)            (* most imported bits first *)  
    
            (* Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (resultToStr e)) and4_out;  *)
            
          |[(lab, _)] -> Error { desc = "s74hc153 FAIL missing input\n" } ;
          | _ -> Error { desc = "s74hc153 FAIL missing unit input\n" } ;
         end
}

let s74hc153_test : unit linesResult = 

  let a = n2Unit 0b0011 "a." 4 in
  let b = n2Unit 0b0110 "b." 4 in
  let ea_eb = n2Unit 0b00 "ea_eb." 2 in

  let res = 
  [ea_eb ++ ("adr1.", l @ l) ++ a ++ b] |> s74hc153.solve >>= fun test01 ->
  [ea_eb ++ ("adr2.", l @ h) ++ a ++ b] |> s74hc153.solve >>= fun test02 ->
  [ea_eb ++ ("adr3.", h @ l) ++ a ++ b] |> s74hc153.solve >>= fun test03 ->
  [ea_eb ++ ("adr4.", h @ h) ++ a ++ b] |> s74hc153.solve >>= fun test04 ->
  (*Printf.printf "s74hc153_test: %s -> %s\n" (unitToStr2  (n2Unit 0b10110100 "exp_val" 8) ) (unitToStr2  (test02));  ++ test02 ++ test03 ++ test04)); *)
  test_unit (test01 ++ test02 ++ test03 ++ test04) (n2Unit 0b10110100 "exp_val" 8) (* a3 b3 a2 b2 a1 b1 a0 b0 *)
  in
  match res with
      |Ok m -> Printf.printf "s74hc153_test: PASS\n"; Ok m
      |Error e ->  Printf.printf "s74hc153_test: FAIL - %s\n" e.desc; Error e



(* 74hc283 adder simulation *)
let s74hc283 : unit solver = {

  solve = fun result' -> 
      (* result' >>= fun unit' -> *)
          begin match result' with
          | [(lab, [])] -> Error { desc = "[s74hc283] missing input\n" }
          | [(lab, cin::a4::a3::a2::a1::b4::b3::b2::b1::xs)] -> 

              [("cin.", [cin])] |> sNeg.solve >>= fun cin_neg ->  

              [("na20.", [a1; b1])] |> sNand.solve >>= fun nand20 ->
              [("no20.", [a1; b1])] |> sNor.solve >>= fun nor20 ->
              [("na21.", [a2; b2])] |> sNand.solve >>= fun nand21 ->
              [("no21.", [a2; b2])] |> sNor.solve >>= fun nor21 ->
              [("na22.", [a3; b3])] |> sNand.solve >>= fun nand22 ->
              [("no22.", [a3; b3])] |> sNor.solve >>= fun nor22 -> 
              [("na23.", [a4; b4])] |> sNand.solve >>= fun nand23 ->
              [("no23.", [a4; b4])] |> sNor.solve >>= fun nor23 -> 

              [nor20] |> sNeg.solve >>= fun nor20n ->
              [nor21] |> sNeg.solve >>= fun nor21n ->
              [nor22] |> sNeg.solve >>= fun nor22n ->
              [nor23] |> sNeg.solve >>= fun nor23n ->

              [nor20n ++ nand20] |> sAnd.solve >>= fun and2020 ->
              [nor20n ++ nand21] |> sAnd.solve >>= fun and2021 ->
              [nor21n ++ nand21] |> sAnd.solve >>= fun and2121 ->
              [nor22n ++ nand22] |> sAnd.solve >>= fun and2222 ->
              [nor23n ++ nand23] |> sAnd.solve >>= fun and2323 ->
              [nand23 ++ nor22] |> sAnd.solve >>= fun and23nor22 ->
              [nand22 ++ nor21] |> sAnd.solve >>= fun and22nor21 ->
              [nand22 ++ nand23 ++ nor21] |> sAnd3.solve >>= fun and32223nor21 ->
              [nand21 ++ nand22 ++ nor20] |> sAnd3.solve >>= fun and32122nor20 ->
              [nand21 ++ nand22 ++ nand23 ++ nor20] |> sAnd4.solve >>= fun and4 ->

              [cin_neg ++ nand20] |> sAnd.solve >>= fun andinn20 ->
              [cin_neg ++ nand20 ++ nand21] |> sAnd3.solve >>= fun iand2021 ->
              [cin_neg ++ nand20 ++ nand21 ++ nand22] |> sAnd4.solve >>= fun iand202122  ->        
              [cin_neg ++ nand20 ++ nand21 ++ nand22 ++ nand23] |> sAnd5.solve >>= fun and5 ->

              [iand2021 ++ and2021 ++ nor21] |> sNor3.solve >>= fun nor32021 ->
              [andinn20 ++ nor20] |> sNor.solve >>= fun nor20in20 ->

              [iand202122 ++ and32122nor20 ++ and22nor21 ++ nor22] |> sNor4.solve >>= fun nor41 ->

              [("cin.", [cin]) ++ and2020] |> sXor.solve >>= fun sum1 ->
              [nor20in20 ++ and2121] |> sXor.solve >>= fun sum2 ->
              [nor32021 ++ and2222] |> sXor.solve >>= fun sum3 ->
              [nor41 ++ and2323] |> sXor.solve >>= fun sum4 ->
              [and5 ++ and4 ++ and32223nor21 ++ and23nor22 ++ nor23] |> sNor5.solve >>= fun overflow ->

              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])) ++ *)
              
              Ok (sum4 ++ sum3 ++ sum2 ++ sum1 ++ overflow ++ ("rest.", xs)) (* most imported bits first *)

          |[(lab, _)] -> Error { desc = "s74hc283 FAIL missing input\n" } ;
          | _ -> Error { desc = "s74hc283 FAIL missing unit input\n" } ;
          end
}


let s74hc283_test : unit linesResult = 

  let a = n2Unit 0b0011 "a." 4 in
  let b = n2Unit 0b0110 "b." 4 in

  let res = 
  [("carry.", l) ++ a ++ b] |> s74hc283.solve >>= fun test01 ->
  [("carry.", h) ++ a ++ b] |> s74hc283.solve >>= fun test02 ->
  [("carry.", l) ++ b ++ a] |> s74hc283.solve >>= fun test03 ->
  [("carry.", h) ++ b ++ a] |> s74hc283.solve >>= fun test04 ->
(* Printf.printf "s74hc283_test: %s -> %s\n" (unitToStr2  (n2Unit 0b10010101001001010100 "exp_val" 20) ) (unitToStr2 (test01 ++ test02 ++ test03 ++ test04)); *)
  test_unit (test01 ++ test02 ++ test03 ++ test04) (n2Unit 0b10010101001001010100 "exp_val" 20) (* a3 b3 a2 b2 a1 b1 a0 b0 *)
  in
  match res with
      |Ok m -> Printf.printf "s74hc283_test: PASS\n"; Ok m
      |Error e ->  Printf.printf "s74hc283_test: FAIL - %s\n" e.desc; Error e


(* 2x 74hc283 sumator simulation *)
let s2x74hc283 : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' ->  *)
    match result' with
      | [] -> Error { desc = "[s2x74hc283] missing input\n" };
      | [(lab, carry0::a7::a6::a5::a4::a3::a2::a1::a0::b7::b6::b5::b4::b3::b2::b1::b0::xs)] -> 
         (* Printf.printf "s2x74hc283 INPUT: %s\n" (unitToStr2 ("input.", carry0::a7::a6::a5::a4::a3::a2::a1::a0::b7::b6::b5::b4::b3::b2::b1::b0::xs)); *)
         [("1-283.", [carry0; a3; a2; a1; a0; b3; b2; b1; b0])] |> s74hc283.solve >>= fun sum1_u -> 

          (* Printf.printf "s2x74hc283 result1: %s\n" (unitToStr2 sum1_u); *)
          begin match sum1_u with
            | (_, s4::s3::s2::s1::carry1::sx) ->  
             (* Printf.printf "s2x74hc283 input: %s\n" (unitToStr2 ("2-283.", [carry1; a7; a6; a5; a4; b7; b6; b5; b4])); *)
              [("2-283.", [carry1; a7; a6; a5; a4; b7; b6; b5; b4])] |> s74hc283.solve >>= fun sum2_u -> 
              (* Printf.printf "s2x74hc283 result2 %s\n" (unitToStr2 sum2_u); *)
                begin match sum2_u with
                  | (_, s8::s7::s6::s5::carry2::sx) -> 
                                      Ok (lab^"2x283.", [s8; s7; s6; s5; s4; s3; s2; s1; carry2]);
                  | _ -> Error { desc = "[s2x74hc283] missing input\n" } ;
                end
            | _ -> Error { desc = "[s2x74hc283] missing input\n" } ;
          end

      | _ -> Error { desc = "[s2x74hc283] missing input\n" };
                            
}

let s2x74hc283_test : unit linesResult = 

  let a = n2Unit 0b00110011 "a." 8 in
  let b = n2Unit 0b00000110 "b." 8 in

  let res = 
  [("carry.", l) ++ a ++ b] |> s2x74hc283.solve >>= fun test01 ->
  [("carry.", h) ++ a ++ b] |> s2x74hc283.solve >>= fun test02 ->
(* Printf.printf "s2x74hc283_test: %s -> %s\n" (unitToStr2  (n2Unit 0b001110010001110100 "exp_val" 18) ) (unitToStr2 (test01 ++ test02)); *)
  test_unit (test01 ++ test02) (n2Unit 0b001110010001110100 "exp_val" 18) 
  in
  match res with
      |Ok m -> Printf.printf "s2x74hc283_test: PASS\n"; Ok m
      |Error e ->  Printf.printf "s2x74hc283_test: FAIL - %s\n" e.desc; Error e


(* sGigatronALU simulation *)
let sGigatronALU : unit solver = {

  solve = fun result' -> 
    (* result' >>= fun unit' -> ea::eb::s1::s0::in3a::in2a::in1a::in0a::in3b::in2b::in1b::in0b  *)
    match result' with
      | [] -> Error { desc = "[s2x74hc283] missing input\n" };
      | [(lab, ar3::ar2::ar1::ar0::al::ac7::ac6::ac5::ac4::ac3::ac2::ac1::ac0::bus7::bus6::bus5::bus4::bus3::bus2::bus1::bus0::xs)] -> 
         
         [("u0.", l @ [al; bus0; ac0; ar3; ar2; ar1; ar0] @ h @ l @ h @ l)] |> s74hc153.solve >>= fun unit0 -> 
         [("u1.", l @ [al; bus1; ac1; ar3; ar2; ar1; ar0] @ h @ l @ h @ l)] |> s74hc153.solve >>= fun unit1 ->
         [("u2.", l @ [al; bus2; ac2; ar3; ar2; ar1; ar0] @ h @ l @ h @ l)] |> s74hc153.solve >>= fun unit2 ->  
         [("u3.", l @ [al; bus3; ac3; ar3; ar2; ar1; ar0] @ h @ l @ h @ l)] |> s74hc153.solve >>= fun unit3 -> 
         [("u4.", al::l @ [ac4; bus4] @ h @ h @ l @ l @ [ar3; ar1; ar2; ar0])] |> s74hc153.solve >>= fun unit4 ->  
         [("u5.", al::l @ [ac5; bus5] @ h @ h @ l @ l @ [ar3; ar1; ar2; ar0])] |> s74hc153.solve >>= fun unit5 -> 
         [("u6.", al::l @ [ac6; bus6] @ h @ h @ l @ l @ [ar3; ar1; ar2; ar0])] |> s74hc153.solve >>= fun unit6 -> 
         [("u7.", al::l @ [ac7; bus7] @ h @ h @ l @ l @ [ar3; ar1; ar2; ar0])] |> s74hc153.solve >>= fun unit7 ->      
         
          begin match unit7 ++ unit6 ++ unit5 ++ unit4 ++ unit3 ++ unit2 ++ unit1 ++ unit0 with
            | (_, zb7::za7::zb6::za6::zb5::za5::zb4::za4::za3::zb3::za2::zb2::za1::zb1::za0::zb0::sx) -> 
            (* Printf.printf "ALU INPUT: %s\n" (unitToStr2 ("alu.INPUT.", [ar0; za7; za6; za5; za4; za3; za2; za1; za0; zb7; zb6; zb5; zb4; zb3; zb2; zb1; zb0])); *)
              [(lab^"alu.", [ar0; za7; za6; za5; za4; za3; za2; za1; za0; zb7; zb6; zb5; zb4; zb3; zb2; zb1; zb0])] |> s2x74hc283.solve 
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

  let instr_code_xor = n2Unit 0b01101 "cod." 5 in (* xor      al=1 c=0 *)
  let instr_code_and = n2Unit 0b10001 "cod." 5 in (* and      al=1 c=0 *)
  let instr_code_or  = n2Unit 0b11101 "cod." 5 in (* or       al=1 c=0 *)
  let instr_code_ld  = n2Unit 0b11001 "cod." 5 in (* ld (b)   al=1 c=0 *)
  let instr_code_add = n2Unit 0b11000 "cod." 5 in (* add      al=0 c=0 *)
  let instr_code_sub = n2Unit 0b00110 "cod." 5 in (* sub      al=0 c=1 *)
  let instr_code_st  = n2Unit 0b00000 "cod." 5 in (* st (a)   al=0 c=0 *)
  let instr_code_bcc = n2Unit 0b01011 "cod." 5 in (* bcc (-a) al=1 c=1 *)

  let xor_ac  = n2Unit  0b10101010  "ac." 8 in 
  let xor_bus = n2Unit  0b11001100 "bus." 8 in

  let and_ac  = n2Unit 0b10101010 "ac." 8 in
  let and_bus = n2Unit 0b11001100 "bus." 8 in

  let or_ac  = n2Unit 0b10101010 "ac." 8 in
  let or_bus = n2Unit 0b11001100 "bus." 8 in

  let ld_ac = n2Unit 0 "ac." 8 in
  let ld_bus = n2Unit (-10) "bus." 8 in

  let add_ac  = n2Unit 20 "ac." 8 in
  let add_bus = n2Unit 15 "bus." 8 in

  let sub_ac  = n2Unit 35 "ac." 8 in
  let sub_bus = n2Unit 20 "bus." 8 in

  let st_ac  = n2Unit 15 "ac" 8 in
  let st_bus = n2Unit 0 "bus." 8 in

  let bcc_ac  = n2Unit (-7)"ac" 8 in (*twos compl. -> NOT then +1 -> (-5) = 11111010 + 1 = 11111011 *)
  let bcc_bus = n2Unit 0 "bus." 8 in

  let _ = 
  [ instr_code_xor ++ xor_ac ++ xor_bus] |> sGigatronALU.solve >>= fun test_xor ->
   (* Printf.printf "test_xor %s\n"  (unitToStr2 test_xor); *)
    match test_unit test_xor (n2Unit 0b0011001100 "exp_val" 9) with
        |Ok m -> Printf.printf "sGigatronALU_test *xor* %s: PASS\n" (unitToStr2 test_xor); Ok m
        |Error e ->  Printf.printf "sGigatronALU_test *xor* %s: FAIL - %s\n" (unitToStr2 test_xor) e.desc; Error e
  in
  let _ =
  [instr_code_and ++ and_ac ++ and_bus] |> sGigatronALU.solve >>= fun test_and ->
    match test_unit test_and (n2Unit 0b100010000 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *and* %s: PASS\n" (unitToStr2 test_and); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *and* %s: FAIL - %s\n" (unitToStr2 test_and) e.desc; Error e
  in
  let _ = 
  [instr_code_or ++ or_ac ++ or_bus] |> sGigatronALU.solve >>= fun test_or  ->
    match test_unit test_or (n2Unit 0b111011100 "exp_val" 9) with
      |Ok m -> Printf.printf "sGigatronALU_test *or* %s: PASS\n" (unitToStr2 test_or); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test *or* %s: FAIL - %s\n" (unitToStr2 test_or) e.desc; Error e
  in
  let _ =
  [instr_code_add ++ add_ac ++ add_bus] |> sGigatronALU.solve >>= fun test_add ->
    let exp_val = ((n2Unit 35 "exp_val." 8) ++ ("carry.", l)) in
    match test_unit test_add exp_val with
      |Ok m -> Printf.printf "sGigatronALU_test %s *add* %s: PASS\n" (unitToStr2 exp_val) (unitToStr2 test_add); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *add* %s: FAIL - %s\n" (unitToStr2 exp_val) (unitToStr2 test_add) e.desc; Error e
  in
  let _ = 
  [instr_code_sub ++ sub_ac ++ sub_bus] |> sGigatronALU.solve >>= fun test_sub ->
    let exp_val = ((n2Unit 15 "exp_val." 8) ++ ("carry.", h)) in
    match test_unit test_sub exp_val with
      |Ok m -> Printf.printf "sGigatronALU_test %s *sub* %s: PASS\n" (unitToStr2 exp_val) (unitToStr2 test_sub); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *sub* %s: FAIL - %s\n" (unitToStr2 exp_val) (unitToStr2 test_sub) e.desc; Error e
  in
  let _ =
  [instr_code_st ++ st_ac ++ st_bus] |> sGigatronALU.solve >>= fun test_st  ->
    let exp_val = ((n2Unit 15 "exp_val." 8) ++ ("carry.", l)) in
    match test_unit test_st exp_val with
      |Ok m -> Printf.printf "sGigatronALU_test %s *st* %s: PASS\n" (unitToStr2 exp_val) (unitToStr2 test_st); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *st* %s: FAIL - %s\n" (unitToStr2 exp_val) (unitToStr2 test_st) e.desc; Error e
  in
  let _ =
  [instr_code_bcc ++ bcc_ac ++ bcc_bus] |> sGigatronALU.solve >>= fun test_bcc ->
    let exp_val = ((n2Unit (-7) "exp_val." 8) ++ ("carry.", h)) in
    match test_unit test_bcc exp_val with
      |Ok m -> Printf.printf "sGigatronALU_test %s *bcc* %s: PASS\n" (unitToStr2 exp_val) (unitToStr2 test_bcc); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *bcc* %s: FAIL - %s\n" (unitToStr2 exp_val) (unitToStr2 test_bcc) e.desc; Error e
  in 
  [instr_code_ld ++ ld_ac ++ ld_bus] |> sGigatronALU.solve >>= fun test_ld  ->
  match test_unit test_ld ((n2Unit (-10) "exp_val." 8) ++ ("carry.", l))  with (* can't convert 9 bit as sign, by alu result 9 bit is used as carry *)
      |Ok m -> Printf.printf "sGigatronALU_test %s *ld* %s: PASS\n" (unitToStr2 ((n2Unit (-10) "exp_val." 8) ++ ("carry.", l)))  (unitToStr2 test_ld); Ok m
      |Error e ->  Printf.printf "sGigatronALU_test %s *ld* %s: FAIL - %s\n" (unitToStr2 ld_bus) (unitToStr2 test_ld) e.desc; Error e 
 

type stateT = {

  tick: int;
  units: unit list (* todo: unit list *)
}


let sMain : unit solver = {
  solve = fun result' -> 
    (* result' >>= fun unit' -> *)
      match result' with
      | [(lab, clk::en_clk::xs)] -> 
      
        sNor.solve [("clk", [clk; en_clk])] >>= fun clk' -> let (_, clk'') = clk' in

        (* let xx = clk'' @ xs
        in *)
        (* Printf.printf "xx:%s\n" (unitToStr2 xx);    *)
        begin match clk'' @ xs with 
        |  LS_0::a::b::c::d::xs' -> 
        
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
          Ok (lab, clk'' @ en_clk::a'::b'::c'::d'::xs')
        | LS_1::a::b::c::d::xs' -> Ok (lab, clk'' @ en_clk::a::b::c::d::xs')
        | _ -> Error {desc="sMain.solver Err.No.: 2"}
        end 
      | _ -> Error {desc="sMain.solver Err.No.: 1"}
}


let rec test_loop (s: stateT) = 
  (* Printf.printf ">>>...%d\n" s.tick; *)
  sMain.solve s.units >>= fun res ->
    Printf.printf ">>> %s\n" (unitToStr2 res);   
    if s.tick > 0 then 

      test_loop {tick = (s.tick - 1); units = [res]}
    else Ok []

  
let () = 
  let _ = make_unit_test in
  let _ = s74hc153_test in
  let _ = s74hc283_test in
  let _ = s2x74hc283_test in
  let _ = sGigatronALU_test in

  let _ = match test_loop {tick = 32; units = [("test", h @ l @ l @ l @ l @ l @ l)] } with
          | Ok m -> ()
          | Error e -> Printf.printf "test_loop FAIL - %s\n" e.desc; 
  in
  Printf.printf "Bye, bye." ; (* (unitToStr (n2Unit 0xaa "lab" 20)); *)
  Printf.printf "%s" (unitToStr2 ((n2Unit 0b0110 "b." 4 ) ++ (n2Unit 0b0011 "a." 4) ));
  Printf.printf "%s" (unitToStr2 ("Man", l @ h @ h @ l @ l @ l @ h @ h));
  Printf.printf "%s" (unitToStr2 ("List", LS_0::LS_1::LS_1::LS_0::LS_0::LS_0::LS_1::LS_1::[]));
  Printf.printf "%s" (unitToStr2 (n2Unit 0b01100011 "b." 8))



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


