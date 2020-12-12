(* type and4 = { in0: int; in1: int ; in2: int; in3: int; output: int } *)

open Table;;
open Array;;

type lineStateT = LS_1 | LS_0 | LS_X 

type signal = (string * lineStateT)



type unit = signal list
type error = { desc: string;}

type 'a linesResult = (('a, error) result) 

(* type mess = SUnit of signal list | MUnit of ((unit, error) result) list *)
type mess =  SUnit of unit linesResult | MUnit of (unit linesResult list) 
(* | (unit linesResult)  list) *)

type 'a solver = {

  solve: 'a linesResult -> 'a linesResult (* type: result of (input * 'a, error) *)
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


  let numToUnit8 n lab = 
    let a = Array.make 8 (Ok [(lab, LS_0)]) in
    a.(0) <- Ok [(Printf.sprintf"%s0" lab, if (n land 0x01) > 0 then LS_1 else LS_0)];
    a.(1) <- Ok [(Printf.sprintf"%s1" lab, if (n land 0x02) > 0 then LS_1 else LS_0)];
    a.(2) <- Ok [(Printf.sprintf"%s2" lab, if (n land 0x04) > 0 then LS_1 else LS_0)];
    a.(3) <- Ok [(Printf.sprintf"%s3" lab, if (n land 0x08) > 0 then LS_1 else LS_0)];
    a.(4) <- Ok [(Printf.sprintf"%s4" lab, if (n land 0x10) > 0 then LS_1 else LS_0)];
    a.(5) <- Ok [(Printf.sprintf"%s5" lab, if (n land 0x20) > 0 then LS_1 else LS_0)];
    a.(6) <- Ok [(Printf.sprintf"%s6" lab, if (n land 0x40) > 0 then LS_1 else LS_0)];
    a.(7) <- Ok [(Printf.sprintf"%s7" lab, if (n land 0x80) > 0 then LS_1 else LS_0)];
    a



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


(* type linesResult = ((unit, error) result)  *)
(* type unit = { lines: signal list; } *)

let rec make_unit (inp : unit linesResult list): unit linesResult =

  let rec f (acc : unit linesResult)  (m : unit linesResult list): unit linesResult  = 
    match m with
    | [] -> acc            
    | x::[] ->   x >>= fun u -> acc >>= fun acc' -> (Ok (acc' @ u))
    | (x::xs) -> x >>= fun u -> acc >>= fun acc' -> (f (Ok (acc' @ u)) xs)
  in
  f (Ok []) inp


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
                    end
                 
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
          | (ea'::eb'::s0'::s1'::in0a::in1a::in2a::in3a::in0b::in1b::in2b::in3b::xs) -> 
              let ea = Ok[ea'] in
              let eb = Ok[eb'] in
              let s0 = Ok[s0'] in
              let s1 = Ok[s1'] in

              let in_ea_neg = sNeg.solve ea in
              let in_eb_neg = sNeg.solve eb in
              let in_s0_neg = sNeg.solve s0 in
              let in_s1_neg = sNeg.solve s1 in

              let in_x = [| 
                  Ok [in0a]; 
                  Ok [in1a]; 
                  Ok [in2a]; 
                  Ok [in3a]; 
                  Ok [in0b]; 
                  Ok [in1b]; 
                  Ok [in2b]; 
                  Ok [in3b] |] in

              let and4_out = Array.make 8 (Ok [("and4_out", LS_0)]) in
              and4_out.(0) <- solve_unit [in_x.(0); in_s1_neg; in_s0_neg; in_ea_neg] sAnd4;
              and4_out.(1) <- solve_unit [in_x.(1); in_s1_neg; s0; in_ea_neg] sAnd4;
              and4_out.(2) <- solve_unit [in_x.(2); s1; in_s0_neg; in_ea_neg] sAnd4;
              and4_out.(3) <- solve_unit [in_x.(3); s1; s0; in_ea_neg] sAnd4;
              and4_out.(4) <- solve_unit [in_x.(4); in_s1_neg; in_s0_neg; in_eb_neg] sAnd4;
              and4_out.(5) <- solve_unit [in_x.(5); in_s1_neg; s0; in_eb_neg] sAnd4;
              and4_out.(6) <- solve_unit [in_x.(6); s1; in_s0_neg; in_eb_neg] sAnd4;
              and4_out.(7) <- solve_unit [in_x.(7); s1; s0; in_eb_neg] sAnd4;

              (* Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (resultToStr e)) and4_out;  *)

              let or4a_out = and4_out.(0) ++ and4_out.(1) ++ and4_out.(2) ++ and4_out.(3) |> sOr4.solve in
              let or4b_out = and4_out.(4) ++ and4_out.(5) ++ and4_out.(6) ++ and4_out.(7) |> sOr4.solve in
              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])); *)
              
              make_unit [or4a_out; or4b_out; (Ok xs)];
              
          | _ -> Error { desc = "[s74hc153] missing input\n" } ;
         end
}

(* 74hc283 sumator simulation *)
let s74hc283 : unit solver = {

  solve = fun result' -> 
      result' >>= fun unit' ->
          begin match unit' with
          | [] -> Error { desc = "[s74hc283] missing input\n" } ;
          | (cin::a1::a2::a3::a4::b1::b2::b3::b4::xs) -> 

              let cin_neg = Ok [cin] |> sNeg.solve in

              let nand2_out = Array.make 4 (Ok [("and2_out", LS_0)]) in
              let nor2_out = Array.make 4 (Ok [("nor2_out", LS_0)]) in

              nand2_out.(0) <- Ok [a1; b1] |> sNand.solve;
              nor2_out.(0) <- Ok [a1; b1] |> sNor.solve;
              nand2_out.(1) <- Ok [a2; b2] |> sNand.solve;
              nor2_out.(1) <- Ok [a2; b2] |> sNor.solve;
              nand2_out.(2) <- Ok [a3; b3] |> sNand.solve;
              nor2_out.(2) <- Ok [a3; b3] |> sNor.solve;
              nand2_out.(3) <- Ok [a4; b4] |> sNand.solve;
              nor2_out.(3) <- Ok [a4; b4] |> sNor.solve;

              let sum1 = (Ok [cin]) ++ ((sNeg.solve nor2_out.(0)) ++ nand2_out.(0) |> sAnd.solve) |> sXor.solve in
              let sum2 = ((cin_neg ++ nand2_out.(0) |> sAnd.solve) ++ nor2_out.(0) |> sNor.solve) ++ ((sNeg.solve nor2_out.(1)) ++ nand2_out.(1) |> sAnd.solve) |> sXor.solve in
              let sum3 = (
                  (cin_neg ++ nand2_out.(0) ++ nand2_out.(1) |> sAnd3.solve) ++ 
                  (nor2_out.(0) ++ nand2_out.(1) |> sAnd.solve) ++ 
                  nor2_out.(1) |> sNor3.solve
                ) ++ 
                ((sNeg.solve nor2_out.(2)) ++ nand2_out.(2) |> sAnd.solve) |> sXor.solve in
              let sum4 = (
                  (cin_neg ++ nand2_out.(0) ++ nand2_out.(1) ++ nand2_out.(2) |> sAnd4.solve) ++ 
                  (nand2_out.(1) ++ nand2_out.(2) ++ nor2_out.(0) |> sAnd3.solve) ++ 
                  (nand2_out.(2) ++ nor2_out.(1) |> sAnd.solve) ++ 
                  nor2_out.(2) |> sNor4.solve
                ) ++ 
                ((sNeg.solve nor2_out.(3)) ++ nand2_out.(3) |> sAnd.solve) |> sXor.solve in
              let overflow = ref (
                  (cin_neg ++ nand2_out.(0) ++ nand2_out.(1) ++ nand2_out.(2) ++ nand2_out.(3) |> sAnd5.solve) ++ 
                  (nand2_out.(1) ++ nand2_out.(2) ++ nand2_out.(3) ++ nor2_out.(0) |> sAnd4.solve) ++ 
                  (nand2_out.(2) ++ nand2_out.(3) ++ nor2_out.(1) |> sAnd3.solve) ++ 
                  (nand2_out.(3) ++ nor2_out.(2) |> sAnd.solve) ++ 
                  nor2_out.(3) |> sNor5.solve
                ) in

              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])) ++ *)
              
              (sum1 ++ sum2 ++ sum3 ++ sum4 ++ !overflow ++ (Ok xs))

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


(* Ok [ea; eb; s0; s1; in0a; in1a; in2a; in3a; in0b; in1b; in2b; in3b] ->  |> *)
(*  za0::zb0::za1::zb1::za2::zb2::za3::zb3::za4::zb4::za5::zb5::za6::zb6::za7::zb7::sx *)
(* 2x 74hc283 sumator simulation *)
let sGigatronALU : unit solver = {

  solve = fun result' -> 
    result' >>= fun unit' -> 
    let l = ("L", LS_0) in
    let h = ("H", LS_1) in
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



(* 

                  let _ = sum1 >>= (fun s74hc283_out -> 
                      begin match s74hc283_out with
                      |s1::s2::s3::s4::carry1::sx -> 
                          let sum2 = Ok [carry1; a4; a5; a6; a7; b4; b5; b6; b7] |> s74hc283.solve in
                          sum2 >>= (fun s74hc283_out2 -> 
                            begin match s74hc283_out2 with
                              |s5::s6::s7::s8::carry2::sx ->( Ok [s1; s2; s3; s4; s5; s6; s7; s8; carry2]);
                              |_ -> Error { desc = "[s2x74hc283] missing input\n" } ;
                              end);
                      |_ -> Error { desc = "[s2x74hc283] missing input\n" } ;
                      end);

 *)

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
let test_make_unit = 

  let base_signals = [                       
          ("a0", LS_0);
          ("a1", LS_0);
          ("a2", LS_1);
          ("a3", LS_0);
          ("b0", LS_1);
          ("b1", LS_1);
          ("b2", LS_1);
          ("b3", LS_1);
        ] in
  (* Printf.printf ">>> function_input_signals result:\n%s\n" (unitToStr base_signals);  *)
 
  let processed_signals = [                       
             ("a0", LS_0);
             ("a1", LS_0);
             ("a2", LS_1);
             ("a3", LS_0);
             ("b0", LS_1);
             ("b1", LS_1);
             ("b2", LS_1);
             ("b3", LS_1);
            ] in

  (* prepare unit to change by tested funciton *)
  let function_input_signals = List.map (fun e -> Ok [e]) processed_signals in

  let destination_signals = ref [] in

  (* change signals by function and recreate original signal *)
  let _ = make_unit function_input_signals >>= fun t -> destination_signals := t; Ok [] in
  (* Printf.printf ">>> destination_signals result:\n%s\n" (unitToStr !destination_signals);  *)

  (* compare time *)
  let res = compare_unit base_signals !destination_signals
  in
  match res with
    |Ok m -> Printf.printf "make_unit: %s\n" m;
    |Error e ->  Printf.printf "make_unit: %s\n" e;



type unitT = VCC | GND | Not | And2 
(* type unitStateT = (unitT, f) *)

type lineMapT = (string, lineStateT) ListMap.t

type unitMapT = (string, unitT) ListMap.t

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
    
let () = 
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

  (* let s74hc153_out = Ok [("ea", LS_1); 
                              ("eb", LS_0); 
                              ("s0", LS_0); 
                              ("s1", LS_1);
                              ("in0a", LS_1);
                              ("in1a", LS_1);
                              ("in2a", LS_1);
                              ("in3a", LS_1);
                              ("in0b", LS_1);
                              ("in1b", LS_1);
                              ("in2b", LS_1);
                              ("in3b", LS_0);                        
                              ] |> s74hc153.solve in *)
  (* Printf.printf ">>> result: %s\n"  (unitToStr  s74hc153_out); *)

 

  let l = Ok [("l", LS_0)] in
  let h = Ok [("h", LS_1)] in
  let al = Ok [("al", LS_0)] in (* 0:LS_1 lub akumulator:LS_0 *)
  let ar0 = Ok [("cin", LS_0)] in (* carry in *)

  let sum_a = numToUnit8 15 "a" in

  let sum_b = numToUnit8 0b1111  "b" in
  (* AR1 AR2 AR3 *)
  (* 110 - xor    al=1 *)
  (* 001 - and    al=1 *)
  (* 111 - or     al=1 *)
  (* 011 - ld (b) al=1 *)
  (* 011 - add    al=0 *)
  (* 100 - sub    al=0 c=1 *)
  (* 000 - st (a) al=0 *)
  (* 010 - bcc (-a) al=1 c=1 *)


  let sum1 = al ++ ar0 ++ l ++ h ++ h ++ sum_a.(0) ++ sum_a.(1) ++ sum_a.(2) ++ sum_a.(3) ++ 
                       sum_a.(4) ++ sum_a.(5) ++ sum_a.(6) ++ sum_a.(7) ++ 
                       sum_b.(0) ++ sum_b.(1) ++ sum_b.(2) ++ sum_b.(3) ++ 
                       sum_b.(4) ++ sum_b.(5) ++ sum_b.(6) ++ sum_b.(7) |> sGigatronALU.solve in

  Printf.printf ">>> result:\n%s\n" (resultToStr sum1);
  Printf.printf ">>> test: 200 + 57 == %d <<< %s\n" (resultToNum sum1) (if (resultToNum sum1) == 200 + 57 then "PASS" else "FAIL") ;

  (* Printf.printf ">>> result:\n%s\n"  (resultToStr a) *)
  Printf.printf "bye.";

  (* {lines=[("a1", LS_0)]}; *)
  (* : output -> out_y neg_y; *)

  (* Printf.printf ">>> result:\n%s\n"  (resultToStr  s74hc283_out2 >>= fun l -> );  *)

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
