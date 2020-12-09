(* type and4 = { in0: int; in1: int ; in2: int; in3: int; output: int } *)

open Table;;
open Array;;

type lineStateT = LS_High | LS_Low | LS_Open 

type signal = (string * lineStateT)



type unit = { lines: signal list; }
type error = { desc: string;}

type linesResult = ((unit, error) result) 

(* type mess = SUnit of signal list | MUnit of ((unit, error) result) list *)
type mess = SUnit of linesResult | MUnit of linesResult list

type 'a solver = {
  solve: linesResult -> 'a (* type: result of (input * 'a, error) *)
}


let unitToStr (out: linesResult) : string =

  match out with
  | Error error -> error.desc
  | Ok o -> 
    let rec f = fun l ->
      match l  with 
      | [] -> ""
      | (s, LS_High)::xs -> Printf.sprintf "[ %s ]: <high> %s\n" s (f xs);
      | (s, LS_Low)::xs -> Printf.sprintf "[ %s ]: <low> %s\n" s (f xs);
      | (s, LS_Open)::xs -> Printf.sprintf "[ %s ]: <open> %s\n" s (f xs);

    in
    f o.lines

(*
type mess = SUnit of ((unit, error) result) | MUnit of ((unit, error) result) list 
type unit = { lines: signal list; }
 *)
let to_unit s = Ok {lines = s}

(* type linesResult = ((unit, error) result)  *)
(* type unit = { lines: signal list; } *)

let rec make_unit (inp : linesResult list): linesResult =

  let rec f (acc : linesResult)  (m : linesResult list): linesResult  = 
              match m with
                | [] -> acc            
                | x::[] -> begin match x with
                    | Error e -> Error e
                    | Ok u -> begin match acc with
                          |Error e -> Error e
                          |Ok acc' -> (to_unit (u.lines @ acc'.lines))
                              end
                      end
                | (x::xs) -> begin match x with
                    | Error e -> Error e
                    | Ok u -> begin match acc with
                          |Error e -> Error e
                          |Ok acc' -> (f (to_unit (u.lines @ acc'.lines)) xs)
                              end
                      end
  in
  f (to_unit []) inp


let sNeg : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e      
      | Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "[sNeg] missing input\n"}
                    | (_, LS_High)::xs -> to_unit (("sNeg", LS_Low)::xs)
                    | _::xs -> to_unit (("sNeg", LS_High)::xs)
} 

(* Tri-state negator oe in *)
let sTneg : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e      
      | Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "[tneg_out] missing input\n"}
                    | _::[] -> Error {desc = "[tneg_out] missing input\n"}
                    | (_, LS_High)::(_, LS_High)::xs -> to_unit (("tneg_out", LS_Low)::xs)
                    | (_, LS_High)::(_, LS_Low)::xs -> to_unit (("tneg_out", LS_High)::xs)
                    | _::_::xs -> to_unit (("tneg_out", LS_Open)::xs)
} 

let sBuf : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "[sBuf] missing input\n"} 
                    | (_, b)::xs -> to_unit (("sBuf", b)::xs)
} 

(* Tri-state bufor inputs: oe in *)
let sTbuf : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e      
      | Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "[tbuf_out] missing input\n"}
                    | _::[] -> Error {desc = "[tbuf_out] missing input\n"}
                    | (_, LS_High)::(_, LS_Low)::xs -> to_unit (("tbuf_out", LS_Low)::xs)
                    | (_, LS_High)::(_, LS_High)::xs -> to_unit (("tbuf_out", LS_High)::xs)
                    | _::_::xs -> to_unit (("tbuf_out", LS_Open)::xs)
} 

let sAnd : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::xs -> to_unit (("sAnd", LS_High)::xs) 
                    | _::_::xs -> to_unit (("sAnd", LS_Low)::xs) 
                    | _ -> Error {desc = "[sAnd] missing input\n"}  
} 

let sAnd3 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::(_, LS_High)::xs -> to_unit (("sAnd3", LS_High)::xs) 
                    | _::_::_::xs -> to_unit (("sAnd3", LS_Low)::xs) 
                    | _ -> Error {desc = "[sAnd3] missing input\n"}  
} 

let sAnd4 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::(_, LS_High)::(_, LS_High)::xs -> to_unit (("sAnd4", LS_High)::xs) 
                    | _::_::_::_::xs -> to_unit (("sAnd4", LS_Low)::xs) 
                    | _ -> Error {desc = "[sAnd4] missing input\n"}  
} 

let sAnd5 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::(_, LS_High)::(_, LS_High)::(_, LS_High)::xs -> to_unit (("sAnd5", LS_High)::xs) 
                    | _::_::_::_::_::xs -> to_unit (("sAnd5", LS_Low)::xs) 
                    | _ -> Error {desc = "[sAnd5] missing input\n"}  
} 



let sNand : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::xs -> to_unit (("sNand", LS_Low)::xs) 
                    | _::_::xs -> to_unit (("sNand", LS_High)::xs) 
                    | _ -> Error {desc = "[sNand] missing input\n"}  
} 




let sOr : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with  
                    | (_, LS_Low)::(n2, LS_Low)::xs -> to_unit (("sOr", LS_Low)::xs)
                    | _::_::xs -> to_unit (("sOr", LS_High)::xs)
                    | _ -> Error {desc = "[sOr] missing 2 inputs"}   
}  

let sXor : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with  
                    | (_, LS_Low)::(n2, LS_Low)::xs -> to_unit (("sOr", LS_Low)::xs)
                    | (_, LS_High)::(n2, LS_High)::xs -> to_unit (("sOr", LS_Low)::xs)
                    | _::_::xs -> to_unit (("sOr", LS_High)::xs)
                    | _ -> Error {desc = "[sOr] missing 2 inputs"}   
}  

let sNor : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor", LS_High)::xs)
                    | _::_::xs -> to_unit (("sNor", LS_Low)::xs)
                    | _ -> Error {desc = "[sNor] missing input\n"} 
}   

let sNor3 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor3", LS_High)::xs)
                    | _::_::_::xs -> to_unit (("sNor3", LS_Low)::xs)
                    | _ -> Error {desc = "[sNor3] missing input\n"} 
}   


let sNor4 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor4", LS_High)::xs)
                    | _::_::_::_::xs -> to_unit (("sNor4", LS_Low)::xs)
                    | _ -> Error {desc = "[sNor4] missing input\n"} 
}   


let sNor5 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor5", LS_High)::xs)
                    | _::_::_::_::_::xs -> to_unit (("sNor5", LS_Low)::xs)
                    | _ -> Error {desc = "[sNor5] missing input\n"} 
}   


let sNor8 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor8", LS_High)::xs)
                    | _::_::_::_::_::_::_::_::xs -> to_unit (("sNor8", LS_Low)::xs)
                    | _ -> Error {desc = "[sNor8] missing input\n"} 
}   

let sOr4 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor4", LS_Low)::xs)
                    | _::_::_::_::xs -> to_unit (("sNor4", LS_High)::xs)
                    | _ -> Error {desc = "[sNor4] missing input\n"} 
}   


(* 74hc251 multiplexer simulation *)
let s74hc251 : linesResult solver = {

  solve = fun result' -> 

    match result' with
      | Error e ->  Error e 
      | Ok unit' -> begin match unit'.lines with
          | [] -> Error { desc = "[s74hc251] missing input\n" } ;
          | (oe'::s0'::s1'::s2'::in0::in1::in2::in3::in4::in5::in6::in7::xs) -> 
              let oe = to_unit [oe'] in
              let s0 = to_unit [s0'] in
              let s1 = to_unit [s1'] in
              let s2 = to_unit [s2'] in

              let in_oe_neg = sNeg.solve  oe in
              let in_s0_neg = sNeg.solve  s0 in
              let in_s1_neg = sNeg.solve  s1 in
              let in_s2_neg = sNeg.solve  s2 in

              let in_x = [| 
                  to_unit [in0]; 
                  to_unit [in1]; 
                  to_unit [in2]; 
                  to_unit [in3]; 
                  to_unit [in4]; 
                  to_unit [in5]; 
                  to_unit [in6]; 
                  to_unit [in7] |] in

              (* let in_x = Array.make 8 (to_unit [("in", LS_Low)]) in *)
              (* Array.iteri (fun x _ -> in_x.(x) <- to_unit [(Printf.sprintf "in%d" x, LS_Low)]) in_x; *)
              (* in_x.(1) <- to_unit [(Printf.sprintf "in%d" 1, LS_Low)]; *)
              
              (* Array.iteri (fun i e -> Printf.printf "in_%d:%s" i (unitToStr e)) in_x; *)

              let buf_out = Array.make 8 (to_unit [("buf_out", LS_Low)]) in
              Array.iteri (fun x _ -> (buf_out.(x) <- sBuf.solve in_x.(x))) buf_out;

              let and4_out = Array.make 8 (to_unit [("and4_out", LS_Low)]) in
              and4_out.(0) <- (make_unit [buf_out.(0); in_s2_neg; in_s1_neg; in_s0_neg]) |> sAnd4.solve;
              and4_out.(1) <- (make_unit [buf_out.(1); in_s2_neg; in_s1_neg; s0]) |> sAnd4.solve;
              and4_out.(2) <- (make_unit [buf_out.(2); in_s2_neg; s1; in_s0_neg]) |> sAnd4.solve;
              and4_out.(3) <- (make_unit [buf_out.(3); in_s2_neg; s1; s0]) |> sAnd4.solve;
              and4_out.(4) <- (make_unit [buf_out.(4); s2; in_s1_neg; in_s0_neg]) |> sAnd4.solve;
              and4_out.(5) <- (make_unit [buf_out.(5); s2; in_s1_neg; s0]) |> sAnd4.solve;
              and4_out.(6) <- (make_unit [buf_out.(6); s2; s1; in_s0_neg]) |> sAnd4.solve;
              and4_out.(7) <- (make_unit [buf_out.(7); s2; s1; s0]) |> sAnd4.solve;

              (* Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (unitToStr e)) and4_out; *)

              let nor8_out = make_unit [and4_out.(0); and4_out.(1); and4_out.(2); and4_out.(3); and4_out.(4); and4_out.(5); and4_out.(6); and4_out.(7)] |> sNor8.solve in
              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])); *)
              
              let out_y = make_unit [nor8_out; in_oe_neg] |> sTbuf.solve in
              let neg_y = make_unit [nor8_out; in_oe_neg] |> sTneg.solve in
              (make_unit [out_y; neg_y; (to_unit xs)])

          | _ -> Error { desc = "[s74hc251] missing input\n" } ;
          end
}

(* s74hc153 multiplexer simulation *)
let s74hc153 : linesResult solver = {

  solve = fun result' -> 

    match result' with
      | Error e ->  Error e 
      | Ok unit' -> begin match unit'.lines with
          | [] -> Error { desc = "[s74hc153] missing input\n" } ;
          | (ea'::eb'::s0'::s1'::in0a::in1a::in2a::in3a::in0b::in1b::in2b::in3b::xs) -> 
              let ea = to_unit [ea'] in
              let eb = to_unit [eb'] in
              let s0 = to_unit [s0'] in
              let s1 = to_unit [s1'] in

              let in_ea_neg = sNeg.solve ea in
              let in_eb_neg = sNeg.solve eb in
              let in_s0_neg = sNeg.solve s0 in
              let in_s1_neg = sNeg.solve s1 in

              let in_x = [| 
                  to_unit [in0a]; 
                  to_unit [in1a]; 
                  to_unit [in2a]; 
                  to_unit [in3a]; 
                  to_unit [in0b]; 
                  to_unit [in1b]; 
                  to_unit [in2b]; 
                  to_unit [in3b] |] in

              (* let in_x = Array.make 8 (to_unit [("in", LS_Low)]) in *)
              (* Array.iteri (fun x _ -> in_x.(x) <- to_unit [(Printf.sprintf "in%d" x, LS_Low)]) in_x; *)
              (* in_x.(1) <- to_unit [(Printf.sprintf "in%d" 1, LS_Low)]; *)
              
              (* Array.iteri (fun i e -> Printf.printf "in_%d:%s" i (unitToStr e)) in_x; *)

              (* let buf_out = Array.make 8 (to_unit [("buf_out", LS_Low)]) in
              Array.iteri (fun x _ -> (buf_out.(x) <- sBuf.solve in_x.(x))) buf_out; *)

              let and4_out = Array.make 8 (to_unit [("and4_out", LS_Low)]) in
              and4_out.(0) <- (make_unit [in_x.(0); in_s1_neg; in_s0_neg; in_ea_neg]) |> sAnd4.solve;
              and4_out.(1) <- (make_unit [in_x.(1); in_s1_neg; s0; in_ea_neg]) |> sAnd4.solve;
              and4_out.(2) <- (make_unit [in_x.(2); s1; in_s0_neg; in_ea_neg]) |> sAnd4.solve;
              and4_out.(3) <- (make_unit [in_x.(3); s1; s0; in_ea_neg]) |> sAnd4.solve;
              and4_out.(4) <- (make_unit [in_x.(4); in_s1_neg; in_s0_neg; in_eb_neg]) |> sAnd4.solve;
              and4_out.(5) <- (make_unit [in_x.(5); in_s1_neg; s0; in_eb_neg]) |> sAnd4.solve;
              and4_out.(6) <- (make_unit [in_x.(6); s1; in_s0_neg; in_eb_neg]) |> sAnd4.solve;
              and4_out.(7) <- (make_unit [in_x.(7); s1; s0; in_eb_neg]) |> sAnd4.solve;

              Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (unitToStr e)) and4_out;

              let or4a_out = make_unit [and4_out.(0); and4_out.(1); and4_out.(2); and4_out.(3)] |> sOr4.solve in
              let or4b_out = make_unit [and4_out.(4); and4_out.(5); and4_out.(6); and4_out.(7)] |> sOr4.solve in
              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])); *)
              
              (make_unit [or4a_out; or4b_out; (to_unit xs)])

          | _ -> Error { desc = "[s74hc153] missing input\n" } ;
          end
}

let s74hc283 : linesResult solver = {

  solve = fun result' -> 

    match result' with
      | Error e ->  Error e 
      | Ok unit' -> begin match unit'.lines with
          | [] -> Error { desc = "[s74hc283] missing input\n" } ;
          | (cin'::a1'::a2'::a3'::a4'::b1'::b2'::b3'::b4'::xs) -> 
              let cin = to_unit [cin'] in (* carry in *)

              let a1 = to_unit [a1'] in
              let a2 = to_unit [a2'] in
              let a3 = to_unit [a3'] in
              let a4 = to_unit [a4'] in

              let b1 = to_unit [b1'] in
              let b2 = to_unit [b2'] in
              let b3 = to_unit [b3'] in
              let b4 = to_unit [b4'] in

              let cin_neg = sNeg.solve cin in

              let nand2_out = Array.make 4 (to_unit [("and2_out", LS_Low)]) in
              let nor2_out = Array.make 4 (to_unit [("nor2_out", LS_Low)]) in

              nand2_out.(0) <- (make_unit [a1; b1]) |> sNand.solve;
              nor2_out.(0) <- (make_unit [a1; b1]) |> sNor.solve;
              nand2_out.(1) <- (make_unit [a2; b2]) |> sNand.solve;
              nor2_out.(1) <- (make_unit [a2; b2]) |> sNor.solve;
              nand2_out.(2) <- (make_unit [a3; b3]) |> sNand.solve;
              nor2_out.(2) <- (make_unit [a3; b3]) |> sNor.solve;
              nand2_out.(3) <- (make_unit [a4; b4]) |> sNand.solve;
              nor2_out.(3) <- (make_unit [a4; b4]) |> sNor.solve;

              let or2_out = Array.make 4 (to_unit [("nor2_out", LS_Low)]) in
              or2_out.(0) <- sNeg.solve nor2_out.(0);
              or2_out.(1) <- sNeg.solve nor2_out.(1);
              or2_out.(2) <- sNeg.solve nor2_out.(2);
              or2_out.(3) <- sNeg.solve nor2_out.(3);

              let l1out = Array.make 14 (to_unit [("l1out", LS_Low)]) in

              l1out.(0) <- (make_unit [or2_out.(0); nand2_out.(0)]) |> sAnd.solve;
              l1out.(1) <- (make_unit [cin_neg; nand2_out.(0)]) |> sAnd.solve;
              l1out.(2) <- (make_unit [or2_out.(1); nand2_out.(1)]) |> sAnd.solve;

              l1out.(3) <- (make_unit [cin_neg; nand2_out.(0); nand2_out.(1)]) |> sAnd3.solve;
              l1out.(4) <- (make_unit [nor2_out.(0); nand2_out.(1)]) |> sAnd.solve;
              l1out.(5) <- (make_unit [or2_out.(2); nand2_out.(2)]) |> sAnd.solve;

              l1out.(6) <- (make_unit [cin_neg; nand2_out.(0); nand2_out.(1); nand2_out.(2)]) |> sAnd4.solve;
              l1out.(7) <- (make_unit [nand2_out.(1); nand2_out.(2); nor2_out.(0)]) |> sAnd3.solve;
              l1out.(8) <- (make_unit [nand2_out.(2); nor2_out.(1)]) |> sAnd.solve;

              l1out.(9) <- (make_unit [or2_out.(3); nand2_out.(3)]) |> sAnd.solve;

              l1out.(10) <- (make_unit [cin_neg; nand2_out.(0); nand2_out.(1); nand2_out.(2); nand2_out.(3)]) |> sAnd5.solve;
              l1out.(11) <- (make_unit [nand2_out.(1); nand2_out.(2); nand2_out.(3); nor2_out.(0)]) |> sAnd4.solve;
              l1out.(12) <- (make_unit [nand2_out.(2); nand2_out.(3); nor2_out.(1)]) |> sAnd3.solve;
              l1out.(13) <- (make_unit [nand2_out.(3); nor2_out.(2)]) |> sAnd.solve;


              let l2out = Array.make 4 (to_unit [("l2out", LS_Low)]) in

              l2out.(0) <- (make_unit [l1out.(1); nand2_out.(0)]) |> sNor.solve;
              l2out.(1) <- (make_unit [l1out.(3); l1out.(4); nor2_out.(1)]) |> sNor3.solve;
              l2out.(2) <- (make_unit [l1out.(6); l1out.(7);l1out.(8); nor2_out.(2)]) |> sNor4.solve;
              l2out.(3) <- (make_unit [l1out.(10); l1out.(11); l1out.(12); l1out.(10); nor2_out.(3)]) |> sNor5.solve;



              let sum1 = (make_unit [cin; l1out.(0)]) |> sXor.solve in
              let sum2 = (make_unit [l2out.(0); l1out.(2)]) |> sXor.solve in
              let sum3 = (make_unit [l2out.(1); l1out.(5)]) |> sXor.solve in
              let sum4 = (make_unit [l2out.(2); l1out.(9)]) |> sXor.solve in
              let overflow = ref l2out.(3) in

              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])); *)
              
              (make_unit [sum1; sum2; sum3; sum4; !overflow; (to_unit xs)])

          | _ -> Error { desc = "[s74hc283] missing input\n" } ;
          end
}

(* 
let ( >>= )  (p: 'a solver) (f: 'a -> 'b solver)  : 'b solver  = { 
  
  solve = fun unit -> 
      match p.solve unit with
      | Ok unit' -> (f unit').solve unit';
      | Error error -> Error error;
} 
*)

(* unit -> (unit, error) result  signal list *)

(* let return (x: 'a) : 'a solver  = { solve = fun _ -> x } *)


(* let ( <*> ) (p1: 'a solver) (p2: 'b solver): 'c solver  = { 
  
  solve = fun input -> 
  
    input |> (p1 >>= fun a -> p2 >>= fun b -> 
      return (a.lines @ b.lines)).solve;
}  *)

 (* let ( <*> ) (p1: 'a solver) (p2: 'b solver): 'c solver  = { 
  
  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    |[] -> Error {desc = "<*> missing input\n 0"} 
                    |[x1] -> Error {desc = "<*> missing input\n 1"} 
                    |[x1; x2] -> Error {desc = "<*> missing input\n 2"} 
                    |[x1; x2; x3] -> Error {desc = "<*> missing input\n 3"} 
                    |(x1::x2::x3::x4::xs) -> 
                      match p1.solve (to_unit [x1;x2;]) with
                        |Error error -> Error error
                        |Ok unit1 -> 
                          match p2.solve (to_unit [x3; x4]) with 
                            |Ok unit2 ->  to_unit (unit1.lines@unit2.lines)
                            |Error error -> Error error

} *)


type unitT = VCC | GND | Not | And2 
(* type unitStateT = (unitT, f) *)

type lineMapT = (string, lineStateT) ListMap.t

type unitMapT = (string, unitT) ListMap.t

(* let lineStatetoStr (state: lineStateT option) : string =

  match state with 
  | Some(LS_High) -> "state:high" 
  | Some(LS_Low) -> "state:low"
  | Some(LS_Open) -> "state:open"
  | _ -> "?" *)

(* let rec insertLines (t : lineMapT ref) (k : string list) =
  match k with
    | x::xs -> 
      t :=(ListMap.insert x LS_Open !t);
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

  (* let s74hc251_out = to_unit [("oe", LS_High); 
                              ("s0", LS_High); 
                              ("s1", LS_Low); 
                              ("s2", LS_High);
                              ("in0", LS_High);
                              ("in1", LS_Low);
                              ("in2", LS_Low);
                              ("in3", LS_Low);
                              ("in4", LS_Low);
                              ("in5", LS_High);
                              ("in6", LS_Low);
                              ("in7", LS_Low);                        
                              ] |> s74hc251.solve in *)

  (* let s74hc153_out = to_unit [("ea", LS_High); 
                              ("eb", LS_Low); 
                              ("s0", LS_Low); 
                              ("s1", LS_High);
                              ("in0a", LS_High);
                              ("in1a", LS_High);
                              ("in2a", LS_High);
                              ("in3a", LS_High);
                              ("in0b", LS_High);
                              ("in1b", LS_High);
                              ("in2b", LS_High);
                              ("in3b", LS_Low);                        
                              ] |> s74hc153.solve in *)
  (* Printf.printf ">>> result: %s\n"  (unitToStr  s74hc153_out); *)

  let s74hc283_out = to_unit [("cin", LS_Low); 
                              ("a1", LS_Low); 
                              ("a2", LS_High); 
                              ("a3", LS_Low);
                              ("a4", LS_High);
                              ("b1", LS_Low);
                              ("b2", LS_High);
                              ("b3", LS_Low);
                              ("b4", LS_High);                      
                              ] |> s74hc283.solve in



  (* : output -> out_y neg_y; *)
  Printf.printf ">>> result: %s\n"  (unitToStr  s74hc283_out);

  (* let tracks : lineMapT ref = ref ListMap.empty in
  let units : unitMapT ref = ref ListMap.empty in
  "x"::"y"::"z"::[] |> insertLines tracks;
  showLines tracks; *)
  

  
(*  result: [out]:low [2]:high [3]:low [out]:low [3]:low  mapowanie wyniku i zastosowanie >>= *)
(* brak przypisania etykiety do czegokolwiek eq. można mieć dwie takie same.. *)
