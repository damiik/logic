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


let (+++) (u1: unit) (u2: unit) : unit =

  let rec f l1 l2 = 
    match l1 with
      |[] -> l2
      |x::xs -> f (x::l2) xs
  in
   f u1 u2

(* (+++) Lokomotywa l = l
(Wagon x xs) +++ pociag = x +:+ (xs +++ pociag) *)

let ( >>= )  (p: 'a linesResult) (f: 'a -> 'b linesResult)  : 'b linesResult =
  
      match p with
      | Ok a -> f a
      | Error error -> Error error


(*  (us: unit solver) *)

let solve_unit (lr: (unit linesResult) list) (us: unit solver): unit linesResult = 


  let rec f  (l: (unit linesResult) list)  : unit linesResult = 
      (* let x::xs = u in Printf.printf "solve_unit:%s" (resultToStr u); *)
      match l with
        | [] -> Printf.printf "solve_unit:[]"; Ok []
        | x::xs -> x >>= fun u -> 
                      Printf.printf "solve_unit:%s" (unitToStr u);
                      match f xs with
                        | Error e -> Error e
                        | Ok w -> Ok (u +++ w)

  in
  let result = ref []
  in
  (f lr) >>= fun u -> result := u ;
  us.solve (Ok !result)
              (* and4_out.(0) <- (buf_out.(0) >>= fun i1 -> 
                                 in_s2_neg >>= fun i2 -> 
                                 in_s1_neg >>= fun i3 -> 
                                 in_s0_neg >>= fun i4 -> i1+++i2+++i3+++i4 |> sAnd4.solve); *)

(* type 'a solver = {
  solve: linesResult -> 'a (* type: result of (input * 'a, error) *)
} *)
(* type linesResult = ((unit, error) result) *)
(* unit -> (unit, error) result  signal list *)

(* let return (x: 'a) : 'a solver  = { solve = fun _ -> x } *)




(*
type mess = SUnit of ((unit, error) result) | MUnit of ((unit, error) result) list 
type unit = { lines: signal list; }
 *)
let to_result s = Ok s

(* type linesResult = ((unit, error) result)  *)
(* type unit = { lines: signal list; } *)
(* miesza w koloejności sygnałów !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
let rec make_unit (inp : unit linesResult list): unit linesResult =

  let rec f (acc : unit linesResult)  (m : unit linesResult list): unit linesResult  = 
              match m with
                | [] -> acc            
                | x::[] -> begin match x with
                    | Error e -> Error e
                    | Ok u -> begin match acc with
                          |Error e -> Error e
                          |Ok acc' -> (to_result (u @ acc'))
                              end
                      end
                | (x::xs) -> begin match x with
                    | Error e -> Error e
                    | Ok u -> begin match acc with
                          |Error e -> Error e
                          |Ok acc' -> (f (to_result (u @ acc')) xs)
                              end
                      end
  in
  f (to_result []) inp


let sNeg : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e      
      | Ok unit' -> match unit' with
                    | [] -> Error {desc = "[sNeg] missing input\n"}
                    | (_, LS_1)::xs -> to_result (("sNeg", LS_0)::xs)
                    | _::xs -> to_result (("sNeg", LS_1)::xs)
} 

(* Tri-state negator oe in *)
let sTneg : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e      
      | Ok unit' -> match unit' with
                    | [] -> Error {desc = "[tneg_out] missing input\n"}
                    | _::[] -> Error {desc = "[tneg_out] missing input\n"}
                    | (_, LS_1)::(_, LS_1)::xs -> to_result (("tneg_out", LS_0)::xs)
                    | (_, LS_1)::(_, LS_0)::xs -> to_result (("tneg_out", LS_1)::xs)
                    | _::_::xs -> to_result (("tneg_out", LS_X)::xs)
} 

let sBuf : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | [] -> Error {desc = "[sBuf] missing input\n"} 
                    | (_, b)::xs -> to_result (("sBuf", b)::xs)
} 

(* Tri-state bufor inputs: oe in *)
let sTbuf : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | [] -> Error {desc = "[tbuf_out] missing input\n"}
                    | _::[] -> Error {desc = "[tbuf_out] missing input\n"}
                    | (_, LS_1)::(_, LS_0)::xs -> to_result (("tbuf_out", LS_0)::xs)
                    | (_, LS_1)::(_, LS_1)::xs -> to_result (("tbuf_out", LS_1)::xs)
                    | _::_::xs -> to_result (("tbuf_out", LS_X)::xs)
} 

let sAnd : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_1)::(_, LS_1)::xs -> to_result (("sAnd", LS_1)::xs) 
                    | _::_::xs -> to_result (("sAnd", LS_0)::xs) 
                    | _ -> Error {desc = "[sAnd] missing input\n"}  
} 

let sAnd3 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_1)::(_, LS_1)::(_, LS_1)::xs -> to_result (("sAnd3", LS_1)::xs) 
                    | _::_::_::xs -> to_result (("sAnd3", LS_0)::xs) 
                    | _ -> Error {desc = "[sAnd3] missing input\n"}  
} 

let sAnd4 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_1)::(_, LS_1)::(_, LS_1)::(_, LS_1)::xs -> to_result (("sAnd4", LS_1)::xs) 
                    | _::_::_::_::xs -> to_result (("sAnd4", LS_0)::xs) 
                    | _ -> Error {desc = "[sAnd4] missing input\n"}  
} 

let sAnd5 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_1)::(_, LS_1)::(_, LS_1)::(_, LS_1)::(_, LS_1)::xs -> to_result (("sAnd5", LS_1)::xs) 
                    | _::_::_::_::_::xs -> to_result (("sAnd5", LS_0)::xs) 
                    | _ -> Error {desc = "[sAnd5] missing input\n"}  
} 



let sNand : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_1)::(_, LS_1)::xs -> to_result (("sNand", LS_0)::xs) 
                    | _::_::xs -> to_result (("sNand", LS_1)::xs) 
                    | _ -> Error {desc = "[sNand] missing input\n"}  
} 




let sOr : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(n2, LS_0)::xs -> to_result (("sOr", LS_0)::xs)
                    | _::_::xs -> to_result (("sOr", LS_1)::xs)
                    | _ -> Error {desc = "[sOr] missing 2 inputs"}   
}  

let sXor : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(n2, LS_0)::xs -> to_result (("sOr", LS_0)::xs)
                    | (_, LS_1)::(n2, LS_1)::xs -> to_result (("sOr", LS_0)::xs)
                    | _::_::xs -> to_result (("sOr", LS_1)::xs)
                    | _ -> Error {desc = "[sOr] missing 2 inputs"}   
}  

let sNor : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(_, LS_0)::xs -> to_result (("sNor", LS_1)::xs)
                    | _::_::xs -> to_result (("sNor", LS_0)::xs)
                    | _ -> Error {desc = "[sNor] missing input\n"} 
}   

let sNor3 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> to_result (("sNor3", LS_1)::xs)
                    | _::_::_::xs -> to_result (("sNor3", LS_0)::xs)
                    | _ -> Error {desc = "[sNor3] missing input\n"} 
}   


let sNor4 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> to_result (("sNor4", LS_1)::xs)
                    | _::_::_::_::xs -> to_result (("sNor4", LS_0)::xs)
                    | _ -> Error {desc = "[sNor4] missing input\n"} 
}   


let sNor5 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> to_result (("sNor5", LS_1)::xs)
                    | _::_::_::_::_::xs -> to_result (("sNor5", LS_0)::xs)
                    | _ -> Error {desc = "[sNor5] missing input\n"} 
}   


let sNor8 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> to_result (("sNor8", LS_1)::xs)
                    | _::_::_::_::_::_::_::_::xs -> to_result (("sNor8", LS_0)::xs)
                    | _ -> Error {desc = "[sNor8] missing input\n"} 
}   

let sOr4 : unit solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
                    | (_, LS_0)::(_, LS_0)::(_, LS_0)::(_, LS_0)::xs -> to_result (("sNor4", LS_0)::xs)
                    | _::_::_::_::xs -> to_result (("sNor4", LS_1)::xs)
                    | _ -> Error {desc = "[sNor4] missing input\n"} 
}   


(* 74hc251 multiplexer simulation *)
let s74hc251 : unit solver = {

  solve = fun result' -> 

      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit' with
          | [] -> Error { desc = "[s74hc251] missing input\n" } ;
          | (oe'::s0'::s1'::s2'::in0::in1::in2::in3::in4::in5::in6::in7::xs) -> 
              let oe = to_result [oe'] in
              let s0 = to_result [s0'] in
              let s1 = to_result [s1'] in
              let s2 = to_result [s2'] in

              let in_oe_neg = sNeg.solve oe in
              let in_s0_neg = sNeg.solve s0 in
              let in_s1_neg = sNeg.solve s1 in
              let in_s2_neg = sNeg.solve s2 in

              let in_x = [| 
                  to_result [in0]; 
                  to_result [in1]; 
                  to_result [in2]; 
                  to_result [in3]; 
                  to_result [in4]; 
                  to_result [in5]; 
                  to_result [in6]; 
                  to_result [in7] |] in

              (* let in_x = Array.make 8 (to_result [("in", LS_0)]) in *)
              (* Array.iteri (fun x _ -> in_x.(x) <- to_result [(Printf.sprintf "in%d" x, LS_0)]) in_x; *)
              (* in_x.(1) <- to_result [(Printf.sprintf "in%d" 1, LS_0)]; *)
              
              (* Array.iteri (fun i e -> Printf.printf "in_%d:%s" i (unitToStr e)) in_x; *)

              let buf_out = Array.make 8 (to_result [("buf_out", LS_0)]) in
              Array.iteri (fun x _ -> (buf_out.(x) <- sBuf.solve in_x.(x))) buf_out;

              let and4_out = Array.make 8 (to_result [("and4_out", LS_0)]) in
              and4_out.(0) <- solve_unit [buf_out.(0); in_s2_neg; in_s1_neg; in_s0_neg] sAnd4;
              and4_out.(1) <- solve_unit [buf_out.(1); in_s2_neg; in_s1_neg; s0] sAnd4;
              and4_out.(2) <- solve_unit [buf_out.(2); in_s2_neg; s1; in_s0_neg] sAnd4;
              and4_out.(3) <- solve_unit [buf_out.(3); in_s2_neg; s1; s0] sAnd4;
              and4_out.(4) <- solve_unit [buf_out.(4); s2; in_s1_neg; in_s0_neg] sAnd4;
              and4_out.(5) <- solve_unit [buf_out.(5); s2; in_s1_neg; s0] sAnd4;
              and4_out.(6) <- solve_unit [buf_out.(6); s2; s1; in_s0_neg] sAnd4;
              and4_out.(7) <- solve_unit [buf_out.(7); s2; s1; s0] sAnd4;

              (* Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (unitToStr e)) and4_out; *)

              let nor8_out = make_unit [and4_out.(0); and4_out.(1); and4_out.(2); and4_out.(3); and4_out.(4); and4_out.(5); and4_out.(6); and4_out.(7)] |> sNor8.solve in
              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])); *)
              
              let out_y = make_unit [nor8_out; in_oe_neg] |> sTbuf.solve in
              let neg_y = make_unit [nor8_out; in_oe_neg] |> sTneg.solve in
              (make_unit [out_y; neg_y; (to_result xs)])

          | _ -> Error { desc = "[s74hc251] missing input\n" } ;
      
}

(* s74hc153 multiplexer simulation *)
let s74hc153 : unit solver = {

  solve = fun result' -> 

      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> begin match unit' with
          | [] -> Error { desc = "[s74hc153] missing input\n" } ;
          | (ea'::eb'::s0'::s1'::in0a::in1a::in2a::in3a::in0b::in1b::in2b::in3b::xs) -> 
              let ea = to_result[ea'] in
              let eb = to_result[eb'] in
              let s0 = to_result[s0'] in
              let s1 = to_result[s1'] in

              let in_ea_neg = sNeg.solve ea in
              let in_eb_neg = sNeg.solve eb in
              let in_s0_neg = sNeg.solve s0 in
              let in_s1_neg = sNeg.solve s1 in

              let in_x = [| 
                  to_result [in0a]; 
                  to_result [in1a]; 
                  to_result [in2a]; 
                  to_result [in3a]; 
                  to_result [in0b]; 
                  to_result [in1b]; 
                  to_result [in2b]; 
                  to_result [in3b] |] in

              (* let in_x = Array.make 8 (to_result [("in", LS_0)]) in *)
              (* Array.iteri (fun x _ -> in_x.(x) <- to_result [(Printf.sprintf "in%d" x, LS_0)]) in_x; *)
              (* in_x.(1) <- to_result [(Printf.sprintf "in%d" 1, LS_0)]; *)
              
              (* Array.iteri (fun i e -> Printf.printf "in_%d:%s" i (unitToStr e)) in_x; *)

              (* let buf_out = Array.make 8 (to_result [("buf_out", LS_0)]) in
              Array.iteri (fun x _ -> (buf_out.(x) <- sBuf.solve in_x.(x))) buf_out; *)

              let and4_out = Array.make 8 (to_result [("and4_out", LS_0)]) in
              and4_out.(0) <- solve_unit [in_x.(0); in_s1_neg; in_s0_neg; in_ea_neg] sAnd4;
              and4_out.(1) <- solve_unit [in_x.(1); in_s1_neg; s0; in_ea_neg] sAnd4;
              and4_out.(2) <- solve_unit [in_x.(2); s1; in_s0_neg; in_ea_neg] sAnd4;
              and4_out.(3) <- solve_unit [in_x.(3); s1; s0; in_ea_neg] sAnd4;
              and4_out.(4) <- solve_unit [in_x.(4); in_s1_neg; in_s0_neg; in_eb_neg] sAnd4;
              and4_out.(5) <- solve_unit [in_x.(5); in_s1_neg; s0; in_eb_neg] sAnd4;
              and4_out.(6) <- solve_unit [in_x.(6); s1; in_s0_neg; in_eb_neg] sAnd4;
              and4_out.(7) <- solve_unit [in_x.(7); s1; s0; in_eb_neg] sAnd4;

              Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (resultToStr e)) and4_out; 

              let or4a_out = make_unit [and4_out.(0); and4_out.(1); and4_out.(2); and4_out.(3)] |> sOr4.solve in
              let or4b_out = make_unit [and4_out.(4); and4_out.(5); and4_out.(6); and4_out.(7)] |> sOr4.solve in
              (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])); *)
              
              make_unit [or4a_out; or4b_out; (to_result xs)];
              
          | _ -> Error { desc = "[s74hc153] missing input\n" } ;
         end
}

let s74hc283 : unit solver = {

  solve = fun result' -> 

      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> begin match unit' with
          | [] -> Error { desc = "[s74hc283] missing input\n" } ;
          | (cin'::a1'::a2'::a3'::a4'::b1'::b2'::b3'::b4'::xs) -> 
              let cin = to_result [cin'] in (* carry in *)

              let a1 = to_result [a1'] in
              let a2 = to_result [a2'] in
              let a3 = to_result [a3'] in
              let a4 = to_result [a4'] in

              let b1 = to_result [b1'] in
              let b2 = to_result [b2'] in
              let b3 = to_result [b3'] in
              let b4 = to_result [b4'] in

              let cin_neg = sNeg.solve cin in

              let nand2_out = Array.make 4 (to_result [("and2_out", LS_0)]) in
              let nor2_out = Array.make 4 (to_result [("nor2_out", LS_0)]) in

              nand2_out.(0) <- (make_unit [a1; b1]) |> sNand.solve;
              nor2_out.(0) <- (make_unit [a1; b1]) |> sNor.solve;
              nand2_out.(1) <- (make_unit [a2; b2]) |> sNand.solve;
              nor2_out.(1) <- (make_unit [a2; b2]) |> sNor.solve;
              nand2_out.(2) <- (make_unit [a3; b3]) |> sNand.solve;
              nor2_out.(2) <- (make_unit [a3; b3]) |> sNor.solve;
              nand2_out.(3) <- (make_unit [a4; b4]) |> sNand.solve;
              nor2_out.(3) <- (make_unit [a4; b4]) |> sNor.solve;

              let or2_out = Array.make 4 (to_result [("nor2_out", LS_0)]) in
              or2_out.(0) <- sNeg.solve nor2_out.(0);
              or2_out.(1) <- sNeg.solve nor2_out.(1);
              or2_out.(2) <- sNeg.solve nor2_out.(2);
              or2_out.(3) <- sNeg.solve nor2_out.(3);

              let l1out = Array.make 14 (to_result [("l1out", LS_0)]) in

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

              let l2out = Array.make 4 (to_result [("l2out", LS_0)]) in

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
              
              (make_unit [sum1; sum2; sum3; sum4; !overflow; (to_result xs)])

          | _ -> Error { desc = "[s74hc283] missing input\n" } ;
          end
}




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
                      match p1.solve (to_result [x1;x2;]) with
                        |Error error -> Error error
                        |Ok unit1 -> 
                          match p2.solve (to_result [x3; x4]) with 
                            |Ok unit2 ->  to_result (unit1@unit2)
                            |Error error -> Error error

} *)


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
  let t0 = [                       
             ("a0", LS_0);
             ("a1", LS_0);
             ("a2", LS_0);
             ("a3", LS_0);
             ("b0", LS_1);
             ("b1", LS_1);
             ("b2", LS_1);
             ("b3", LS_1);
            ] in

  let t1 = List.map (fun e -> Ok [e]) t0
  in
  (* let t1 = [                       
            Ok {lines = [("a0", LS_0)]};
            Ok {lines = [("a1", LS_0)]};
            Ok {lines = [("a2", LS_0)]};
            Ok {lines = [("a3", LS_0)]};
            Ok {lines = [("b0", LS_1)]};
            Ok {lines = [("b1", LS_1)]};
            Ok {lines = [("b2", LS_1)]};
            Ok {lines = [("b3", LS_1)]};
            ] in *)
  let t2 = make_unit t1
  in
  Printf.printf ">>> t1 result:\n%s\n" (unitToStr t0); 
  Printf.printf ">>> t2 result:\n%s\n" (resultToStr t2); 

  (* let s74hc251_out = to_result [("oe", LS_1); 
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

  (* let s74hc153_out = to_result [("ea", LS_1); 
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

 

              let carry0 = to_result [("cin", LS_0)] in

              let sum_a = Array.make 8 (to_result [("sum_a", LS_0)]) in
              sum_a.(0) <- to_result [("a0", LS_0)];
              sum_a.(1) <- to_result [("a1", LS_0)];
              sum_a.(2) <- to_result [("a2", LS_0)];
              sum_a.(3) <- to_result [("a3", LS_0)];
              sum_a.(4) <- to_result [("a4", LS_0)];
              sum_a.(5) <- to_result [("a5", LS_0)];
              sum_a.(6) <- to_result [("a6", LS_0)];
              sum_a.(7) <- to_result [("a7", LS_0)];

              let sum_b = Array.make 8 (to_result [("sum_a", LS_0)]) in
              sum_b.(0) <- to_result [("b0", LS_1)];
              sum_b.(1) <- to_result [("b1", LS_1)];
              sum_b.(2) <- to_result [("b2", LS_1)];
              sum_b.(3) <- to_result [("b3", LS_1)];
              sum_b.(4) <- to_result [("b4", LS_0)];
              sum_b.(5) <- to_result [("b5", LS_0)];
              sum_b.(6) <- to_result [("b6", LS_0)];
              sum_b.(7) <- to_result [("b7", LS_0)];

              let test = to_result [("carry", LS_0);
                                    ("a0", LS_0); 
                                    ("a1", LS_0);
                                    ("a2", LS_0);  
                                    ("a3", LS_0);                  
                                    ("b0", LS_1); 
                                    ("b1", LS_1);
                                    ("b2", LS_1);
                                    ("b3", LS_0) ] |> s74hc283.solve in
              Printf.printf ">>> test result:\n%s\n" (resultToStr test); 

              let sum1 = (make_unit [carry0; sum_a.(0); sum_a.(1); sum_a.(2); sum_a.(3); 
                                             sum_b.(0); sum_b.(1); sum_b.(2); sum_b.(3)] |> s74hc283.solve) in
              (* Printf.printf ">>> sum0 result:\n%s\n" (resultToStr sum1);   *)
              let a = sum1 >>= fun s74hc283_out -> 
                match s74hc283_out with
                  |s1::s2::s3::s4::carry1::sx -> 
                      let sum2 = (make_unit [to_result [carry1]; sum_a.(4); sum_a.(5); sum_a.(6); sum_a.(7); 
                                                              sum_b.(4); sum_b.(5); sum_b.(6); sum_b.(7)] |> s74hc283.solve) in
                      sum2 >>= fun s74hc283_out2 -> 
                          begin match s74hc283_out2 with
                            |s5::s6::s7::s8::carry2::sx -> 
                                    (* Printf.printf ">>> result:\n%s\n" (resultToStr (Ok {lines=[s1; s2; s3; s4; s5; s6; s7; s8; carry2]})); *)
                                    Ok []
                            | _ -> Ok []
                          end

                  | _ -> Ok []
  in Printf.printf "bye.";
  (* in
  Printf.printf ">>> result:\n%s\n"  (resultToStr a) *)

  (* {lines=[("a1", LS_0)]}; *)
  (* : output -> out_y neg_y; *)

  (* Printf.printf ">>> result:\n%s\n"  (resultToStr  s74hc283_out2 >>= fun l -> );  *)

  (* let tracks : lineMapT ref = ref ListMap.empty in
  let units : unitMapT ref = ref ListMap.empty in
  "x"::"y"::"z"::[] |> insertLines tracks;
  showLines tracks; *)
  
              (* let test = make_unit [ Ok (to_unit [("b0", LS_0)] +++ 
                             to_unit [("b1", LS_1)] +++
                             to_unit [("b2", LS_1)] +++
                             to_unit [("b3", LS_0)] )] |> s74hc283.solve in *)
  
(*  result: [out]:low [2]:high [3]:low [out]:low [3]:low  mapowanie wyniku i zastosowanie >>= *)
(* brak przypisania etykiety do czegokolwiek eq. można mieć dwie takie same.. *)
