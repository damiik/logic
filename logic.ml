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

let sAnd4 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::(_, LS_High)::(_, LS_High)::xs -> to_unit (("sAnd4", LS_High)::xs) 
                    | _::_::_::_::xs -> to_unit (("sAnd4", LS_Low)::xs) 
                    | _ -> Error {desc = "[sAnd4] missing input\n"}  
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

let sNor8 : linesResult solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("sNor8", LS_High)::xs)
                    | _::_::_::_::_::_::_::_::xs -> to_unit (("sNor8", LS_Low)::xs)
                    | _ -> Error {desc = "[sNor8] missing input\n"} 
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

  let s74hc251_out = to_unit [("oe", LS_High); 
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
                              ] |> s74hc251.solve in

  (* : output -> out_y neg_y; *)
  Printf.printf ">>> result: %s\n"  (unitToStr  s74hc251_out);

  (* let tracks : lineMapT ref = ref ListMap.empty in
  let units : unitMapT ref = ref ListMap.empty in
  "x"::"y"::"z"::[] |> insertLines tracks;
  showLines tracks; *)
  

  
(*  result: [out]:low [2]:high [3]:low [out]:low [3]:low  mapowanie wyniku i zastosowanie >>= *)
(* brak przypisania etykiety do czegokolwiek eq. można mieć dwie takie same.. *)
