(* type and4 = { in0: int; in1: int ; in2: int; in3: int; output: int } *)

open Table;;
open Array;;

type lineStateT = LS_High | LS_Low | LS_Open 

type signal = (string * lineStateT)

type unit = { lines: signal list; }

type error = { desc: string;}

(* type mess = SUnit of signal list | MUnit of ((unit, error) result) list *)
type mess = SUnit of ((unit, error) result) | MUnit of (((unit, error) result) list)

type 'a solver = {
  solve: (unit, error) result -> (unit, error) result (* type: result of (input * 'a, error) *)
}



(* return? *)
(* type 'a tree = Leaf of 'a | Node of 'a tree list *)

(* let make_unit (s: mess) : unit = 

  let rec f = fun m -> 

        let conv = fun a -> match a with
                  | Ok (unit', _) -> unit'.lines;
                  | Error error -> Error error;
        in
        match m with
        | SUnit x ->  x;
        | MUnit x ->  f (conv x);
  in
  { lines = f s  } 
*)

(*
type mess = SUnit of ((unit, error) result) | MUnit of ((unit, error) result) list 
type unit = { lines: signal list; }
*)
let to_unit s = Ok {lines = s}

let make_unit (s: mess) : (unit, error) result = 

    let rec f (m : mess) (acc : signal list) : (signal list) =
        match m with
          | SUnit x -> begin
            match x with
                | Error e -> acc
                | Ok u -> (u.lines @ acc)
          end
          | MUnit l -> begin
            match l with 
                | [] -> acc            
                | x::[] -> begin match x with
                                  | Error e -> acc
                                  | Ok u -> u.lines @ acc
                           end
                | (x::xs) -> begin match x with
                                  | Error e -> acc
                                  | Ok u -> (f (MUnit xs) (u.lines @ acc))
                           end
          end

          
    in
    to_unit (f s [])
    (* (Ok { lines = (f s []) }) *)


(* 
      let rec f = fun m -> 

        let conv = fun a -> match a with
                  | Ok (unit', _) -> unit'.lines;
                  | Error error -> Error error;
        in
        match m with
        | SUnit x ->  x;
        | MUnit x ->  f (conv x);
  in
  { lines = f s  } 
*)

let sNeg : ((unit, error) result) solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e      
      | Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "[NEG] missing input"}
                    | (_, LS_High)::xs -> to_unit (("out", LS_Low)::xs)
                    | _::xs -> to_unit (("out", LS_High)::xs)
} 

let sBuf : ((unit, error) result) solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "[NEG] missing input"} 
                    | (_, b)::xs -> to_unit (("out", b)::xs)
} 

let sAnd : ((unit, error) result) solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::xs -> to_unit (("out", LS_High)::xs) 
                    | _::_::xs -> to_unit (("out", LS_Low)::xs) 
                    | _ -> Error {desc = "[AND2] missing inputs"}  
} 

let sAnd4 : ((unit, error) result) solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_High)::(_, LS_High)::(_, LS_High)::(_, LS_High)::xs -> to_unit (("out", LS_High)::xs) 
                    | _::_::_::_::xs -> to_unit (("out", LS_Low)::xs) 
                    | _ -> Error {desc = "[AND4] missing inputs"}  
} 

let sOr : ((unit, error) result) solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with  
                    | (_, LS_Low)::(n2, LS_Low)::xs -> to_unit (("out", LS_Low)::xs)
                    | _::_::xs -> to_unit (("out", LS_High)::xs)
                    | _ -> Error {desc = "[OR2] missing 2 inputs"}   
}  

let sNor8 : ((unit, error) result) solver = {

  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      |  Ok unit' -> match unit'.lines with
                    | (_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::(_, LS_Low)::xs -> to_unit (("out", LS_High)::xs)
                    | _::_::_::_::_::_::_::_::xs -> to_unit (("out", LS_Low)::xs)
                    | _ -> Error {desc = "[OR8] missing inputs"} 
}   


(* let ( >>= )  (p: 'a solver) (f: 'a -> 'b solver)  : 'b solver  = { 
  
  solve = fun unit -> 
      match p.solve unit with
      | Ok unit' -> (f unit').solve unit';
      | Error error -> Error error;
} *)

(* unit -> (unit, error) result
  signal list
*)
let return (x: 'a) : 'a solver  = { solve = fun _ -> Ok {lines=x;} }


(* let ( <*> ) (p1: 'a solver) (p2: 'b solver): 'c solver  = { 
  
  solve = fun input -> 
  
    input |> (p1 >>= fun a -> p2 >>= fun b -> 
      return (a.lines @ b.lines)).solve;
}  *)

 let ( <*> ) (p1: 'a solver) (p2: 'b solver): 'c solver  = { 
  
  solve = fun result' -> 
      match result' with
      | Error e ->  Error e 
      | Ok unit' -> match unit'.lines with
                    | [] -> Error {desc = "<*> missing input 0"} 
                    |x1::[] -> Error {desc = "<*> missing input 1"} 
                    |x1::x2::[] -> Error {desc = "<*> missing input 2"} 
                    |x1::x2::x3::[] -> Error {desc = "<*> missing input 3"} 
                    |(x1::x2::x3::x4::xs) -> 
                      match p1.solve (to_unit (x1::x2::[])) with
                        |Error error -> Error error
                        |Ok unit1 -> 
                          match p2.solve (to_unit (x3::x4::[])) with 
                            |Ok unit2 ->  to_unit (unit1.lines@unit2.lines)
                            |Error error -> Error error

}

let unitToStr (out: (unit, error) result) : string =

  match out with
  | Error error -> error.desc
  | Ok o -> 
    let rec f = fun l ->
      match l  with 
      | [] -> ""
      | (s, LS_High)::xs -> Printf.sprintf "[%s]:high %s\n" s (f xs);
      | (s, LS_Low)::xs -> Printf.sprintf "[%s]:low %s\n" s (f xs);
      | (s, LS_Open)::xs -> Printf.sprintf "[%s]:open %s\n" s (f xs);

    in
    f o.lines


type unitT = VCC | GND | Not | And2 
(* type unitStateT = (unitT, f) *)

type lineMapT = (string, lineStateT) ListMap.t

type unitMapT = (string, unitT) ListMap.t

let lineStatetoStr (state: lineStateT option) : string =

  match state with 
  | Some(LS_High) -> "state:high" 
  | Some(LS_Low) -> "state:low"
  | Some(LS_Open) -> "state:open"
  | _ -> "?"

let rec insertLines (t : lineMapT ref) (k : string list) =
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
  f (ListMap.bindings !t)
    
let () = 
  let tracks : lineMapT ref = ref ListMap.empty in
  let units : unitMapT ref = ref ListMap.empty in
  "x"::"y"::"z"::[] |> insertLines tracks;
  showLines tracks;
  
  (* 74hc251 multiplexer simulation *)
  
  let oe = Ok {lines=("oe", LS_High)::[]} in
  let s0 = Ok {lines=("s0", LS_Low)::[]} in
  let s1 = Ok {lines=("s1", LS_Low)::[]} in
  let s2 = Ok {lines=("s2", LS_Low)::[]} in

  let in_x = Array.make 8 (to_unit (("in", LS_Low)::[])) in
  Array.iteri (fun x _ -> in_x.(x) <- (to_unit ((Printf.sprintf "in%d" x, LS_Low)::[]))) in_x;
  in_x.(0) <- (to_unit ((Printf.sprintf "in%d" 0, LS_High)::[]));
  Array.iteri(fun i e -> Printf.printf "in_%d:%s" i (unitToStr e)) in_x;
  let in_oe_neg = sNeg.solve oe in
  let in_s0_neg = sNeg.solve s0 in
  let in_s1_neg = sNeg.solve s1 in
  let in_s2_neg = sNeg.solve s2 in

  let buf_out = Array.make 8 (to_unit (("buf_out", LS_Low)::[])) in

  Array.iteri (fun x _ -> (buf_out.(x) <- sBuf.solve in_x.(x))) buf_out;

  let and4_out = Array.make 8 (to_unit (("and4_out", LS_Low)::[])) in

  and4_out.(0) <- (make_unit (MUnit (buf_out.(0)::in_s2_neg::in_s1_neg::in_s0_neg::[])) |> sAnd4.solve);
  and4_out.(1) <- (make_unit (MUnit (buf_out.(1)::in_s2_neg::in_s1_neg::s0::[])) |> sAnd4.solve);
  and4_out.(2) <- (make_unit (MUnit (buf_out.(2)::in_s2_neg::s1::in_s0_neg::[])) |> sAnd4.solve);
  and4_out.(3) <- (make_unit (MUnit (buf_out.(3)::in_s2_neg::s1::s0::[])) |> sAnd4.solve);
  and4_out.(4) <- (make_unit (MUnit (buf_out.(4)::s2::in_s1_neg::in_s0_neg::[])) |> sAnd4.solve);
  and4_out.(5) <- (make_unit (MUnit (buf_out.(5)::s2::in_s1_neg::s0::[])) |> sAnd4.solve);
  and4_out.(6) <- (make_unit (MUnit (buf_out.(6)::s2::s1::in_s0_neg::[])) |> sAnd4.solve);
  and4_out.(7) <- (make_unit (MUnit (buf_out.(7)::s2::s1::s0::[])) |> sAnd4.solve);

  Array.iteri(fun i e -> Printf.printf "and4_out%d:%s" i (unitToStr e)) and4_out;

  let nor8_out = make_unit (MUnit (and4_out.(0)::and4_out.(1)::and4_out.(2)::and4_out.(3)::and4_out.(4)::and4_out.(5)::and4_out.(6)::and4_out.(7)::[])) |> sNor8.solve in

  (* let out_y = make_unit (MUnit (in_oe_neg::nor8_out::[])) |> sTbuf.solve in
  let neg_y = make_unit (MUnit (in_oe_neg::nor8_out::[])) |> sTneg.solve *)

  (* : output -> out_y neg_y; *)
  Printf.printf " result: %s\n"  (unitToStr nor8_out); 

  
(*  result: [out]:low [2]:high [3]:low [out]:low [3]:low  mapowanie wyniku i zastosowanie >>= *)
(* brak przypisania etykiety do czegokolwiek eq. można mieć dwie takie same.. *)
