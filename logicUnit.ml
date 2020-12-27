(* module LogicUnit = struct *)

open LogicBase

    type unitSignals = signalValueT list

    type unit = (string * unitSignals)
    type error = {desc: string;}

    type 'a linesResult = (('a, error) result) 


    type 'a solver = {

      solve: 'a list -> 'a linesResult; (* type: result of (input * 'a, error) *)
    }


    let unitToStr (out: unit) : string =

    let rec f = fun l i ->
      match l with
      | [] -> ""
      | s::xs -> begin match s with

        | Bit a -> if a <= (Int32.one) then Printf.sprintf "%d:<%ld>, %s" i a (f xs (i+1)) else Printf.sprintf "%d:<*>, %s" i (f xs (i+1))
        | Vector (a, l) -> Printf.sprintf "%d:<0x%lx:(%d)>, %s" i a l (f xs (i+1));

      end
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
      | s::xs -> begin match s with

        | Bit a -> if a <= (Int32.one) && a >= (Int32.zero) then Printf.sprintf "%ld%s"a (f xs) else Printf.sprintf "{*}%s" (f xs)
        | Vector (a, l) -> 
            let mask = Int32.shift_right_logical (Int32.of_int 0xffffffff) (32 - l) in
            Printf.sprintf " (0x%lx :%d),%s" (Int32.logand a mask) l (f xs);
      end
    in
    match out with
    | (lab, signals) -> Printf.sprintf "%s [ %s ]" lab (f signals)




    let signalsToVectors (s: signalValueT list) : signalValueT list =

    let rec f = fun signals' acc b ->
      match signals' with
      | [] -> acc
      | s::xs -> begin match s with
        | Bit a -> 
            begin match acc with
            |((Vector (xacc, l))::xsacc) -> if a = (Int32.one) then (f xs ((Vector ((Int32.add xacc (Int32.shift_left Int32.one l)), l+1))::xsacc) (l+1)) 
                else if a = (Int32.zero) then (f xs ((Vector (xacc, l+1))::xsacc) (l+1))
                else (f xs ((Vector (xacc, 0))::xsacc) (l+1))
            |[] -> if a = (Int32.one) then (f xs [Vector ((Int32.shift_left Int32.one b), b+1)] (b+1)) 
                else if a = (Int32.zero) then (f xs [Vector (Int32.zero, b+1)] (b+1))
                else (f xs [Vector ((Int32.zero), 0)] (b+1))   
            | _ -> (f xs [Vector ((Int32.zero), 0)] (b+1)) (* could not exists *)
            end
        | Vector a -> (f xs ((Vector a)::acc) 0); (* if it's a Vector just copy them and reset current bit level*)
        end
    in
    f s [] 0

    (* list: [Bit Bit Bit Vector(8) Bit Bit Bit Bit] will be converted to: [Vector(3) Vector(12)] *)
    let unitToVectors (out: unit) : unit =

    let (lab, signals) = out in
    (lab, (signalsToVectors signals))


    let signalsToBits (s: signalValueT list) : signalValueT list =

    let rec f = fun signals' acc ->
      match signals' with
      | [] -> acc
      | s::xs -> begin match s with
        | Bit a -> f xs ((Bit a)::acc);
        | Vector (v, 0) -> f xs acc;
        | Vector (v, l) -> 
          let mask1 = Int32.shift_right_logical (Int32.of_int 0xffffffff) (32 - (l-1)) in
          f ((Vector (Int32.logand mask1 v, l-1))::xs) (acc @ [Bit (Int32.shift_right_logical v (l-1))]);
      end
    in
    f s []

    let signalToBitsArr (s: signalValueT) : signalValueT array =

      let rec f = fun v l acc ->
        if l = 0 then acc
        else 
          let mask1 = Int32.shift_right_logical (Int32.of_int 0xffffffff) (32 - (l-1)) in
          (acc.(l-1) <- Bit (Int32.shift_right_logical v (l-1)));
          f (Int32.logand mask1 v) (l-1) acc
      in
      match s with
        |Vector (s', l) -> f s' l (Array.make l (Bit Int32.zero))
        |Bit a -> (Array.make 1 (Bit a))


    let bitsArrToVector(a: signalValueT array) : signalValueT =

      let r = Array.fold_left (fun acc e -> 
      
        match e with
        | Bit bv -> 
          let (v, l) = acc in
            (Int32.add v (Int32.shift_left bv l), l+1)
        | _ -> acc
      
       )  (Int32.zero, 0) a 
       in
       Vector r   

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


    let n2Unit (n : int) lab s : unit = 

      let u : int32 = if n < 0 then Int32.add (Int32.lognot (Int32.of_int (-n))) Int32.one
        else (Int32.of_int n) 
      in (* (if n<0 then (lnot (-n) + 1) else n)  *)

      let rec f = fun (n: int32) (acc: signalValueT list) (i:int) : signalValueT list ->
        if s > i then 
          (f (Int32.shift_right_logical n 1) 
              ((if (Int32.logand n (Int32.of_int 0x01)) > Int32.zero then (Bit Int32.one) else (Bit Int32.zero))::[])
              (i + 1)
          ) @ acc        
        else 
          acc
      in
      (lab,  (f u [] 0))


    let ( >>= )  (p: 'a linesResult) (f: 'a -> 'b linesResult)  : 'b linesResult =

    match p with
    | Ok a -> f a
    | Error error -> Error error


    (* monoid ++ *)
    let ( ++ ) (u1: unit) (u2: unit) : unit =

    match (u1, u2) with
    | ((lab1, u1signals), (lab2, u2signals)) -> ((lab1 ^ lab2), u1signals @ u2signals)


    let rec lMap (l: unit) (f: signalValueT -> signalValueT) (acc: signalValueT list) =

      match l with
      | (lab, []) -> (lab, acc)
      | (lab, x::xs) -> lMap (lab, xs) f ((f x)::acc)




    (* let ( ++ ) (u1: unit linesResult ) (u2: unit linesResult) : unit linesResult =

    u1 >>= fun u1' -> u2 >>= fun u2' -> 

      match (u1', u2') with
      | ((lab1, u1signals), (lab2, u2signals)) -> Ok ((lab1 ^ lab2), u1signals @ u2signals) *)



    (* compare without labels *)
    let test_unit (p1: unit) (p2: unit) : unit linesResult = 

      match (p1, p2) with
        | ((lab1, u1signals), (lab2, u2signals)) -> 

            let rec f (l1: signalValueT list) (l2: signalValueT list) : unit linesResult = 
            match l1 with
            | [] -> begin match l2 with       
                          | [] -> Ok (lab1^lab2, [])
                          | x2::x2s -> Error {desc = Printf.sprintf "unit %s too short compared to %s" lab1 lab2}
                          end
            | x1::x1s -> begin match l2 with
                                | [] -> Error {desc = "second unit too short"}
                                | x2::x2s -> 
                                  (* Printf.sprintf "signal levels not equal [%s] <> [%s]" ; *)
                                  if not (isEqual x1 x2) then Error {desc = Printf.sprintf "signal levels not equal [%s] <> [%s]" lab1 lab2}
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


(* end *)