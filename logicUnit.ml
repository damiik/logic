(* module LogicUnit = struct *)

    type lineStateT = LS_1 | LS_0 | LS_X


    let lAnd = fun (a: lineStateT) (b : lineStateT) -> 

    if a == LS_1 && b == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 then LS_0
    else
          LS_X

    let lAnd3 = fun (a: lineStateT) (b : lineStateT) (c : lineStateT) -> 

    if a == LS_1 && b == LS_1 && c == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 || c == LS_0 then LS_0
    else
          LS_X

    let lAnd4 = fun (a: lineStateT) (b : lineStateT) (c : lineStateT) (d : lineStateT) -> 

    if a == LS_1 && b == LS_1 && c == LS_1 && d == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 || c == LS_0 || d == LS_0 then LS_0
    else
          LS_X

    let lAnd5 = fun (a: lineStateT) (b : lineStateT) (c : lineStateT) (d : lineStateT) (e : lineStateT) -> 

    if a == LS_1 && b == LS_1 && c == LS_1 && d == LS_1 && e == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 || c == LS_0 || d == LS_0 || e == LS_0 then LS_0
    else
          LS_X

    let lNand = fun (a: lineStateT) (b : lineStateT) -> 
    if a == LS_1 && b == LS_1 then LS_0
    else if a == LS_0 || b == LS_0 then LS_1
    else
          LS_X

    let lOr = fun (a: lineStateT) (b : lineStateT) -> 
    if a == LS_0 && b == LS_0 then LS_0
    else if a == LS_1 || b == LS_1 then LS_1
    else
          LS_X


    let lOr4 = fun (a: lineStateT) (b : lineStateT) (c : lineStateT) (d : lineStateT) -> 

    if a == LS_0 && b == LS_0 && c == LS_0 && d == LS_0 then LS_0
    else if a == LS_1 || b == LS_1 || c == LS_1 || d == LS_1 then LS_1
    else
          LS_X

    let lNor = fun (a: lineStateT) (b : lineStateT) -> 
    if a == LS_0 && b == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 then LS_0
    else
          LS_X
        
    let lNor3 = fun (a: lineStateT) (b: lineStateT) (c: lineStateT) -> 
    if a == LS_0 && b == LS_0 && c == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 || c == LS_1 then LS_0
    else
          LS_X

    let lNor4 = fun (a: lineStateT) (b: lineStateT) (c: lineStateT)  (d: lineStateT) -> 
    if a == LS_0 && b == LS_0 && c == LS_0 && d == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 || c == LS_1 || d == LS_1 then LS_0
    else
          LS_X

    let lNor5 = fun (a: lineStateT) (b: lineStateT) (c: lineStateT) (d: lineStateT)  (e: lineStateT) -> 
    if a == LS_0 && b == LS_0 && c == LS_0 && d == LS_0  && e == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 || c == LS_1 || d == LS_1 || e == LS_1 then LS_0
    else
          LS_X


    let lXor = fun (a: lineStateT) (b : lineStateT) -> 

    if (a == LS_X || b == LS_X) then LS_X
    else if a != b then LS_1
    else
          LS_0

    let lNot = fun (a: lineStateT) -> 
        if a == LS_X then LS_X
        else
          if a == LS_1 then LS_0
          else LS_1

    let lTnot = fun (oe: lineStateT) (a: lineStateT) -> 
        if oe != LS_1 then LS_X
        else 
          if a == LS_X then LS_X
          else
            if a == LS_1 then LS_0
            else LS_1

    let lTbuf = fun (oe: lineStateT) (a: lineStateT) -> 
        if oe != LS_1 then LS_X
        else 
          if a == LS_X then LS_X
          else a


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


(* end *)