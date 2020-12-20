open LogicUnit


(* 74hc251 multiplexer simulation *)
let s74hc251 : unit solver = {

  solve = fun (input: unit list) -> match input with

      | [(lab, [])] -> Error { desc = "[s74hc251] missing input\n" } ;
      | [(lab, oe::s2::s1::s0::in7::in6::in5::in4::in3::in2::in1::in0::xs)] -> 

          let in_oe_neg = lNot oe in
          let in_s0_neg = lNot s0 in
          let in_s1_neg = lNot s1 in
          let in_s2_neg = lNot s2 in

          let and4_out_0 = lAnd4 in0 in_s2_neg in_s1_neg in_s0_neg in
          let and4_out_1 = lAnd4 in1 in_s2_neg in_s1_neg s0 in
          let and4_out_2 = lAnd4 in2 in_s2_neg s1 in_s0_neg in
          let and4_out_3 = lAnd4 in3 in_s2_neg s1 s0 in
          let and4_out_4 = lAnd4 in4 s2 in_s1_neg in_s0_neg in
          let and4_out_5 = lAnd4 in5 s2 in_s1_neg s0 in
          let and4_out_6 = lAnd4 in6 s2 s1 in_s0_neg in
          let and4_out_7 = lAnd4 in7 s2 s1 s0 in

          let nor8_out = lNor (lOr4 and4_out_0  and4_out_1  and4_out_2  and4_out_3) 
                              (lOr4 and4_out_4  and4_out_5  and4_out_6  and4_out_7) in
          (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out]))  *)
          
          let out_y = lTbuf nor8_out in_oe_neg in
          let neg_y = lTnot nor8_out in_oe_neg in

          Ok (("buf_out.", [out_y]) ++ ("neg_out.", [neg_y]) ++ (".", xs))  (* most imported bits first *)
      | [(lab, _)] -> Error {desc = (Printf.sprintf "s74hc251 FAIL - %s missing inputs\n" lab)}
      | _ -> Error { desc = "s74hc251 FAIL missing unit input\n" } ;
      
}

(* s74hc153 multiplexer simulation *)
let s74hc153 : unit LogicUnit.solver = {

  solve = fun input -> begin match input with

          | [(lab, [])] -> Error { desc = "[s74hc153] missing input\n" } ;
          | [(lab, ea::eb::s1::s0::in3a::in2a::in1a::in0a::in3b::in2b::in1b::in0b::xs)] -> 
          (* Printf.printf "MUX153 IN %s %s %s\n" (unitToStr2 ("es.eb.s1.s0", ea::eb::s1::s0::[])) (unitToStr2 ("ina", in3a::in2a::in1a::in0a::[])) (unitToStr2 ("inb", in3b::in2b::in1b::in0b::[])); *)
            
            let in_ea_neg = lNot ea in
            let in_eb_neg = lNot eb in 
            let in_s0_neg = lNot s0 in 
            let in_s1_neg = lNot s1 in 
            
            let or4a_out = lOr4
                (lAnd4 in0a in_s1_neg in_s0_neg in_ea_neg)
                (lAnd4 in1a in_s1_neg s0 in_ea_neg)
                (lAnd4 in2a s1 in_s0_neg in_ea_neg)
                (lAnd4 in3a s1 s0 in_ea_neg)
            in
            let or4b_out = lOr4
                (lAnd4 in0b in_s1_neg in_s0_neg in_eb_neg)
                (lAnd4 in1b in_s1_neg s0 in_eb_neg)
                (lAnd4 in2b s1 in_s0_neg in_eb_neg)
                (lAnd4 in3b s1 s0 in_eb_neg)
            in
            (* Printf.printf "MUX153 OUT %s %s\n" (unitToStr2 ("or4a_out", or4a_out)) (unitToStr2 ("or4b_out", or4b_out)); *)
            Ok (lab, or4a_out::or4b_out::xs)            (* most imported bits first *)  
    
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
let s74hc283 : unit LogicUnit.solver = {

  solve = fun input -> 
          begin match input with
          | [(lab, [])] -> Error { desc = "[s74hc283] missing input\n" }
          | [(lab, cin::a4::a3::a2::a1::b4::b3::b2::b1::xs)] -> 

            let cin_neg = lNot cin in
            let nand20 = lNand a1 b1 in
            let nor20 = lNor a1 b1 in
            let nand21 = lNand a2 b2 in
            let nor21 = lNor a2 b2 in
            let nand22 = lNand a3 b3 in
            let nor22 = lNor a3 b3 in 
            let nand23 = lNand a4 b4 in
            let nor23 = lNor a4 b4 in 

            let nor20n = lNot nor20 in 
            let nor21n = lNot nor21 in 
            let nor22n = lNot nor22 in 
            let nor23n = lNot nor23 in 

            let nor32021 = lNor3 
                                (lAnd3 cin_neg nand20 nand21) 
                                (lAnd nor20n nand21)
                                nor21 in

            let nor41 = lNor4
                              (lAnd4 cin_neg nand20 nand21 nand22) 
                              (lAnd3 nand21 nand22 nor20)
                              (lAnd nand22 nor21)
                              nor22 in

            let sum1 = lXor cin (lAnd nor20n nand20) in
            let sum2 = lXor 
                           (lNor(lAnd cin_neg nand20) nor20) 
                           (lAnd nor21n nand21) in
            let sum3 = lXor nor32021 (lAnd nor22n nand22) in
            let sum4 = lXor nor41 (lAnd nor23n nand23) in
            let overflow = lNor5 
                              (lAnd5 cin_neg nand20 nand21 nand22 nand23) 
                              (lAnd4 nand21 nand22 nand23 nor20)  
                              (lAnd3 nand22 nand23 nor21)
                              (lAnd nand23 nor22) 
                              nor23 in

            (* Printf.printf "nor8_out: %s\n"  (unitToStr (make_unit[nor8_out])) ++ *)
            
            Ok (("sum", [sum4; sum3; sum2; sum1; overflow]) ++ ("rest.", xs)) (* most imported bits first *)

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
let s2x74hc283 : unit LogicUnit.solver = {

  solve = fun input -> 
    (* input >>= fun unit' ->  *)
    match input with
      | [] -> Error { desc = "[s2x74hc283] missing input\n" };
      | [(lab, carry0::a7::a6::a5::a4::a3::a2::a1::a0::b7::b6::b5::b4::b3::b2::b1::b0::xs)] -> 
         (* Printf.printf "s2x74hc283 INPUT: %s\n" (unitToStr2 ("input.", carry0::a7::a6::a5::a4::a3::a2::a1::a0::b7::b6::b5::b4::b3::b2::b1::b0::xs)); *)
         [("1-283.", [carry0; a3; a2; a1; a0; b3; b2; b1; b0])] |> s74hc283.solve >>= 
         
         fun sum1_u -> 
          (* Printf.printf "s2x74hc283 result1: %s\n" (unitToStr2 sum1_u); *)
          begin match sum1_u with
            | (_, s4::s3::s2::s1::carry1::sx) ->  
             (* Printf.printf "s2x74hc283 input: %s\n" (unitToStr2 ("2-283.", [carry1; a7; a6; a5; a4; b7; b6; b5; b4])); *)
              [("2-283.", [carry1; a7; a6; a5; a4; b7; b6; b5; b4])] |> s74hc283.solve >>= 
              
                fun sum2_u -> 
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
let sGigatronALU : unit LogicUnit.solver = {

  solve = fun input -> 
    (* input >>= fun unit' -> ea::eb::s1::s0::in3a::in2a::in1a::in0a::in3b::in2b::in1b::in0b  *)
    match input with
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


let sMain : unit LogicUnit.solver = {
  solve = fun input -> 
      match input with
      | [(lab, d::c::b::a::en_clk::clk::xs)] -> 
      
        let clk' = lNor clk en_clk in

          let ca = clk' in

          let a' = lOr (lAnd a (lNot ca)) (lAnd (lNot a) ca) in
          let ca = lAnd a clk' in

          let b' = lOr (lAnd b (lNot ca)) (lAnd (lNot b) ca) in
          let ca = lAnd3 b a clk' in

          let c' = lOr (lAnd c (lNot ca)) (lAnd (lNot c) ca) in
          let ca = lAnd4 a b c clk' in

          let d' = lOr (lAnd d (lNot ca)) (lAnd (lNot d) ca) in
          let ca = lAnd d clk' in

          Ok (lab, d'::c'::b'::a'::en_clk::clk'::xs)

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

  let _ = match test_loop {tick = 48; units = [("test", l @ l @ l @ l @ l @ h @ l)] } with
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
  
  solve = fun input -> 
      match input with
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


