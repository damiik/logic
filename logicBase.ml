    type signalValueT = Bit of int32 | Vector of (int32 * int)


    let lAnd = fun (a: signalValueT) (b : signalValueT) -> 
    match (a,b) with
    |(Bit a, Bit b) -> Bit  (Int32.logand a b)
    | (Vector (a, l), Vector (b, _)) -> Vector ((Int32.logand a b), l)
    |_ -> Bit Int32.max_int


    (* if a == LS_1 && b == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 then LS_0
    else
          LS_X *)

    let lAnd3 = fun (a: signalValueT) (b : signalValueT) (c : signalValueT) ->
    match (a,b,c) with
    |(Bit a, Bit b, Bit c) -> Bit (Int32.logand a (Int32.logand b c))
    | (Vector (a, l), Vector (b, _), Vector (c, _)) -> Vector ((Int32.logand a (Int32.logand b c)), l)
    |_ -> Bit Int32.max_int

    (* if a == LS_1 && b == LS_1 && c == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 || c == LS_0 then LS_0
    else
          LS_X *)

    let lAnd4 = fun (a: signalValueT) (b : signalValueT) (c : signalValueT) (d : signalValueT) ->
    match (a,b,c,d) with
    |(Bit a, Bit b, Bit c, Bit d) -> Bit (Int32.logand a (Int32.logand b (Int32.logand c d)))
    | (Vector (a, l), Vector (b, _), Vector (c, _), Vector (d, _)) -> Vector ((Int32.logand a (Int32.logand b (Int32.logand c d))), l)
    |_ -> Bit Int32.max_int
    (* if a == LS_1 && b == LS_1 && c == LS_1 && d == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 || c == LS_0 || d == LS_0 then LS_0
    else
          LS_X *)

    let lAnd5 = fun (a: signalValueT) (b : signalValueT) (c : signalValueT) (d : signalValueT) (e : signalValueT) -> 
    match (a,b,c,d,e) with
    |(Bit a, Bit b, Bit c, Bit d, Bit e) -> Bit (Int32.logand a (Int32.logand b (Int32.logand c (Int32.logand d e)))) 
    | (Vector (a, l), Vector (b, _), Vector (c, _), Vector (d, _), Vector (e, _)) -> Vector ((Int32.logand a (Int32.logand b (Int32.logand c (Int32.logand d e)))), l)
    |_ -> Bit Int32.max_int

    (* if a == LS_1 && b == LS_1 && c == LS_1 && d == LS_1 && e == LS_1 then LS_1
    else if a == LS_0 || b == LS_0 || c == LS_0 || d == LS_0 || e == LS_0 then LS_0
    else
          LS_X *)

    let lNand = fun (a: signalValueT) (b : signalValueT) -> 
    match (a,b) with
    |(Bit a, Bit b) -> Bit (Int32.logxor (Int32.of_int 0x1) (Int32.logand a b))
    | (Vector (a, l), Vector (b, _)) -> Vector ((Int32.lognot (Int32.logand a b)), l)  (* lognot will negate all 32bits! *)
    |_ -> Bit Int32.max_int
    (* if a == LS_1 && b == LS_1 then LS_0
    else if a == LS_0 || b == LS_0 then LS_1
    else
          LS_X *)

    let lOr = fun (a: signalValueT) (b : signalValueT) -> 
    match (a,b) with
    |(Bit a, Bit b) -> Bit (Int32.logor a b) 
    | (Vector (a, l), Vector (b, _)) -> Vector ((Int32.logor a b), l)
    |_ -> Bit Int32.max_int
    (* if a == LS_0 && b == LS_0 then LS_0
    else if a == LS_1 || b == LS_1 then LS_1
    else
          LS_X *)


    let lOr4 = fun (a: signalValueT) (b : signalValueT) (c : signalValueT) (d : signalValueT) -> 
    match (a,b,c,d) with
    |(Bit a, Bit b, Bit c, Bit d) -> Bit (Int32.logor a (Int32.logor b (Int32.logor c d))) 
    | (Vector (a, l), Vector (b, _), Vector (c, _), Vector (d, _)) -> Vector ((Int32.logor a (Int32.logor b (Int32.logor c d))), l)
    |_ -> Bit Int32.max_int
    (* if a == LS_0 && b == LS_0 && c == LS_0 && d == LS_0 then LS_0
    else if a == LS_1 || b == LS_1 || c == LS_1 || d == LS_1 then LS_1
    else
          LS_X *)

    let lNor = fun (a: signalValueT) (b : signalValueT) -> 
    match (a,b) with
    |(Bit a, Bit b) -> Bit (Int32.logxor (Int32.of_int 0x1) (Int32.logor a b)) 
    | (Vector (a, l), Vector (b, _)) -> Vector ((Int32.lognot (Int32.logor a b)), l)   (* lognot will negate all 32bits! *)
    |_ -> Bit Int32.max_int
    (* if a == LS_0 && b == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 then LS_0
    else
          LS_X *)
        
    let lNor3 = fun (a: signalValueT) (b: signalValueT) (c: signalValueT) ->
    match (a,b,c) with
    |(Bit a, Bit b, Bit c) -> Bit (Int32.logxor (Int32.of_int 0x1) (Int32.logor a (Int32.logor b  c))) 
    | (Vector (a, l), Vector (b, _), Vector (c, _)) -> Vector ((Int32.lognot (Int32.logor a (Int32.logor b c))), l)
    |_ -> Bit Int32.max_int
    (* if a == LS_0 && b == LS_0 && c == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 || c == LS_1 then LS_0
    else
          LS_X *)

    let lNor4 = fun (a: signalValueT) (b: signalValueT) (c: signalValueT)  (d: signalValueT) -> 
    match (a,b,c,d) with
    |(Bit a, Bit b, Bit c, Bit d) -> Bit (Int32.logxor (Int32.of_int 0x1) (Int32.logor a (Int32.logor b (Int32.logor c d)))) 
    | (Vector (a, l), Vector (b, _), Vector (c, _), Vector (d, _)) -> Vector ((Int32.lognot (Int32.logor a (Int32.logor b (Int32.logor c d)))), l)   (* lognot will negate all 32bits! *)
    |_ -> Bit Int32.max_int
    (* if a == LS_0 && b == LS_0 && c == LS_0 && d == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 || c == LS_1 || d == LS_1 then LS_0
    else
          LS_X *)

    let lNor5 = fun (a: signalValueT) (b: signalValueT) (c: signalValueT) (d: signalValueT)  (e: signalValueT) -> 
    match (a,b,c,d,e) with
    |(Bit a, Bit b, Bit c, Bit d, Bit e) -> Bit (Int32.logxor (Int32.of_int 0x1) (Int32.logor a (Int32.logor b (Int32.logor c (Int32.logor d e)))))
    | (Vector (a, l), Vector (b, _), Vector (c, _), Vector (d, _), Vector (e, _)) -> Vector ((Int32.lognot (Int32.logor a (Int32.logor b (Int32.logor c (Int32.logor d e))))), l)   (* lognot will negate all 32bits! *)
    |_ -> Bit Int32.max_int
    (* if a == LS_0 && b == LS_0 && c == LS_0 && d == LS_0  && e == LS_0 then LS_1
    else if a == LS_1 || b == LS_1 || c == LS_1 || d == LS_1 || e == LS_1 then LS_0
    else
          LS_X *)


    let lXor = fun (a: signalValueT) (b : signalValueT) -> 
    match (a,b) with
    |(Bit a, Bit b) -> Bit (Int32.logxor a b) 
    | (Vector (a, l), Vector (b, _)) -> Vector ((Int32.logxor a b), l)
    |_ -> Bit Int32.max_int
    (* if (a == LS_X || b == LS_X) then LS_X
    else if a != b then LS_1
    else
          LS_0 *)

    let lNot = fun (a: signalValueT) -> 
    match a with
    |Bit a -> Bit (Int32.logxor (Int32.of_int 0x1) a) 
    |Vector (a, l) -> Vector ((Int32.lognot a ), l)

        (* if a == LS_X then LS_X
        else
          if a == LS_1 then LS_0
          else LS_1 *)

    let lTnot = fun (oe: signalValueT) (a: signalValueT) -> 
    match (a,oe) with
    |(Bit a, Bit oe) -> Bit (if oe == Int32.one then (Int32.logxor (Int32.of_int 0x1) a)  else Int32.max_int) 
    |(Vector (a, l), Vector (oe, _)) -> Vector ((if oe == Int32.one then (Int32.lognot a) else Int32.max_int), l)   (* lognot will negate all 32bits! *)
    |_ -> Bit Int32.max_int
        (* if oe != LS_1 then LS_X
        else 
          if a == LS_X then LS_X
          else
            if a == LS_1 then LS_0
            else LS_1 *)

    let lTbuf = fun (oe: signalValueT) (a: signalValueT) -> 
    match (a,oe) with
    |(Bit a, Bit oe) -> Bit (if oe == Int32.one then a else Int32.max_int) 
    |(Vector (a, l), Vector (oe, _)) -> Vector ((if oe == Int32.one then a else Int32.max_int), l)
    |_ -> Bit Int32.max_int
        (* if oe != LS_1 then LS_X
        else 
          if a == LS_X then LS_X
          else a *)


    let l = [Bit Int32.zero]
    let h = [Bit Int32.one]

    (* Vector constructor with v:value l:length *)
    let vector v l = Vector (Int32.of_int v, l)

    (* compare two signals (Vector or Bit) values (vectors doesn't have to have the same length) *)
    let isEqual v1 v2 = 
            match v1 with
            |Bit v1' -> begin match v2 with
                  |Bit v2' -> (v1'=v2')
                  |Vector _ -> false
                  end
            |Vector (v1', l1) -> begin match v2 with
                  |Vector (v2', l2) ->
                              let mask1 = Int32.shift_right_logical (Int32.of_int 0xffffffff) (32 - l1) in 
                              let mask2 = Int32.shift_right_logical (Int32.of_int 0xffffffff) (32 - l2) in
                              ((Int32.logand mask1 v1') = (Int32.logand mask2 v2')) (* with masks vectors doesn't have to have the same length *)
                  |Bit _ -> false
                  end