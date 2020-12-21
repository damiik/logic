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