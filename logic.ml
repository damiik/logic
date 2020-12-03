(* type and4 = { in0: int; in1: int ; in2: int; in3: int; output: int } *)

open Table;;

type lineStateT = LS_High | LS_Low | LS_Open 
type lineMapT = (string, lineStateT) ListMap.t

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
  "x"::"y"::"z"::[] |> insertLines tracks;
  showLines tracks;
  (* Printf.printf "%s" (lineStatetoStr (ListMap.find "z" !tracks)); *)
  Printf.printf " hello\n";
  (* !tracks |> List.map fst |> fun inp ->  *)






(* 
let and4 (in0: bool) (in1: bool) (in2: bool) (in3: bool) : bool = in0 && in1 && in2 && in3

let unit = fun (input_list: string list) :  ->  *)