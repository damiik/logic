

(* ------------------------------------------------ List Map -------------------------------------------------------- *)


module type Map = sig

  (** [('k, 'v) t] is the type of maps that bind keys of type
      ['k] to values of type ['v]. *)
  type ('k, 'v) t

  (** [insert k v m] is the same map as [m], but with an additional
      binding from [k] to [v].  If [k] was already bound in [m],
      that binding is replaced by the binding to [v] in the new map. *)
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

  (** [find k m] is [Some v] if [k] is bound to [v] in [m],
      and [None] if not. *)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [remove k m] is the same map as [m], but without any binding of [k].
      If [k] was not bound in [m], then the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

  (** [empty] is the empty map. *)
  val empty : ('k, 'v) t

  (** [of_list lst] is a map containing the same bindings as
      association list [lst]. 
      Requires: [lst] does not contain any duplicate keys. *)
  val of_list : ('k * 'v) list -> ('k, 'v) t

  (** [bindings m] is an association list containing the same
      bindings as [m]. There are no duplicates in the list. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module ListMap : Map = struct

  (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map 
      {k1 : v1, k2 : v2, ..., kn : vn}.
      If a key appears more than once in the list, then in the map it is
      bound to the left-most occurrence in the list. For example,
      [[(k, v1); (k, v2)]] represents {k : v1}. The empty list represents
      the empty map.
      RI: none. *)
  type ('k,'v) t = ('k * 'v) list

  (** Efficiency: O(1). *)
  let insert k v m = 
    (k, v) :: m

  (** Efficiency: O(n). *)
  let find = List.assoc_opt

  (** Efficiency: O(n). *)
  let remove k lst = 
    List.filter (fun (k', _) -> k <> k') lst

  let empty = []

  (** Efficiency: O(1). *)  
  let of_list lst = 
    lst

  (** [keys m] is a list of the keys in [m], without
      any duplicates. 
      Efficiency: O(n log n). *)
  let keys m =
    m |> List.map fst |> List.sort_uniq Stdlib.compare

  (** [binding m k] is [(k, v)], where [v] is the value that [k]
       binds in [m].
       Requires: [k] is a key in [m]. 
       Efficiency: O(n). *)
  let binding m k = 
    (k, List.assoc k m)

  (** Efficiency: O(n log n) + O(n) * O(n), which is O(n^2). *)
  let bindings m =
    List.map (binding m) (keys m)

end

(*
type lineState = LS_High | LS_Low | LS_Open 
type lineMap = (string, lineState) ListMap.t

(* let printLines l : lineMap =  *)

let () = 
  let tracks : lineMap ref = ref ListMap.empty in
  tracks := ListMap.insert "hello" LS_Open !tracks;
  tracks := ListMap.insert "world" LS_Open !tracks;
  tracks := ListMap.insert "my" LS_Open !tracks;
  tracks := ListMap.insert "friend" LS_Open !tracks;
  Printf.printf "%s" (match (ListMap.find "my" !tracks) with |Some(LS_High) -> "high" |Some(LS_Low) -> "low"|Some(LS_Open) -> "open"| _ -> "?");;
  Printf.printf " hello\n"
*)


(* ------------------------------------------------ Array Map -------------------------------------------------------- *)
module type DirectAddressMap = sig

  (** [t] is the type of maps that bind keys of type int
      to values of type ['v]. *)
  type 'v t

  (** [insert k v m] mutates map [m] to bind [k] to [v].
      If [k] was already bound in [m], that binding is
      replaced by the binding to [v] in the new map.
      Requires: [k] is in bounds for [m]. *)
  val insert : int -> 'v -> 'v t -> unit

  (** [find k m] is [Some v] if [k] is bound to [v] in [m],
      and [None] if not.
      Requires: [k] is in bounds for [m]. *)
  val find : int -> 'v t -> 'v option

  (** [remove k m] mutates [m] to remove any binding of [k].
      If [k] was not bound in [m], then the map is unchanged.
      Requires: [k] is in bounds for [m]. *)
  val remove : int -> 'v t -> unit

  (** [create c] creates a map with capacity [c]. Keys [0]
      through [c-1] are _in bounds_ for the map. *)
  val create : int -> 'v t

  (** [of_list c lst] is a map containing the same bindings
      as association list [lst] and with capacity [c].
      Requires: [lst] does not contain any duplicate keys, 
      and every key in [lst] is in bounds for capacity [c]. *)
  val of_list : int -> (int * 'v) list -> 'v t

  (** [bindings m] is an association list containing the same
      bindings as [m]. There are no duplicate keys in the list. *)
  val bindings : 'v t -> (int * 'v) list

end

module ArrayMap : DirectAddressMap = struct

  (** AF: [[|Some v0; Some v1; ... |]] represents
          {0 : v0, 1 : v1, ...}.  If element [i] of
          the array is instead [None], then [i] is not
          bound in the map.
      RI: None.
  *)
  type 'v t = 'v option array

  (** Efficiency: O(1) *)
  let insert k v a =
    a.(k) <- Some v

  (** Efficiency: O(1) *)
  let find k a =
    a.(k)

  (** Efficiency: O(1) *)
  let remove k a =
    a.(k) <- None

  (** Efficiency: O(c) *)
  let create c =
    Array.make c None

  (** Efficiency: O(c) *)
  let of_list c lst =
    let a = create c in (* O(c) *)
    List.iter (fun (k, v) -> insert k v a) lst;
    (* O(c) * O(1) = O(c) *)
    a

  (** Efficiency: O(c)  returns a list of (k, v) pairs *)
  let bindings a =
    let bs = ref [] in
    let add_binding k v = (* O(1) *)
      match v with
      | None -> ()
      | Some v -> bs := (k, v) :: !bs
    in
    Array.iteri add_binding a; (* O(c) *)
    !bs
end

(*
  let tracks : string ArrayMap.t = ArrayMap.create 10000 in

  
  ArrayMap.insert 0 "hello" tracks;
  ArrayMap.insert 1 "world" tracks;
  ArrayMap.insert 2 "my" tracks;
  ArrayMap.insert 3 "friend" tracks;

  Printf.printf "%s" (match (ArrayMap.find 0 tracks) with |Some x -> x |None -> "?");;

*)
