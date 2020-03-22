module Int = struct
    let compare = compare
    type t = int
end

module IntSet = Set.Make(Int)

module IntMap = Map.Make(Int)

(*
 * type `int' was chosen for `Var' and `Node' because:
 *
 * 1. variables and symbols can be from denumerable sets,
 *    which are best represented by natural numbers.
 *
 * 2. implementations can be made considerably efficient:
 *    2.1. instead of using a memory-consuming data structure
 *    for arity, a function can simply check integer ranges.
 *    2.2. the check_sig function is trivial.
 *
 * 3. it offers great flexibility.
 *
 * note: integers used are expected to be non-negative.
 *)
type term = Var of int | Node of int * (term list)

(*
 * a signature is represented by its arity function.
 * this function should return negative values for variables
 * which don't exist in the signature.
 *)
type signature = int -> int

(*
 * checks if the signature is valid.
 *
 * since there is no redundancy in my representation of
 * signatures, check_sig will always return true.
 * repeating symbols is not possible, and negative arities
 * and interpreted as the non-existence of symbol.
 *)
let check_sig: signature -> bool =
    fun _ -> true

(*
 * checks if term is well-formed according to the signature
 *)
let rec wfterm: signature -> term -> bool =
    fun ar -> function
    | Var _ -> true
    | Node (s, l) ->
        (ar s = List.length l) && (List.for_all (wfterm ar) l)

(*
 * computes height of the term
 * single node terms have height 0
 *)
let rec ht: term -> int =
    function
    | Var _ -> 0
    | Node (_, l) ->
        List.map ht l |> List.fold_left max (-1) |> ( + ) 1

(*
 * computes size of the term
 * variables and nodes add 1 to the size
 *)
let rec size: term -> int =
    function
    | Var _ -> 1
    | Node (_, l) ->
        List.map size l |> List.fold_left ( + ) 0 |> ( + ) 1

(*
 * returns the set of variables (as a list) in the term
 *)
let vars: term -> int list =
    fun t ->
    let rec aux = function
    | Var v -> IntSet.singleton v
    | Node (_, l) ->
        List.map aux l
        |> List.fold_left IntSet.union IntSet.empty 
    in IntSet.elements (aux t)

(*
 * a substitution is represented as a map from `int' to `term'
 * a composition of substitutions is also of type `substitution'
 *)
type substitution = term IntMap.t

(*
 * applies a substitution to a term and returns the result term
 *)
let rec subst: substitution -> term -> term =
    fun s -> function
    | Var v -> (
        match IntMap.find_opt v s with 
        | None -> Var v
        | Some t -> t
    )
    | Node (x, l) -> Node (x, List.map (subst s) l)

(*
 * compose s1 s2 = s,
 * means that substituion s is equivalent to first applying
 * substitution s1, and then applying substitution s2
 * 
 * logic: first apply s2 to each term of s1,
 * then add (key, value) pairs from s2 whose keys aren't in s1
 * note: x -> x, is allowed to remain because removal is expected
 * to be more expensive than the performance gain otherwise
 *)
let compose: substitution -> substitution -> substitution =
    fun s1 s2 ->
        IntMap.map (subst s2) s1
        |> IntMap.union (fun k x y -> Some y) s2

(*
 * performs the occurs check for a variable in a term
 *)
let rec occurs: int -> term -> bool =
    fun v -> function
    | Var x when x = v -> true
    | Var _ -> false
    | Node (_, l) -> List.exists (occurs v) l

exception Not_unifiable
exception Not_well_formed

(*
 * returns the most general unifier of two terms
 * uses robinson's algorithm
 * substitution is over finite terms, i.e. occurs check is used
 *)
let rec mgu: term -> term -> substitution =
    fun t1 t2 -> match t1, t2 with
    | Var x, Var y when x = y -> IntMap.empty

    | Var x, t when (occurs x t) -> raise Not_unifiable
    | t, Var x when (occurs x t) -> raise Not_unifiable

    | Var x, t
    | t, Var x -> IntMap.singleton x t

    | Node (f, _), Node (g, _) when f <> g -> raise Not_unifiable

    | Node (_, xl), Node (_, yl) ->
        let rec aux acc xl yl = match xl, yl with
        | [], [] -> acc
        | [], _
        | _, [] -> raise Not_well_formed
        | x::xs, y::ys ->
            let m = mgu (subst acc x) (subst acc y) in
            aux (compose acc m) xs ys
        in aux IntMap.empty xl yl

(* testing *)

(*
 * get_left and get_right return terms which according to
 * Wikipedia give worst-case for naive Robinson's algo.
 * the mgu for these have exponential size, however
 * since most of the terms are simply pointers to others
 * in ocaml's handling, this does not imply exponential
 * time complexity.
 *)

let rec get_left n =
    if n = 0 then Node (0, [])
    else Node (1, [get_left (n-1); Var n])

let rec get_right n =
    if n = 0 then Node (0, [])
    else Node (1, [get_right (n-1); Var n])

let time f x y =
    let t = Sys.time() in
    let fxy = f x y in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxy

(*
 * empirical testing suggests that mgu for these terms
 * runs in O(n^2) time (ignoring possible log terms)
 *)
let test n =
    let l = get_left n in
    let r = get_right n in
    let m = time mgu l r in
    let lm = subst m l in
    let rm = subst m r in
    Printf.printf "Correctness: %s\n"
        (if lm = rm then "true" else "false")

