module IntSet = Set.Make(
    struct
        let compare = compare
        type t = int
    end
)

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
        1 + (List.fold_left max 0 (List.map ht l))

(*
 * computes size of the term
 * variables and nodes add 1 to the size
 *)
let rec size: signature -> term -> int =
    fun ar -> function
    | Var _ -> 1
    | Node (_, l) ->
        1 + (List.fold_left ( + ) 0 (List.map (size ar) l))

(*
 * returns the set of variables (as a list) in the term
 *)
let vars: term -> int list =
    fun t ->
    let rec aux = function
    | Var v -> IntSet.singleton v
    | Node (_, l) ->
        List.fold_left IntSet.union IntSet.empty (List.map aux l)
    in IntSet.elements (aux t)

