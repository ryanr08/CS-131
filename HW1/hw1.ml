(* function that returns true iff a is a subset of b *)
let subset a b = 
    List.for_all (fun x -> List.exists (fun y -> x = y) b) a

(* function that returns true iff the represented sets are equal  *)
let equal_sets a b = 
    subset a b
    &&
    subset b a

(* function that returns a list representing the union of the two sets *)
let rec set_union a b = 
    match a with
    | [] -> b
    | _ -> 
            if (List.mem (List.hd a) b) then set_union (List.tl a) b
            else set_union (List.tl a) ((List.hd a)::b)

(* function that returns a list representing the symmetric difference of the two sets *)
let set_symdiff a b = 
    List.filter (fun x -> not ((List.exists (fun y -> x = y) b) && (List.exists (fun z -> x = z) a))) (set_union a b) 

(* Russel's Paradox is not possible to program into ocaml due to type issues.
If a set A had type int and is a list of ints, then we cannot check if A contains A since
a list of type int cannot hold lists of ints. This problem arises due to the nature of lists
in ocaml and that they are static typed*)
let self_member s = 
    false

(* function that returns the computed fixed point for f with respect to x *)
let rec computed_fixed_point eq f x = 
    if (eq (f x) x) then x
    else computed_fixed_point eq f (f x)

(* function that returns a copy of the grammar g except with all the unreachable rules filtered out.
preserves the order of the rules when returning g.*)

type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

(* function to see if an element in a list is a nonterminal *)
let is_nonterminal x = 
    match x with 
    | N nonterminal -> true
    | T terminal -> false

let get_symbol x = 
    match x with
    | N nonterminal -> [nonterminal]
    | _ -> failwith "ERROR"

let rec get_symbols lst output = 
    match lst with
    | [] -> output
    | _ -> get_symbols (List.tl lst) (set_union (get_symbol (List.hd lst)) output)

(* function that returns all newly reached symbols from RHS of a rule *)
let new_symbols curr_rule reachable = 
    let curr_symbol = fst curr_rule in 
    if subset [curr_symbol] reachable then List.filter is_nonterminal (snd curr_rule)
    else []

(* function to recursively go through all rules in a grammar and add any new found reachables *)
let rec find_reachables rules reachables = 
    match rules with
    | [] -> reachables
    | _ -> 
            let curr_rule = List.hd rules in
            let rest_rules = List.tl rules in
            let new_reachables = new_symbols curr_rule reachables in
            let new_reachables = get_symbols new_reachables [] in
            find_reachables rest_rules (set_union reachables new_reachables)

(* obtain all reachable symbols by calling find_reachables until a fixed point of no new reachables is reached *)
let reachable_symbols rules reachable = 
    computed_fixed_point equal_sets (fun x -> (find_reachables rules x)) reachable

(* function to filter out all unreachable rules from a grammar's rules *)
let filter_rules rules reachables = 
    List.filter (fun rule -> let left_sym = (fst rule) in List.mem left_sym reachables) rules

(* main function to filter a grammar *)
let filter_reachable g = 
    match g with
    | (a, b) -> let reachable = reachable_symbols b [a] in 
                let filtered = filter_rules b reachable in
                (a, filtered)