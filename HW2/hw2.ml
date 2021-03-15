type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* function that takes in a homework 1 style grammar and returns a homework 2 style grammar *)

let rec right_hand_sides rules = 
    match rules with 
    | [] -> []
    | _ -> (snd (List.hd rules))::(right_hand_sides (List.tl rules))            (* return RHS of first rule then recursively call with rest of rules *)

(* production function *)
let production_func rules nonterm = 
    let filtered_rules = List.filter (fun rule -> (fst rule) = nonterm) rules in     (* filter out all rules with nonterm as their LHS *)
    right_hand_sides filtered_rules                                                   (* return a list of all RHS of the above rules *)

(* main function *)
let convert_grammar gram1 = 
    match gram1 with
    | (start, rules) -> (start, production_func rules)


(* function that traverses the parse tree from left to right and yields a list of leaves encountered*)

let rec parse_tree_leaves tree =       
  match tree with
          | Leaf terminal -> [terminal]  (* If current node is leaf, return the terminal value of that node *)
          | Node (nonterminal, []) -> []    (* otherwise, call parse_tree_leaves on all children of that node *)
          | Node (nonterminal, lst) -> (parse_tree_leaves (List.hd lst)) @ (parse_tree_leaves (Node (nonterminal, (List.tl lst))))


(* functions for make_matcher *)

let match_empty accept frag = accept frag

let match_nothing accept frag = None

let rec or_matcher rules prodFunc accept frag =  
  match rules with
  | [] -> match_nothing accept frag             (* All possible rules exhausted with no matches, return None *)
  | (firstRule::restRules) -> 
    let result = and_matcher prodFunc firstRule accept frag in      (* Start with first rule and try to generate a match *)
    match result with
    | None -> or_matcher restRules prodFunc accept frag             (* If no acceptable match found, start over with rest of rules *)
    | Some x -> Some x                                        (* Match found! Return what acceptor returned *)

and and_matcher prodFunc rule accept frag = 
  match rule with 
  | [] -> match_empty accept frag         (* If rule is used up, match_empty and return accept of suffix *)
  | (symbol::rule_tail) ->
    match symbol with 
    | (N nonterminal) ->            (* If head of rule is a nonterminal, append an and_matcher to an or_matcher*)
        let new_acceptor = (fun fragment -> and_matcher prodFunc rule_tail accept fragment) in
        let new_rules = prodFunc nonterminal in
        or_matcher new_rules prodFunc new_acceptor frag           (* Use an or_matcher for nonterminal symbol and pass resulting suffix to and_matcher of remaining symbols *)
    | (T terminal) ->       
        match frag with 
        | [] -> None                      (* All of frag is used up but rule is not, backtrack *)
        | (frag_head::frag_tail) ->
            if (terminal = frag_head) then and_matcher prodFunc rule_tail accept frag_tail        (* if terminal symbol is equal to head of frag, continue matching rest of rule *)
            else None                                                                        (* backtrack *)

(* Returns a matcher for the grammar gram *)
let make_matcher gram = 
  match gram with 
  | (start, prodFunc) -> let rules = prodFunc start in 
    (fun accept frag -> or_matcher rules prodFunc accept frag)    (* Return the matcher for the grammar *)


(* functions for make_parser *)

(* acceptor used for make_parser *)
let accept_parser frag tree = 
  match frag with
  | [] -> Some tree
  | _ -> None

let rec or_parser rules nonterm prodFunc accept frag = 
  match rules with
  | [] -> None             (* All possible rules exhausted with no parse trees, return None *)
  | (firstRule::restRules) -> 
    let result = and_parser nonterm prodFunc firstRule accept frag [] in      (* Start with first rule and try to generate a complete parse tree *)
    match result with
    | None -> or_parser restRules nonterm prodFunc accept frag             (* If no acceptable parse tree found, start over with rest of rules *)
    | Some x -> Some x

and and_parser nonterm prodFunc rule accept frag tree = 
  match rule with 
  | [] -> accept frag (Node(nonterm, tree))      (* If rule is used up call acceptor on fragment with built tree*)
  | (symbol::rule_tail) ->
    match symbol with 
    | (N nonterminal) ->            (* If head of rule is a nonterminal, append an and_parser to an or_parser *)
        let new_acceptor = (fun fragment p_tree -> and_parser nonterm prodFunc rule_tail accept fragment (tree @ [p_tree])) in     (* Append resulting tree of or_parser to and_parser tree *)
        let new_rules = prodFunc nonterminal in
        or_parser new_rules nonterminal prodFunc new_acceptor frag     (* Use an or_parser for nonterminal symbol and pass resulting suffix and tree to and_parser of remaining symbols *)
    | (T terminal) ->       
        match frag with 
        | [] -> None                      (* All of frag is used up but rule is not, backtrack *)
        | (frag_head::frag_tail) ->
            if (terminal = frag_head) then 
              let new_tree = tree @ [Leaf terminal] in                            (* Add terminal leaf node to tree *)
              and_parser nonterm prodFunc rule_tail accept frag_tail new_tree       (* if terminal symbol is equal to head of frag, continue matching rest of rule *)
            else None

let make_parser gram = 
  match gram with
  | (start, prodFunc) -> let rules = prodFunc start in
      (fun frag -> or_parser rules start prodFunc accept_parser frag)       (* Return the parser *)