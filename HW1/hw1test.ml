(* test cases for hw1.ml *)

(* subset tests *)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [1; 6; 6; 6; 7] [7; 6; 1; 1]
let my_subset_test2 = not (subset [3; 4; 5; 6] [5; 6; 7])

(* equal_sets tests *)
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [6; 6; 6] [1; 2; 3])
let my_equal_sets_test2 = equal_sets [4; 5; 5; 6; 6] [4; 4; 5; 6]

(* set_union tests *)
let my_set_union_test0 = equal_sets (set_union [1] [2]) [1; 2]
let my_set_union_test1 = equal_sets (set_union [1; 1; 2] [1;2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [4; 5; 6] [4; 5; 5; 6]) [4; 5; 6]

(* set_symdiff tests *)
let my_set_symdiff_test0 =
  equal_sets (set_symdiff [2] [1]) [1;2]
let my_set_symdiff_test1 =
  equal_sets (set_symdiff [6; 6; 6] [1; 6]) [1]
let my_set_symdiff_test2 =
  equal_sets (set_symdiff [3; 4] [4; 3]) []

(* computed_fixed_point tests *)
let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x) 25 = 25
let computed_fixed_point_test1 =
  computed_fixed_point equal_sets (fun x -> 1::x) [2; 3; 4] = [1; 2; 3; 4]
let computed_fixed_point_test2 = 
  computed_fixed_point equal_sets (fun x -> List.filter (fun y -> y > 2) x) [4; 1; 2; 6; 0; 7] = [4; 6; 7]


(* tests for filter_unreachable using animal_grammar *)
type animal_nonterminals = 
    | Animal | Pet| NotPet | Cat | Dog | Elephant | Zebra

let animal_grammar =
    Animal, 
    [Animal, [N Pet; T"Attacks"; N NotPet];
    Cat, [T "Meow"];
    Dog, [T "Bark"];
    Pet, [N Cat; N Dog];
    NotPet, [N Elephant];
    Elephant, [T"IDK"];
    Zebra, [T"yep"; N Dog]
    ]

let revised_animal_grammar = 
    Animal, 
    [Animal, [N Pet; T"Attacks"; N NotPet];
    Cat, [T "Meow"];
    Dog, [T "Bark"];
    Pet, [N Cat; N Dog];
    NotPet, [N Elephant];
    Elephant, [T"IDK"];
    ]

let my_animal_test0 = 
    filter_reachable animal_grammar = revised_animal_grammar

let my_animal_test1 = 
    filter_reachable (Zebra, (snd animal_grammar)) = (Zebra, [Dog, [T "Bark"]; Zebra, [T"yep"; N Dog]])