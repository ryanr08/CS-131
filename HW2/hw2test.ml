let accept_all string = Some string

type english_nonterminals =
  | Sentence | NP | VP | Verb | Noun | Adjective

let  english_grammar =
  (Sentence,
   function
     | Sentence ->
         [[N NP; N VP]]
     | NP ->
	 [[N Noun];
	  [N Adjective];
	  [N Adjective; N Noun]]
     | VP ->
	 [[N Verb]; 
	 [N Verb; N NP]
	 ]
     | Noun ->
	 [[T"Car"];
	  [T"Ryan"];
	  [T"Jesus"];
	  [T"Airplanes"];
	  [T"Cat"]
	  ]
     | Verb ->
	 [[T"is"];
	 [T"like"];
	 [T"want"];
	 [T"run"];
	 [T"fly"]]
     | Adjective ->
	 [[T"smart"]; [T"dumb"]; [T"funny"]; [T"fast"]; [T"slow"];
	  [T"light"]; [T"heavy"]])

let make_matcher_test = ((make_matcher english_grammar accept_all ["This";"fragment";"should";"not";"work"]) = None)

let make_parser_test =
   match make_parser english_grammar ["Ryan";"is";"smart"] with
      | Some tree -> parse_tree_leaves tree = ["Ryan";"is";"smart"]
      | _ -> false
