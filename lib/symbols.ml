type kind = Mi | Mo | Mn | LargeOp

let greek =
  [
    ("alpha", "\xce\xb1");
    ("beta", "\xce\xb2");
    ("gamma", "\xce\xb3");
    ("delta", "\xce\xb4");
    ("epsilon", "\xcf\xb5");
    ("varepsilon", "\xce\xb5");
    ("zeta", "\xce\xb6");
    ("eta", "\xce\xb7");
    ("theta", "\xce\xb8");
    ("vartheta", "\xcf\x91");
    ("iota", "\xce\xb9");
    ("kappa", "\xce\xba");
    ("lambda", "\xce\xbb");
    ("mu", "\xce\xbc");
    ("nu", "\xce\xbd");
    ("xi", "\xce\xbe");
    ("pi", "\xcf\x80");
    ("varpi", "\xcf\x96");
    ("rho", "\xcf\x81");
    ("varrho", "\xcf\xb1");
    ("sigma", "\xcf\x83");
    ("varsigma", "\xcf\x82");
    ("tau", "\xcf\x84");
    ("upsilon", "\xcf\x85");
    ("phi", "\xcf\x95");
    ("varphi", "\xcf\x86");
    ("chi", "\xcf\x87");
    ("psi", "\xcf\x88");
    ("omega", "\xcf\x89");
    ("Gamma", "\xce\x93");
    ("Delta", "\xce\x94");
    ("Theta", "\xce\x98");
    ("Lambda", "\xce\x9b");
    ("Xi", "\xce\x9e");
    ("Pi", "\xce\xa0");
    ("Sigma", "\xce\xa3");
    ("Upsilon", "\xce\xa5");
    ("Phi", "\xce\xa6");
    ("Psi", "\xce\xa8");
    ("Omega", "\xce\xa9");
  ]

let misc = [
  ("ell", "\xe2\x84\x93");
  ("questeq", "\xe2\x89\x9f");
  ("wp", "\xe2\x84\x98");
  ("bot", "\xe2\x8a\xa5");
  ("top", "\xe2\x8a\xa4");
  ("blacksquare", "\xe2\x96\xa0");
]

let operators =
  [
    ("leq", "\xe2\x89\xa4");
    ("le", "\xe2\x89\xa4");
    ("geq", "\xe2\x89\xa5");
    ("ge", "\xe2\x89\xa5");
    ("neq", "\xe2\x89\xa0");
    ("ne", "\xe2\x89\xa0");
    ("prec", "\xe2\x89\xba");
    ("preceq", "\xe2\xaa\xaf");
    ("succeq", "\xe2\xaa\xb0");
    ("succ", "\xe2\x89\xbb");
    ("equiv", "\xe2\x89\xa1");
    ("approx", "\xe2\x89\x88");
    ("sim", "\xe2\x88\xbc");
    ("simeq", "\xe2\x89\x83");
    ("cong", "\xe2\x89\x85");
    ("triangleq", "\xe2\x89\x9c");
    ("vdash", "\xe2\x8a\xa2");
    ("models", "\xe2\x8a\xa8");
    ("in", "\xe2\x88\x88");
    ("notin", "\xe2\x88\x89");
    ("subset", "\xe2\x8a\x82");
    ("subseteq", "\xe2\x8a\x86");
    ("supset", "\xe2\x8a\x83");
    ("supseteq", "\xe2\x8a\x87");
    ("sqsubseteq", "\xe2\x8a\x91");
    ("sqsubset", "\xe2\x8a\x8f");
    ("cup", "\xe2\x88\xaa");
    ("cap", "\xe2\x88\xa9");
    ("uplus", "\xe2\x8a\x8e");
    ("emptyset", "\xe2\x88\x85");
    ("forall", "\xe2\x88\x80");
    ("exists", "\xe2\x88\x83");
    ("nexists", "\xe2\x88\x84");
    ("neg", "\xc2\xac");
    ("lnot", "\xc2\xac");
    ("wedge", "\xe2\x88\xa7");
    ("land", "\xe2\x88\xa7");
    ("vee", "\xe2\x88\xa8");
    ("lor", "\xe2\x88\xa8");
    ("rightarrow", "\xe2\x86\x92");
    ("to", "\xe2\x86\x92");
    ("nRightarrow", "\xe2\x86\x9b");
    ("nrightarrow", "\xe2\x86\x9b");
    ("leftarrow", "\xe2\x86\x90");
    ("gets", "\xe2\x86\x90");
    ("nLeftarrow", "\xe2\x86\x9a");
    ("nleftarrow", "\xe2\x86\x9a");
    ("leftrightarrow", "\xe2\x86\x94");
    ("mapsto", "\xe2\x86\xa6");
    ("Rightarrow", "\xe2\x87\x92");
    ("implies", "\xe2\x87\x92");
    ("Leftarrow", "\xe2\x87\x90");
    ("Leftrightarrow", "\xe2\x87\x94");
    ("rightharpoonup", "\xe2\x87\x80");
    ("downarrow", "\xe2\x86\x93");
    ("uparrow", "\xe2\x86\x91");
    ("Downarrow", "\xe2\x87\x93");
    ("Uparrow", "\xe2\x87\x91");
    ("iff", "\xe2\x87\x94");
    ("times", "\xc3\x97");
    ("div", "\xc3\xb7");
    ("cdot", "\xe2\x8b\x85");
    ("pm", "\xc2\xb1");
    ("mp", "\xe2\x88\x93");
    ("circ", "\xe2\x88\x98");
    ("infty", "\xe2\x88\x9e");
    ("partial", "\xe2\x88\x82");
    ("nabla", "\xe2\x88\x87");
    ("int", "\xe2\x88\xab");
    ("colon", ":");
    ("mid", "\xe2\x88\xa3");
    ("ldots", "\xe2\x80\xa6");
    ("dots", "\xe2\x80\xa6");
    ("cdots", "\xe2\x8b\xaf");
    ("langle", "\xe2\x9f\xa8");
    ("rangle", "\xe2\x9f\xa9");
    ("lfloor", "\xe2\x8c\x8a");
    ("rfloor", "\xe2\x8c\x8b");
    ("lceil", "\xe2\x8c\x88");
    ("rceil", "\xe2\x8c\x89");
    ("setminus", "\xe2\x88\x96");
    ("frown", "\xe2\x8c\xa2");
    ("{", "{");
    ("}", "}");
    ("|", "\xe2\x80\x96");
    ("backslash", "\\");
  ]

let large_operators =
  [
    ("sum", "\xe2\x88\x91");
    ("prod", "\xe2\x88\x8f");
    ("coprod", "\xe2\x88\x90");
    ("bigsqcup", "\xe2\xa8\x86");
    ("bigcup", "\xe2\x8b\x83");
    ("bigcap", "\xe2\x8b\x82");
    ("bigwedge", "\xe2\x8b\x80");
    ("bigvee", "\xe2\x8b\x81");
  ]

let functions =
  [
    "sin"; "cos"; "tan"; "cot"; "sec"; "csc"; "log"; "ln"; "exp"; "lim"; "max";
    "min"; "sup"; "inf"; "det"; "dim"; "ker"; "deg"; "gcd"; "arg";
  ]

let blackboard = function
  | "N" -> Some "\xe2\x84\x95"
  | "Z" -> Some "\xe2\x84\xa4"
  | "Q" -> Some "\xe2\x84\x9a"
  | "R" -> Some "\xe2\x84\x9d"
  | "C" -> Some "\xe2\x84\x82"
  | "H" -> Some "\xe2\x84\x8d"
  | "P" -> Some "\xe2\x84\x99"
  | _ -> None

let table : (string, kind * string) Hashtbl.t = Hashtbl.create 256

let () =
  List.iter (fun (n, s) -> Hashtbl.replace table n (Mi, s)) greek;
  List.iter (fun (n, s) -> Hashtbl.replace table n (Mo, s)) operators;
  List.iter (fun (n, s) -> Hashtbl.replace table n (LargeOp, s)) large_operators;
  List.iter (fun n -> Hashtbl.replace table n (Mi, n)) functions;
  List.iter (fun (n, s) -> Hashtbl.replace table n (Mi, s)) misc

let lookup name : kind * string =
  match Hashtbl.find_opt table name with Some r -> r | None -> (Mi, name)
