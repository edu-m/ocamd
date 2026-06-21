open Ocamd

let failures = ref 0

let check name got want =
  if got = want then Printf.printf "ok   - %s\n" name
  else (
    incr failures;
    Printf.printf "FAIL - %s\n  got:  %s\n  want: %s\n" name got want)

let math_ns = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"

let () =
  check "latex/sup+frac"
    (Mathml.render (Latex.parse "x^2 + \\frac{1}{2}"))
    (math_ns
   ^ "<mrow><msup><mi>x</mi><mn>2</mn></msup><mo>+</mo><mfrac><mn>1</mn><mn>2</mn></mfrac></mrow></math>"
    )

let () =
  check "latex/sub+symbol"
    (Mathml.render (Latex.parse "n_0 \\in \\mathbb{N}"))
    (math_ns
   ^ "<mrow><msub><mi>n</mi><mn>0</mn></msub><mo>\xe2\x88\x88</mo><mi>\xe2\x84\x95</mi></mrow></math>"
    )

let () =
  check "latex/mathrm"
    (Mathml.render (Latex.parse "\\mathrm{d}x"))
    (math_ns
   ^ "<mrow><mi mathvariant=\"normal\">d</mi><mi>x</mi></mrow></math>")

let () =
  check "latex/mathit"
    (Mathml.render (Latex.parse "\\mathit{Hom}"))
    (math_ns ^ "<mi mathvariant=\"italic\">Hom</mi></math>")

let () =
  check "latex/overline"
    (Mathml.render (Latex.parse "\\overline{\\alpha}"))
    (math_ns
   ^ "<mover accent=\"true\"><mi>\xce\xb1</mi><mo stretchy=\"true\">\xc2\xaf</mo></mover></math>")

let () =
  check "latex/hat"
    (Mathml.render (Latex.parse "\\hat{x}"))
    (math_ns
   ^ "<mover accent=\"true\"><mi>x</mi><mo stretchy=\"false\">^</mo></mover></math>")

let () =
  check "latex/mathrel-nested-bold"
    (Mathml.render (Latex.parse "p \\mathrel{\\mathbf{R}} q"))
    (math_ns
   ^ "<mrow><mi>p</mi><mrow><mspace width=\"0.278em\"/><mi mathvariant=\"bold\">R</mi><mspace width=\"0.278em\"/></mrow><mi>q</mi></mrow></math>")

let () =
  check "latex/cases"
    (Mathml.render (Latex.parse "\\begin{cases} a \\\\ b \\end{cases}"))
    (math_ns
   ^ "<mrow><mo>{</mo><mtable columnalign=\"left\"><mtr><mtd><mi>a</mi></mtd></mtr><mtr><mtd><mi>b</mi></mtd></mtr></mtable></mrow></math>")

let () =
  check "latex/stackrel+mathcal"
    (Mathml.render (Latex.parse "\\mathcal{G} \\stackrel{?}{=} H"))
    (math_ns
   ^ "<mrow><mi mathvariant=\"script\">G</mi><mover><mo>=</mo><mo>?</mo></mover><mi>H</mi></mrow></math>")

let () =
  check "latex/prime-after-subscript"
    (Mathml.render (Latex.parse "e_0'"))
    (math_ns ^ "<msup><msub><mi>e</mi><mn>0</mn></msub><mo>\xe2\x80\xb2</mo></msup></math>")

let () =
  check "latex/inferrule"
    (Mathml.render (Latex.parse "\\inferrule{A \\quad B}{C}"))
    (math_ns
   ^ "<mfrac linethickness=\"medium\"><mrow><mi>A</mi><mspace width=\"1em\"/><mi>B</mi></mrow><mi>C</mi></mfrac></math>")

let () =
  check "latex/xrightarrow"
    (Mathml.render (Latex.parse "p \\xrightarrow{\\mu} q"))
    (math_ns
   ^ "<mrow><mi>p</mi><mover><mo stretchy=\"true\">\xe2\x86\x92</mo><mi>\xce\xbc</mi></mover><mi>q</mi></mrow></math>")

let () =
  check "latex/bigsqcup-limits"
    (Mathml.render (Latex.parse "\\bigsqcup_{n} \\Phi"))
    (math_ns
   ^ "<mrow><msub><mo largeop=\"true\" movablelimits=\"true\">\xe2\xa8\x86</mo><mi>n</mi></msub><mi>\xce\xa6</mi></mrow></math>")

let () =
  let doc = Transform.run [ Ast.Paragraph (Inline.parse "a $x^2$ b") ] in
  check "inline/math-in-paragraph" (Render.document doc)
    ("<p>a " ^ math_ns ^ "<msup><mi>x</mi><mn>2</mn></msup></math> b</p>\n")

let () =
  let doc = Transform.run [ Ast.Paragraph (Inline.parse "a*b **c** \\& <x>") ] in
  check "inline/emphasis+escape" (Render.document doc)
    "<p>a*b <strong>c</strong> &amp; &lt;x&gt;</p>\n"

let () =
  check "latex/malformed-safe"
    (Mathml.render (Latex.parse "\\frac{1"))
    (math_ns ^ "<mrow><mo>\\frac{1</mo></mrow></math>")

let () =
  if !failures = 0 then print_endline "All tests passed."
  else (
    Printf.printf "%d test(s) failed.\n" !failures;
    exit 1)
