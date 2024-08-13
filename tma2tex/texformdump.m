(* Wolfram Language Package *)

BeginPackage["Texformdump`"]
(* Exported symbols added here with SymbolName::usage *)  

Texformdump`BoxesToTeX::usage = "Converts boxes to TeX.";
Texformdump`ExpressionToTeX::usage = "Converts an expression to TeX.";

Texformdump`MakeTeX::usage = "Underlying function to transform boxes or expressions to TeX, called by Texformdump`BoxesToTeX and Texformdump`ExpressionToTeX"; (* overwrite with $TM specifics? *)

Texformdump`$customTeXCommands::usage = "Association of custom commands without prefixed backlash, goes to number of arguments the individual macro takes."

(*Texformdump`$customTeXCommands = {}*)

Texformdump`CheckTeX[str_String] := 
 (* MakeTeX with some input string cleaning wrapped around *)
 Module[{cleaned}, 
  cleaned = 
   ToLowerCase @ StringTrim[StringReplace[str, {"\\text{" -> "\\", "{" -> "", "}" -> ""}], "TM"]; 
   (* default TeXForm renders unknown commands as \\text{...} so we bring that to canonical *)
  If[MemberQ[First /@ Texformdump`$customTeXCommands, cleaned], (* Transformation rules operate on LaTeX *)
   If[Or[StringStartsQ[str, "\\text{"], StringEndsQ[str, "{"]], MakeTeX[cleaned] <> "{", 
    MakeTeX[cleaned]], str]] (* take care of opening brackets if there to begin with *)

Begin["`Private`"] (* Begin Private Context *) 

DebugPrint[x___]:= If[ System`Convert`CommonDump`$htmldebugprint, Print[x] ];

(* Imported symbols from ConvertCommon.m *)
GetStyleSheet = System`Convert`CommonDump`GetStyleSheet;
EliminateRepetition = System`Convert`CommonDump`EliminateRepetition;
RemoveLinearSyntax = System`Convert`CommonDump`RemoveLinearSyntax;
Recursive = System`Convert`CommonDump`Recursive;
EmbeddedStringWithLinearSyntaxQ = System`Convert`CommonDump`EmbeddedStringWithLinearSyntaxQ;

(* ClearAll symbols so this package can be reloaded (for debugging through
    Workbench or a manual Get) *)
ClearAll[Texformdump`BoxesToTeX, Texformdump`ExpressionToTeX]

DebugPrint[x___] := If[System`Convert`CommonDump`$htmldebugprint, Print[x]];

(* --- BoxesToTeX --- *)
Options[Texformdump`BoxesToTeX] = {"BoxRules" -> {}}

Texformdump`BoxesToTeX[boxes_?BoxQ, opts___?OptionQ] := 
 Block[{$RecursionLimit = 4096, boxrules, MakeTeX, new},
  boxrules = "BoxRules" /. {opts} /. Options[Texformdump`BoxesToTeX];
  MakeTeXRule /@ boxrules;
  MakeTeX[arg_] := maketex[arg];
  new = StripIdentityBoxes[boxes];
  (* fixes bug 198055 -- MakeBox[] of Piecewise returns differently *)
  If[MatchQ[new, GridBox[{{"\[Piecewise]", ___}}, ___]],
   new = RowBox[new[[1, 1]]]];
  (* end fix to bug 198055 *)
  Print[new];
  new = MakeTeX[new];
  If[StringQ@new, StringTrim[new], new]
]

StripIdentityBoxes[boxes_] := 
 boxes //. {RowBox[{x_}] :> x, 
   InterpretationBox[TemplateBox[_, "NumericalApproximation", ___], x_, ___] :> 
    Block[{BoxForm`UseApproximations = False}, MakeBoxes[x, TraditionalForm]], 
   box : InterpretationBox[DynamicModuleBox[_, TemplateBox[_, "IconizedObject", ___], ___], __] :> 
    makeTeXIconizedObject[box], 
   (AdjustmentBox | InterpretationBox | TagBox | DynamicBox | TooltipBox)[x_, ___] :> x
  }

makeTeXIconizedObject[
  InterpretationBox[DynamicModuleBox[_, TemplateBox[args_, "IconizedObject", ___], ___], 
   interp_, ___]] := 
 Replace[args, {{_, StyleBox[custom_, "IconizedCustomName", ___], ___} :> 
    custom, {_, "ListIcon", ___} :> RowBox[{"{", "\[Ellipsis]", "}"}], 
   {_, "AssociationIcon", ___} :> RowBox[{"<|", "\[Ellipsis]", "|>"}], 
   {_, "StringIcon", ___} :> "\"\\[Ellipsis]\"", {_, "SequenceIcon", ___} :> "\[Ellipsis]", 
   {_, other_, ___} :> (other //. DynamicBox[FEPrivate`FrontEndResource["FEBitmaps", "IconizeEllipsis"], ___] :> "\[Ellipsis]"), 
   else_ :> "\[Ellipsis]"}]

MakeTeXRule[lhs_ -> rhs_] := MakeTeXRule[lhs :> rhs]
MakeTeXRule[lhs_ :> rhs_] := (MakeTeX[lhs] := rhs)

BoxQ[box_] := 
 MatchQ[Unevaluated@box, 
  _String | _RowBox | _GridBox | _SuperscriptBox | _SubscriptBox | 
   _SubsuperscriptBox | _OverscriptBox | _UnderscriptBox | 
   _UnderoverscriptBox | _FractionBox | _SqrtBox | _RadicalBox | 
   _StyleBox | _FrameBox | _PaneBox | _PanelBox | _AdjustmentBox | 
   _ButtonBox | _FormBox | _InterpretationBox | _TagBox | _ErrorBox | 
   _CounterBox | _ValueBox | _OptionValueBox | _Cell | _BoxData | 
   _TextData | _Notebook | _Graphics | _Graphics3D | _GraphicsArray | 
   _GraphicsGrid | _SurfaceGraphics | _ContourGraphics | 
   _DensityGraphics | _AnimatorBox | _CheckboxBox | _DynamicBox | 
   _InputFieldBox | _MultiviewBox | _OpenerBox | _PopupMenuBox | 
   _RadioButtonBox | _SliderBox | _TabbedMultiviewBox | _TooltipBox | 
   _AreaSliderBox | _PaneSelectorBox | _TemplateBox | _TabViewBox]

Texformdump`BoxesToTeX[other_, opts___?OptionQ] := 
 Null /; Message[Texformdump`BoxesToTeX::notboxes, other, Texformdump`BoxesToTeX]

Texformdump`BoxesToTeX[other___, opts___?OptionQ] := 
 Null /; (Length@{other} =!= 1 && 
    Message[Texformdump`BoxesToTeX::argx, Texformdump`BoxesToTeX, Length@{other}])

(* --- ExpressionToTeX --- *)
Texformdump`ExpressionToTeX[expr_, opts___?OptionQ] := 
 Texformdump`BoxesToTeX[MakeBoxes[expr, TraditionalForm], opts, 
  "BoxRules" -> $GreekWords]

Texformdump`ExpressionToTeX[other___] := 
 Null /; Message[Texformdump`ExpressionToTeX::argx, Texformdump`ExpressionToTeX, Length@{other}]

(* --- MakeTeX ---- *)
(* Any overloading of MakeTeX overrides all maketex rules. BUT it is very
important that maketex recursively calls MakeTeX, not maketex, so that user
definitions can override at any level of the expression. *)

(* Only built-in rule *)
MakeTeX[boxes_] := maketex[boxes]

(*MakeTeX[RowBox[{"Theorema`Language`Iff$TM", ___}]] := "test2a"

maketex[RowBox[{"Theorema`Language`Iff$TM", ___}]] := "test3a"*)

(* --- maketex --- *)

(* special cases *)
maketex[RowBox[{"T", AdjustmentBox["E", ___], "X"}]] := "\\TeX{}"

maketex[
  RowBox[{"L", StyleBox[AdjustmentBox["A", ___], ___], "T", 
    AdjustmentBox["E", ___], "X"}]] := "\\LaTeX{}"

(* distiguish piecewise from matrix *)
PW = "\[Piecewise]"

maketex[RowBox[{PW, GridBox[grid_, ___]}]] := 
 StringJoin["\\begin{cases}", "\n", Trim@Map[MakeRow, grid], "\n", 
  "\\end{cases}\n"]

(* Notebook/Linebreaking *)

(* single cell *)
maketex[
  nb : Notebook[{cell : Cell[cont_, sty_String : "", opts___?OptionQ]}, 
    ___]] := 
 Module[{pkt}, 
  DebugPrint["------------------------------------"];
  DebugPrint[
   "maketex[nb:Notebook[{cell:Cell[cont_, sty_String:\"\", opts___?OptionQ]}, ___]]"];
  DebugPrint["nb: ", nb];
  If[(*
        Front end doesn't like to compute the line break of an empty box.
        See bug 47285 for the gory details.
    *)MatchQ[cell, Cell[BoxData[""], ___]], pkt = $Failed, 
   pkt = Developer`UseFrontEnd[
     MathLink`CallFrontEnd[FrontEnd`GetLinebreakInformationPacket[nb]]]];
  pkt = StripIdentityBoxes[pkt];
  If[pkt === $Failed, MakeTeX@cell, 
   MakeTeX[
    pkt /. {Conversion`LineWrapBox[{elem_}, ___] :> 
       StyleBox[elem, sty, opts], 
      Conversion`LineWrapBox[boxes_List, ___] :> 
       Conversion`LineWrapBox@Map[StyleBox[#, sty, opts] &, boxes]}]]]

maketex[Conversion`LineWrapBox[{elem_}, ___]] := 
 (DebugPrint["------------------------------------"];
  DebugPrint["maketex[Conversion`LineWrapBox[{elem_}, ___]]"];
  DebugPrint["elem:", elem];
  MakeTeX[elem])

maketex[Conversion`LineWrapBox[boxes_List, ___]] := 
 (DebugPrint["------------------------------------"];
  DebugPrint["maketex[Conversion`LineWrapBox[boxes_List, ___]]"];
  DebugPrint["boxes:", boxes];
  Intercalate[boxes, "\\\\\n"])

(* if no fe *)
maketex[Notebook[{cell_}, ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[Notebook[{cell_}, ___] ]"];
  DebugPrint["cell: ", cell];
  MakeTeX[cell]
)

(* multiple cells *)
maketex[Notebook[cells_, ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[Notebook[cells_, ___]]"];
  DebugPrint["cells: ", cells]; 
  Intercalate[cells, "\\\\\n\\\\\n"]
)

Intercalate[stuff_List, item_] :=
  StringJoin@Most@Flatten@Map[{MakeTeX[#], item}&, stuff]
  
(* BoxData *)

maketex[BoxData[boxes_List]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[BoxData[boxes_List]]"];
  DebugPrint["boxes: ", boxes]; 
  Intercalate[boxes, "\\\\\n"]
)

maketex[BoxData[boxes_]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[BoxData[boxes_]]"];
  DebugPrint["boxes: ", boxes];
  MakeTeX[boxes]
)

(* String *)

(* Embedded *)
maketex[str_String?EmbeddedStringWithLinearSyntaxQ] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[str_String?EmbeddedStringWithLinearSyntaxQ]"];
  DebugPrint["str: ", str];
  Module[{strippedstr},
	strippedstr = RemoveLinearSyntax[str, Recursive->True];
	If [SameQ[str, strippedstr],
		str,
		MakeTeX[strippedstr]
		]
	]
)


(* single character *)
$GreekLetters =
{
  "\[Alpha]" -> "\\alpha ",
  "\[Beta]" -> "\\beta ",
  "\[Gamma]" -> "\\gamma ",
  "\[Delta]" -> "\\delta ",
  "\[Epsilon]" -> "\\epsilon ",
  "\[CurlyEpsilon]" -> "\\varepsilon ",
  "\[Zeta]" -> "\\zeta ",
  "\[Eta]" -> "\\eta ",
  "\[Theta]" -> "\\theta ",
  "\[CurlyTheta]" -> "\\vartheta ",
  "\[Iota]" -> "\\iota ",
  "\[Kappa]" -> "\\kappa ",
  "\[CurlyKappa]" -> "\\varkappa ",
  "\[Lambda]" -> "\\lambda ",
  "\[Mu]" -> "\\mu ",
  "\[Nu]" -> "\\nu ",
  "\[Xi]" -> "\\xi ",
  "\[Omicron]" -> "o",
  "\[Pi]" -> "\\pi ",
  "\[CurlyPi]" -> "\\varpi ",
  "\[Rho]" -> "\\rho ",
  "\[CurlyRho]" -> "\\varrho ",
  "\[Sigma]" -> "\\sigma ",
  "\[FinalSigma]" -> "\\varsigma ",
  "\[Tau]" -> "\\tau ",
  "\[Upsilon]" -> "\\upsilon ",
  "\[Phi]" -> "\\phi ",
  "\[CurlyPhi]" -> "\\varphi ",
  "\[Chi]" -> "\\chi ",
  "\[Psi]" -> "\\psi ",
  "\[Omega]" -> "\\omega ",
  "\[Digamma]" -> "\\digamma ",
  "\[Koppa]" -> "\\mathsym{\\koppa}", (* ? *)
  "\[Stigma]" -> "\\varsigma", (* ? *)
  "\[Sampi]" -> "\\mathsym{\\sampi}", (* ? *)
  "\[CapitalAlpha]" -> "A",
  "\[CapitalBeta]" -> "B",
  "\[CapitalGamma]" -> "\\Gamma ",
  "\[CapitalDelta]"  -> "\\Delta ",
  "\[CapitalEpsilon]" -> "E",
  "\[CapitalZeta]" -> "Z",
  "\[CapitalEta]" -> "H",
  "\[CapitalTheta]" -> "\\Theta ",
  "\[CapitalIota]" -> "I",
  "\[CapitalKappa]" -> "K",
  "\[CapitalLambda]" -> "\\Lambda ",
  "\[CapitalMu]" -> "M",
  "\[CapitalNu]" -> "N",
  "\[CapitalXi]" -> "\\Xi ",
  "\[CapitalOmicron]" -> "O",
  "\[CapitalPi]" -> "\\Pi ",
  "\[CapitalRho]" -> "P",
  "\[CapitalSigma]" -> "\\Sigma ",
  "\[CapitalTau]" -> "T",
  "\[CapitalUpsilon]" -> "\\Upsilon ",
  "\[CurlyCapitalUpsilon]" -> "\\varUpsilon ", (* ? *)
  "\[CapitalPhi]" -> "\\Phi ",
  "\[CapitalChi]" -> "X",
  "\[CapitalPsi]" -> "\\Psi ",
  "\[CapitalOmega]" -> "\\Omega ",
  "\[CapitalDigamma]" -> "F",
  "\[CapitalKoppa]" -> "\\mathsym{\\CapitalKoppa}", (* ? *)
  "\[CapitalStigma]" -> "\\varsigma", (* ? *)
  "\[CapitalSampi]" -> "\\mathsym{\\CapitalSampi}" (* ? *)
}
$GreekWords = (* silly peudo-characters from pre-V3 days *)
{
  "alpha" :> MakeTeX["\[Alpha]"],
  "beta" :> MakeTeX["\[Beta]"],
  "gamma" :> MakeTeX["\[Gamma]"],
  "delta" :> MakeTeX["\[Delta]"],
  "epsilon" :> MakeTeX["\[Epsilon]"],
  "zeta" :> MakeTeX["\[Zeta]"],
  "eta" :> MakeTeX["\[Eta]"],
  "theta" :> MakeTeX["\[Theta]"],
  "iota" :> MakeTeX["\[Iota]"],
  "kappa" :> MakeTeX["\[Kappa]"],
  "lambda" :> MakeTeX["\[Lambda]"],
  "mu" :> MakeTeX["\[Mu]"],
  "nu" :> MakeTeX["\[Nu]"],
  "xi" :> MakeTeX["\[Xi]"],
  "omicron" :> MakeTeX["\[Omicron]"],
  "pi" :> MakeTeX["\[Pi]"],
  "rho" :> MakeTeX["\[Rho]"],
  "sigma" :> MakeTeX["\[Sigma]"],
  "tau" :> MakeTeX["\[Tau]"],
  "upsilon" :> MakeTeX["\[Upsilon]"],
  "phi" :> MakeTeX["\[Phi]"],
  "chi" :> MakeTeX["\[Chi]"],
  "psi" :> MakeTeX["\[Psi]"],
  "omega" :> MakeTeX["\[Omega]"],
  "digamma" :> MakeTeX["\[Digamma]"],
  "ALPHA" :> MakeTeX["\[CapitalAlpha]"],
  "BETA" :> MakeTeX["\[CapitalBeta]"],
  "GAMMA" :> MakeTeX["\[CapitalGamma]"],
  "DELTA" :> MakeTeX["\[CapitalDelta]"],
  "EPSILON" :> MakeTeX["\[CapitalEpsilon]"],
  "ZETA" :> MakeTeX["\[CapitalZeta]"],
  "ETA" :> MakeTeX["\[CapitalEta]"],
  "THETA" :> MakeTeX["\[CapitalTheta]"],
  "IOTA" :> MakeTeX["\[CapitalIota]"],
  "KAPPA" :> MakeTeX["\[CapitalKappa]"],
  "LAMBDA" :> MakeTeX["\[CapitalLambda]"],
  "MU" :> MakeTeX["\[CapitalMu]"],
  "NU" :> MakeTeX["\[CapitalNu]"],
  "XI" :> MakeTeX["\[CapitalXi]"],
  "OMICRON" :> MakeTeX["\[CapitalOmicron]"],
  "PI" :> MakeTeX["\[CapitalPi]"],
  "RHO" :> MakeTeX["\[CapitalRho]"],
  "SIGMA" :> MakeTeX["\[CapitalSigma]"],
  "TAU" :> MakeTeX["\[CapitalTau]"],
  "UPSILON" :> MakeTeX["\[CapitalUpsilon]"],
  "PHI" :> MakeTeX["\[CapitalPhi]"],
  "CHI" :> MakeTeX["\[CapitalChi]"],
  "PSI" :> MakeTeX["\[CapitalPsi]"],
  "OMEGA" :> MakeTeX["\[CapitalOmega]"],
  "DIGAMMA" :> MakeTeX["\[CapitalDigamma]"]
}
(* \mathcal{letter} - only uppercase, use \mathit{letter} as poor approx  *)
$CaligraphicLetters = {
  "\[ScriptA]" -> "\\mathit{a}",
  "\[ScriptB]" -> "\\mathit{b}",
  "\[ScriptC]" -> "\\mathit{c}",
  "\[ScriptD]" -> "\\mathit{d}",
  "\[ScriptE]" -> "\\mathit{e}",
  "\[ScriptF]" -> "\\mathit{f}",
  "\[ScriptG]" -> "\\mathit{g}",
  "\[ScriptH]" -> "\\mathit{h}",
  "\[ScriptI]" -> "\\mathit{i}",
  "\[ScriptJ]" -> "\\mathit{j}",
  "\[ScriptK]" -> "\\mathit{k}",
  "\[ScriptL]" -> "\\ell ",
  "\[ScriptM]" -> "\\mathit{m}",
  "\[ScriptN]" -> "\\mathit{n}",
  "\[ScriptO]" -> "\\mathit{o}",
  "\[ScriptP]" -> "\\mathit{p}",
  "\[ScriptQ]" -> "\\mathit{q}",
  "\[ScriptR]" -> "\\mathit{r}",
  "\[ScriptS]" -> "\\mathit{s}",
  "\[ScriptT]" -> "\\mathit{t}",
  "\[ScriptU]" -> "\\mathit{u}",
  "\[ScriptV]" -> "\\mathit{v}",
  "\[ScriptW]" -> "\\mathit{w}",
  "\[ScriptX]" -> "\\mathit{x}",
  "\[ScriptY]" -> "\\mathit{y}",
  "\[ScriptZ]" -> "\\mathit{z}",
  "\[ScriptCapitalA]" -> "\\mathcal{A}",
  "\[ScriptCapitalB]" -> "\\mathcal{B}",
  "\[ScriptCapitalC]" -> "\\mathcal{C}",
  "\[ScriptCapitalD]" -> "\\mathcal{D}",
  "\[ScriptCapitalE]" -> "\\mathcal{E}",
  "\[ScriptCapitalF]" -> "\\mathcal{F}",
  "\[ScriptCapitalG]" -> "\\mathcal{G}",
  "\[ScriptCapitalH]" -> "\\mathcal{H}",
  "\[ScriptCapitalI]" -> "\\mathcal{I}",
  "\[ScriptCapitalJ]" -> "\\mathcal{J}",
  "\[ScriptCapitalK]" -> "\\mathcal{K}",
  "\[ScriptCapitalL]" -> "\\mathcal{L}",
  "\[ScriptCapitalM]" -> "\\mathcal{M}",
  "\[ScriptCapitalN]" -> "\\mathcal{N}",
  "\[ScriptCapitalO]" -> "\\mathcal{O}",
  "\[ScriptCapitalP]" -> "\\mathcal{P}",
  "\[ScriptCapitalQ]" -> "\\mathcal{Q}",
  "\[ScriptCapitalR]" -> "\\mathcal{R}",
  "\[ScriptCapitalS]" -> "\\mathcal{S}",
  "\[ScriptCapitalT]" -> "\\mathcal{T}",
  "\[ScriptCapitalU]" -> "\\mathcal{U}",
  "\[ScriptCapitalV]" -> "\\mathcal{V}",
  "\[ScriptCapitalW]" -> "\\mathcal{W}",
  "\[ScriptCapitalX]" -> "\\mathcal{X}",
  "\[ScriptCapitalY]" -> "\\mathcal{Y}",
  "\[ScriptCapitalZ]" -> "\\mathcal{Z}",
  "\[ScriptOne]" -> "\\mathit{1}",
  "\[ScriptTwo]" -> "\\mathit{2}",
  "\[ScriptThree]" -> "\\mathit{3}",
  "\[ScriptFour]" -> "\\mathit{4}",
  "\[ScriptFive]" -> "\\mathit{5}",
  "\[ScriptSix]" -> "\\mathit{6}",
  "\[ScriptSeven]" -> "\\mathit{7}",
  "\[ScriptEight]" -> "\\mathit{8}",
  "\[ScriptNine]" -> "\\mathit{9}",
  "\[ScriptZero]" -> "\\mathit{0}"
}
(* frak letters are poor arrrox of gothic *)
$GothicLetters = {
  "\[GothicA]" -> "\\mathfrak{a}",
  "\[GothicB]" -> "\\mathfrak{b}",
  "\[GothicC]" -> "\\mathfrak{c}",
  "\[GothicD]" -> "\\mathfrak{d}",
  "\[GothicE]" -> "\\mathfrak{e}",
  "\[GothicF]" -> "\\mathfrak{f}",
  "\[GothicG]" -> "\\mathfrak{g}",
  "\[GothicH]" -> "\\mathfrak{h}",
  "\[GothicI]" -> "\\mathfrak{i}",
  "\[GothicJ]" -> "\\mathfrak{j}",
  "\[GothicK]" -> "\\mathfrak{k}",
  "\[GothicL]" -> "\\mathfrak{l}",
  "\[GothicM]" -> "\\mathfrak{m}",
  "\[GothicN]" -> "\\mathfrak{n}",
  "\[GothicO]" -> "\\mathfrak{o}",
  "\[GothicP]" -> "\\mathfrak{p}",
  "\[GothicQ]" -> "\\mathfrak{q}",
  "\[GothicR]" -> "\\mathfrak{r}",
  "\[GothicS]" -> "\\mathfrak{s}",
  "\[GothicT]" -> "\\mathfrak{t}",
  "\[GothicU]" -> "\\mathfrak{u}",
  "\[GothicV]" -> "\\mathfrak{v}",
  "\[GothicW]" -> "\\mathfrak{w}",
  "\[GothicX]" -> "\\mathfrak{x}",
  "\[GothicY]" -> "\\mathfrak{y}",
  "\[GothicZ]" -> "\\mathfrak{z}",
  "\[GothicCapitalA]" -> "\\mathfrak{A}",
  "\[GothicCapitalB]" -> "\\mathfrak{B}",
  "\[GothicCapitalC]" -> "\\mathfrak{C}",
  "\[GothicCapitalD]" -> "\\mathfrak{D}",
  "\[GothicCapitalE]" -> "\\mathfrak{E}",
  "\[GothicCapitalF]" -> "\\mathfrak{F}",
  "\[GothicCapitalG]" -> "\\mathfrak{G}",
  "\[GothicCapitalH]" -> "\\mathfrak{H}",
  "\[GothicCapitalI]" -> "\\mathfrak{I}",
  "\[GothicCapitalJ]" -> "\\mathfrak{J}",
  "\[GothicCapitalK]" -> "\\mathfrak{K}",
  "\[GothicCapitalL]" -> "\\mathfrak{L}",
  "\[GothicCapitalM]" -> "\\mathfrak{M}",
  "\[GothicCapitalN]" -> "\\mathfrak{N}",
  "\[GothicCapitalO]" -> "\\mathfrak{O}",
  "\[GothicCapitalP]" -> "\\mathfrak{P}",
  "\[GothicCapitalQ]" -> "\\mathfrak{Q}",
  "\[GothicCapitalR]" -> "\\mathfrak{R}",
  "\[GothicCapitalS]" -> "\\mathfrak{S}",
  "\[GothicCapitalT]" -> "\\mathfrak{T}",
  "\[GothicCapitalU]" -> "\\mathfrak{U}",
  "\[GothicCapitalV]" -> "\\mathfrak{V}",
  "\[GothicCapitalW]" -> "\\mathfrak{W}",
  "\[GothicCapitalX]" -> "\\mathfrak{X}",
  "\[GothicCapitalY]" -> "\\mathfrak{Y}",
  "\[GothicCapitalZ]" -> "\\mathfrak{Z}",
  "\[GothicOne]" -> "\\mathfrak{1}",
  "\[GothicTwo]" -> "\\mathfrak{2}",
  "\[GothicThree]" -> "\\mathfrak{3}",
  "\[GothicFour]" -> "\\mathfrak{4}",
  "\[GothicFive]" -> "\\mathfrak{5}",
  "\[GothicSix]" -> "\\mathfrak{6}",
  "\[GothicSeven]" -> "\\mathfrak{7}",
  "\[GothicEight]" -> "\\mathfrak{8}",
  "\[GothicNine]" -> "\\mathfrak{9}",
  "\[GothicZero]" -> "\\mathfrak{0}"
}
(* mathbb only available for uppercase letters, use mathbf as poor approx. *)
$DoubleStruckLetters = {
  "\[DoubleStruckA]" -> "\\mathbf{a}",
  "\[DoubleStruckB]" -> "\\mathbf{b}",
  "\[DoubleStruckC]" -> "\\mathbf{c}",
  "\[DoubleStruckD]" -> "\\mathbf{d}",
  "\[DoubleStruckE]" -> "\\mathbf{e}",
  "\[DoubleStruckF]" -> "\\mathbf{f}",
  "\[DoubleStruckG]" -> "\\mathbf{g}",
  "\[DoubleStruckH]" -> "\\mathbf{h}",
  "\[DoubleStruckI]" -> "\\mathbf{i}",
  "\[DoubleStruckJ]" -> "\\mathbf{j}",
  "\[DoubleStruckK]" -> "\\Bbbk ",
  "\[DoubleStruckL]" -> "\\mathbf{l}",
  "\[DoubleStruckM]" -> "\\mathbf{m}",
  "\[DoubleStruckN]" -> "\\mathbf{n}",
  "\[DoubleStruckO]" -> "\\mathbf{o}",
  "\[DoubleStruckP]" -> "\\mathbf{p}",
  "\[DoubleStruckQ]" -> "\\mathbf{q}",
  "\[DoubleStruckR]" -> "\\mathbf{r}",
  "\[DoubleStruckS]" -> "\\mathbf{s}",
  "\[DoubleStruckT]" -> "\\mathbf{t}",
  "\[DoubleStruckU]" -> "\\mathbf{u}",
  "\[DoubleStruckV]" -> "\\mathbf{v}",
  "\[DoubleStruckW]" -> "\\mathbf{w}",
  "\[DoubleStruckX]" -> "\\mathbf{x}",
  "\[DoubleStruckY]" -> "\\mathbf{y}",
  "\[DoubleStruckZ]" -> "\\mathbf{z}",
  "\[DoubleStruckCapitalA]" -> "\\mathbb{A}",
  "\[DoubleStruckCapitalB]" -> "\\mathbb{B}",
  "\[DoubleStruckCapitalC]" -> "\\mathbb{C}",
  "\[DoubleStruckCapitalD]" -> "\\mathbb{D}",
  "\[DoubleStruckCapitalE]" -> "\\mathbb{E}",
  "\[DoubleStruckCapitalF]" -> "\\mathbb{F}",
  "\[DoubleStruckCapitalG]" -> "\\mathbb{G}",
  "\[DoubleStruckCapitalH]" -> "\\mathbb{H}",
  "\[DoubleStruckCapitalI]" -> "\\mathbb{I}",
  "\[DoubleStruckCapitalJ]" -> "\\mathbb{J}",
  "\[DoubleStruckCapitalK]" -> "\\mathbb{K}",
  "\[DoubleStruckCapitalL]" -> "\\mathbb{L}",
  "\[DoubleStruckCapitalM]" -> "\\mathbb{M}",
  "\[DoubleStruckCapitalN]" -> "\\mathbb{N}",
  "\[DoubleStruckCapitalO]" -> "\\mathbb{O}",
  "\[DoubleStruckCapitalP]" -> "\\mathbb{P}",
  "\[DoubleStruckCapitalQ]" -> "\\mathbb{Q}",
  "\[DoubleStruckCapitalR]" -> "\\mathbb{R}",
  "\[DoubleStruckCapitalS]" -> "\\mathbb{S}",
  "\[DoubleStruckCapitalT]" -> "\\mathbb{T}",
  "\[DoubleStruckCapitalU]" -> "\\mathbb{U}",
  "\[DoubleStruckCapitalV]" -> "\\mathbb{V}",
  "\[DoubleStruckCapitalW]" -> "\\mathbb{W}",
  "\[DoubleStruckCapitalX]" -> "\\mathbb{X}",
  "\[DoubleStruckCapitalY]" -> "\\mathbb{Y}",
  "\[DoubleStruckCapitalZ]" -> "\\mathbb{Z}",
  "\[DoubleStruckOne]" -> "\\mathbf{1}",
  "\[DoubleStruckTwo]" -> "\\mathbf{2}",
  "\[DoubleStruckThree]" -> "\\mathbf{3}",
  "\[DoubleStruckFour]" -> "\\mathbf{4}",
  "\[DoubleStruckFive]" -> "\\mathbf{5}",
  "\[DoubleStruckSix]" -> "\\mathbf{6}",
  "\[DoubleStruckSeven]" -> "\\mathbf{7}",
  "\[DoubleStruckEight]" -> "\\mathbf{8}",
  "\[DoubleStruckNine]" -> "\\mathbf{9}",
  "\[DoubleStruckZero]" -> "\\mathbf{0}"
}
$AccentedLetters = {
  "\[AGrave]" -> "\\text{\\` a}",
  "\[AAcute]" -> "\\text{\\' a}",
  "\[AHat]" -> "\\text{\\^ a}",
  "\[ACup]" -> "\\text{\\u a}",
  "\[ATilde]" -> "\\text{\\~ a}",
  "\[ADoubleDot]" -> "\\text{\\\" a}",
  "\[ABar]" -> "\\text{\\= a}",
  "\[ARing]" -> "\\text{\\aa}",
  "\[AE]" -> "\\text{\\ae}",
  "\[CAcute]" -> "\\text{\\' c}",
  "\[CHacek]" ->  "\\text{\\v c}",
  "\[CCedilla]" -> "\\text{\\c c}",
  "\[EGrave]" -> "\\text{\\` e}",
  "\[EAcute]" -> "\\text{\\' e}",
  "\[EHat]" -> "\\text{\\^ e}",
  "\[ECup]" -> "\\text{\\u e}",
  "\[EDoubleDot]" -> "\\text{\\\" e}",
  "\[EBar]" -> "\\text{\\= e}",
  "\[IGrave]" -> "\\text{\\` \\i}",
  "\[IAcute]" -> "\\text{\\' \\i}",
  "\[IHat]" -> "\\text{\\^ \\i}",
  "\[ICup]" -> "\\text{\\u \\i}",
  "\[IDoubleDot]" -> "\\text{\\\" \\i}",
  "\[Eth]" -> "\\eth ",
  "\[LSlash]" -> "\\text{\\l}",
  "\[NTilde]" -> "\\text{\\~ n}",
  "\[OGrave]" -> "\\text{\\` o}",
  "\[OAcute]" -> "\\text{\\' o}",
  "\[OHat]" -> "\\text{\\^ o}",
  "\[OTilde]" -> "\\text{\\~ o}",
  "\[ODoubleDot]" -> "\\text{\\\" o}",
  "\[ODoubleAcute]" -> "\\text{\\H o}",
  "\[OSlash]" -> "\\text{\\o}",
  "\[SHacek]" -> "\\text{\\v s}",
  "\[UGrave]" -> "\\text{\\` u}",
  "\[UAcute]" -> "\\text{\\' u}",
  "\[UHat]" -> "\\text{\\^ u}",
  "\[UDoubleDot]" -> "\\text{\\\" u}",
  "\[UDoubleAcute]" -> "\\text{\\H u}",
  "\[YAcute]" -> "\\text{\\' y}",
  "\[YDoubleDot]" -> "\\text{\\\" y}",
  "\[Thorn]" -> "\\mathsym{\\Thorn}", (* ? *)
  "\[CapitalAGrave]" -> "\\text{\\` A}",
  "\[CapitalAAcute]" -> "\\text{\\' A}",
  "\[CapitalAHat]" -> "\\text{\\^ A}",
  "\[CapitalACup]" -> "\\text{\\u A}",
  "\[CapitalATilde]" -> "\\text{\\~ A}",
  "\[CapitalADoubleDot]" -> "\\text{\\\" A}",
  "\[CapitalABar]" -> "\\text{\\= A}",
  "\[CapitalARing]" -> "\\text{\\AA}",
  "\[CapitalAE]" -> "\\text{\\AE}",
  "\[CapitalCAcute]" -> "\\text{\\' C}",
  "\[CapitalCHacek]" ->  "\\text{\\v C}",
  "\[CapitalCCedilla]" -> "\\text{\\c C}",
  "\[CapitalEGrave]" -> "\\text{\\` E}",
  "\[CapitalEAcute]" -> "\\text{\\' E}",
  "\[CapitalEHat]" -> "\\text{\\^ E}",
  "\[CapitalECup]" -> "\\text{\\u E}",
  "\[CapitalEDoubleDot]" -> "\\text{\\\" E}",
  "\[CapitalEBar]" -> "\\text{\\= E}",
  "\[CapitalIGrave]" -> "\\text{\\` I}",
  "\[CapitalIAcute]" -> "\\text{\\' I}",
  "\[CapitalIHat]" -> "\\text{\\^ I}",
  "\[CapitalICup]" -> "\\text{\\u I}",
  "\[CapitalIDoubleDot]" -> "\\text{\\\" I}",
  "\[CapitalEth]" -> "\\mathsym{\\CapitalEth}",
  "\[CapitalLSlash]" -> "\\text{\\L}",
  "\[CapitalNTilde]" -> "\\text{\\~ N}",
  "\[CapitalOGrave]" -> "\\text{\\` O}",
  "\[CapitalOAcute]" -> "\\text{\\' O}",
  "\[CapitalOHat]" -> "\\text{\\^ O}",
  "\[CapitalOTilde]" -> "\\text{\\~ O}",
  "\[CapitalODoubleDot]" -> "\\text{\\\" O}",
  "\[CapitalODoubleAcute]" -> "\\text{\\H O}",
  "\[CapitalOSlash]" -> "\\text{\\O}",
  "\[CapitalSHacek]" -> "\\text{\\v S}",
  "\[CapitalUGrave]" -> "\\text{\\` U}",
  "\[CapitalUAcute]" -> "\\text{\\' U}",
  "\[CapitalUHat]" -> "\\text{\\^ U}",
  "\[CapitalUDoubleDot]" -> "\\text{\\\" U}",
  "\[CapitalUDoubleAcute]" -> "\\text{\\H U}",
  "\[CapitalYAcute]" -> "\\text{\\' Y}",
  "\[CapitalThorn]" -> "\\mathsym{\\CapitalThorn}",
  "\[SZ]" -> "\\text{\\ss}"
}
$MiscellaneousSymbols = {
  "\[ConstantC]" -> "c", (* sematically this should be right *)
  "\[ExponentialE]" -> "e", (* sematically this should be right *)
  "\[ImaginaryI]" -> "i", (* sematically this should be right *)
  "\[ImaginaryJ]" -> "j", (* sematically this should be right *)
  "\[DoubledPi]" -> "\\pi ", (* ? *)
  "\[DoubledGamma]" -> "\\gamma ", (* ? *)
  "\[Infinity]" -> "\\infty ",
  "\[Micro]" -> "\\mu ", (* ? *)
  "\[Mho]" -> "\\mho ",
  "\[Angstrom]" -> "\\text{\\AA}",
  "\[HBar]" -> "\\hbar ", (* ? \\hslash looks better but \\hbar is closer semantically *)
  "\[Diameter]" -> "\\varnothing ", (* ? *)
  "\[Aleph]" -> "\\aleph ",
  "\[Bet]" -> "\\beth ",
  "\[Gimel]" -> "\\gimel ",
  "\[Dalet]" -> "\\daleth ",
  "\[WeierstrassP]" -> "\\wp ",
  "\[EmptySet]" -> "\\emptyset ",
  "\[Degree]" -> "{}^{\\circ}",
  "\[Angle]" -> "\\angle ",
  "\[RightAngle]" -> "\\angle", (* ? *)
  "\[MeasuredAngle]" -> "\\measuredangle ",
  "\[SphericalAngle]" -> "\\sphericalangle ",
  "\[Mercury]" -> "\\mathsym{\\Mercury}", (* ? *)
  "\[Venus]" -> "\\mathsym{\\Venus}", (* ? *)
  "\[Earth]" -> "\\mathsym{\\Earth}",  (* ? *)
  "\[Mars]" -> "\\mathsym{\\Mars}", (* ? *)
  "\[Jupiter]" -> "\\mathsym{\\Jupiter}", (* ? *)
  "\[Saturn]" -> "\\mathsym{\\Saturn}", (* ? *)
  "\[Uranus]" -> "\\mathsym{\\Uranus}", (* ? *)
  "\[Neptune]" -> "\\mathsym{\\Neptune}",  (* ? *)
  "\[Pluto]" -> "\\mathsym{\\Pluto}" (* ? *)
}
$Shapes = {
  "\[FilledSquare]" -> "\\blacksquare ",
  "\[FilledSmallCircle]" -> "\\bullet ",
  "\[FilledCircle]" -> "\\bullet", (* ? *)
  "\[FilledRectangle]" -> "\\blacksquare", (* ? *)
  "\[FilledUpTriangle]" -> "\\blacktriangle ",
  "\[FilledDownTriangle]" -> "\\blacktriangledown ",
  "\[FilledLeftTriangle]" -> "\\blacktriangleleft ",
  "\[FilledRightTriangle]" -> "\\blacktriangleright ",
  "\[EmptySquare]" -> "\\square ",
  "\[EmptySmallCircle]" -> "\\circ ",
  "\[EmptyCircle]" -> "\\bigcirc ",
  "\[EmptyRectangle]" -> "\\square", (* ? *)
  "\[EmptyUpTriangle]" -> "\\triangle ", (* or \\vartriangle *)
  "\[EmptyDownTriangle]" -> "\\triangledown ",
  "\[FilledVerySmallSquare]" -> "\\square", (* ? *)
  "\[FilledSmallSquare]" -> "\\blacksquare", (* ? *)
  "\[FilledDiamond]" -> "\\blacklozenge ",
  "\[GraySquare]" -> "\\blacksquare", (* ? *)
  "\[GrayCircle]" -> "\\bullet", (* ? *)
  "\[EmptyVerySmallSquare]" -> "\\square", (* ? *)
  "\[EmptySmallSquare]" -> "\\square", (* ? *)
  "\[EmptyDiamond]" -> "\\lozenge ",
  "\[DottedSquare]" -> "\\square", (* ? *)
  "\[FirstPage]" -> "|\\blacktriangleleft ", (* ? *)
  "\[LeftPointer]" -> "\\blacktriangleleft ",
  "\[UpPointer]" -> "\\blacktriangle ",
  "\[DownPointer]" -> "\\blacktriangledown ",
  "\[RightPointer]" -> "\\blacktriangleright ",
  "\[LastPage]" -> "\\blacktriangleright |", (* ? *)
  "\[FivePointedStar]" -> "\\star ",
  "\[SixPointedStar]" -> "\\ast", (* ? *)
  "\[DiamondSuit]" -> "\\diamondsuit ",
  "\[ClubSuit]" -> "\\clubsuit ",
  "\[HeartSuit]" -> "\\heartsuit ",
  "\[SpadeSuit]" -> "\\spadesuit ",
  "\[MathematicaIcon]" -> "", (* ? *)
  "\[KernelIcon]" -> "\\mathsym{\\KernelIcon}", (* ? *)
  "\[Checkmark]" -> "\\checkmark ",
  "\[CheckmarkedBox]" -> "\\fbox{\\checkmark}",
  "\[WatchIcon]" -> "\\mathsym{\\WatchIcon}",   (* ? *)
  "\[WarningSign]" -> "\\mathsym{\\WarningSign}",  (* ? *)
  "\[HappySmiley]" -> "\\mathsym{\\HappySmiley}",  (* ? *)
  "\[NeutralSmiley]" -> "\\mathsym{\\NeutralSmiley}",  (* ? *)
  "\[SadSmiley]" -> "\\mathsym{\\SadSmiley}",   (* ? *)
  "\[FreakedSmiley]" -> "\\mathsym{\\FreakedSmiley}",  (* ? *)
  "\[LightBulb]" -> "\\mathsym{\\LightBulb}",  (* ? *)
  "\[Wolf]" -> "\\mathsym{\\Wolf}"  (* ? *)
}
$TextualForms ={
  "\[DotlessI]" -> "\\text{\\i}",
  "\[DotlessJ]" -> "\\text{\\j}",
  "\[ScriptDotlessI]" -> "\\imath ",
  "\[ScriptDotlessJ]" -> "\\jmath ",
  "\[Dash]" -> "--",
  "\[LongDash]" -> "---",
  "\[Hyphen]" -> "-",
  "\[BeamedSixteenthNote]" -> "\\mathsym{\\BeamedSixteenthNote}",  (* ? *)
  "\[EighthNote]" -> "\\mathsym{\\EighthNote}" , (* ? *)
  "\[BeamedEighthNote]" -> "\\mathsym{\\BeamedEighthNote}",  (* ? *)
  "\[QuarterNote]" -> "\\mathsym{\\QuarterNote}",  (* ? *)
  "\[Flat]" -> "\\flat ",
  "\[Natural]" -> "\\natural ",
  "\[Sharp]" -> "\\sharp ",
  "\[Cent]" -> "\\not{c}", (* ? *)
  "\[Euro]" -> "\\matkfrac{E}", (* ? *)
  "\[Sterling]" -> "\\pounds ",
  "\[Yen]" -> "\\yen ",
  "\[Currency]" -> "\\mathsym{\\Currency}",  (* ? *)
  "\[DownQuestion]" -> "\\text{?`}",
  "\[DownExclamation]" -> "\\text{!`}",
  "\[Copyright]" -> "\\copyright ",
  "\[RegisteredTrademark]" -> "\\circledR ",
  "\[Trademark]" -> "{}^{TM}",
  "\[NumberSign]" -> "\\#",
  "\[Florin]" -> "\\mathit{f}", (* ? *)
  "\[Paragraph]" -> "\\P ",
  "\[Section]" -> "\\S ",
  "\[Bullet]" -> "\\bullet ",
  "\[Dagger]" -> "\\dagger ",
  "\[DoubleDagger]" -> "\\ddagger ",
  "\[Prime]" -> "\\prime ",
  "\[DoublePrime]" -> "\\prime\\prime ",
  "\[ReversePrime]" -> "\\backprime ",
  "\[ReverseDoublePrime]" -> "\\backprime\\backprime ",
  "\[DoubleDot]" -> "..",
  "\[TripleDot]" -> "...",
  "\[Hacek]" -> "{}_{\\check{}}", (* ? *)
  "\[Breve]" -> "\\smallsmile ", (* ?, {}_{\\breve{}} *)
  "\[DownBreve]" -> "\\smallfrown ", (* ? *)
  "\[Cedilla]" -> "\\text{\\c }",
  "\[OpenCurlyQuote]" -> "\\text{`}",
  "\[CloseCurlyQuote]" -> "\\text{'}",
  "\[OpenCurlyDoubleQuote]" -> "\\text{``}",
  "\[CloseCurlyDoubleQuote]" -> "\\text{''}",
  "\[Ellipsis]" -> "\\ldots ",
  "\[CenterEllipsis]" -> "\\cdots ",
  "\[VerticalEllipsis]" -> "\\vdots ",
  "\[AscendingEllipsis]" -> ".\\cdot{}^{\\cdot}", (* ? *)
  "\[DescendingEllipsis]" -> "\\ddots",
  "\[HorizontalLine]" -> "_", (* ? *)
  "\[VerticalLine]" -> "|",
  "\[UnderParenthesis]" -> "\\smile ", (* ? *)
  "\[OverParenthesis]" -> "\\frown ", (* ? *)
  "\[UnderBracket]" -> "\\mathsym{\\UnderBracket}",  (* ? *)
  "\[OverBracket]" -> "\\mathsym{\\OverBracket}",  (* ? *)
  "\[UnderBrace]" -> "\\underbrace{}", (* ? *)
  "\[OverBrace]" -> "{}_{\\overbrace{}}", (* ? *)
  "\[SpaceIndicator]" -> "\\_",  (* ? *)
  "\[RoundSpaceIndicator]" -> "\\smallsmile ", (* ? *)
  "\[CloverLeaf]" -> "\\mathsym{\\CloverLeaf}",  (* ? *)
  "\[ReturnIndicator]" -> "\\mathsym{\\ReturnIndicator}",  (* ? *)
  "\[AltKey]" -> "\\fbox{ALT}",
  "\[CommandKey]" -> "\\fbox{CMD}",
  "\[ControlKey]" -> "\\fbox{CTRL}",
  "\[DeleteKey]" -> "\\fbox{DEL}",
  "\[EnterKey]" -> "\\fbox{ENTER}",
  "\[EscapeKey]" -> "\\fbox{ESC}",
  "\[Mod1Key]" -> "\\fbox{MOD1}",
  "\[Mod2Key]" -> "\\fbox{MOD2}",
  "\[OptionKey]" -> "\\fbox{OPTION}",
  "\[ShiftKey]" -> "\\fbox{SHIFT}",
  "\[SpaceKey]" -> "\\fbox{SPACE}",
  "\[ReturnKey]" -> "\\fbox{RETURN}",
  "\[TabKey]" -> "\\fbox{TAB}",
  "\[KeyBar]" -> "-",
  "\[EntityStart]" -> "\\fbox{\&}",
  "\[EntityEnd]" -> "\\fbox{;}",
  "\[SelectionPlaceholder]" -> "\\blacksquare ",
  "\[AutoPlaceholder]" -> "\\square ", (* ? *)
  "\[Placeholder]" -> "\\square ",
  "\[SkeletonIndicator]" -> "-",
  "\[Continuation]" -> "\\ddots ",
  "\[ErrorIndicator]" -> "\\fbox{$\\smallfrown$}", (* ? *)
  "\[AliasIndicator]" -> "\\vdots ", (* ? *)
  "\[AliasDelimiter]" -> "\\vdots ", (* ? *)
  "\[UnknownGlyph]" -> "\\square " (* ? *)
}
$Operators = {
  "\[Times]" -> "\\times ",
  "\[Divide]" -> "\\div ",
  "\[Sqrt]" -> "\\surd ",
  "\[Cross]" -> "\\times ",
  "\[PlusMinus]" -> "\\pm ",
  "\[MinusPlus]" -> "\\mp ",
  "\[Sum]" -> "\\sum ",
  "\[Product]" -> "\\prod ",
  "\[Del]" -> "\\nabla ",
  "\[DifferentialD]" -> "d", (* semantically d is the right choice for use in an integral *)
  "\[PartialD]" -> "\\partial ",
  "\[CapitalDifferentialD]" -> "\\mathbb{D}", (* ? *)
  "\[Integral]" -> "\\int ",
  "\[ContourIntegral]" -> "\\oint ",
  "\[ClockwiseContourIntegral]" -> "\\oint ", (* ? *)
  "\[CounterClockwiseContourIntegral]" -> "\\oint ", (* ? *)
  "\[DoubleContourIntegral]" -> "\\oint ", (* ? *)
  "\[Rule]" -> "\\to ",
  "\[RuleDelayed]" -> ":\\to ",
  "\[Not]" -> "\\neg ",
  "\[And]" -> "\\land ",
  "\[Nand]" -> "\\barwedge ",
  "\[Or]" -> "\\lor ",
  "\[Nor]" -> "\\bar{\\vee}",
  "\[Xor]" -> "\\veebar ",
  "\[Implies]" -> "\\Rightarrow ",
  "\[ForAll]" -> "\\forall ",
  "\[Exists]" -> "\\exists ",
  "\[NotExists]" -> "\\nexists ",
  "\[SuchThat]" -> "\\backepsilon ",
  "\[Therefore]" -> "\\therefore ",
  "\[Because]" -> "\\because ",
  "\[RoundImplies]" -> "\\Rightarrow ", (* ? *)
  "\[SmallCircle]" -> "\\circ ",
  "\[CirclePlus]" -> "\\oplus ",
  "\[CircleMinus]" -> "\\ominus ",
  "\[CircleDot]" -> "\\odot ",
  "\[CircleTimes]" -> "\\otimes ",
  "\[Diamond]" -> "\\diamond ",
  "\[CenterDot]" -> "\\cdot ",
  "\[Star]" -> "*", (* ? *)
  "\[VerticalTilde]" -> "\\wr ",
  "\[Backslash]" -> "\\backslash ",
  "\[Wedge]" -> "\\wedge ",
  "\[Vee]" -> "\\vee ",
  "\[Cap]" -> "\\frown ",
  "\[Cup]" -> "\\smile ",
  "\[Union]" -> "\\cup ",
  "\[UnionPlus]" -> "\\uplus ",
  "\[Intersection]" -> "\\cap ",
  "\[SquareIntersection]" -> "\\sqcap ",
  "\[SquareUnion]" -> "\\sqcup ",
  "\[Coproduct]" -> "\\coprod ",
  "\[Square]" -> "\\square ",
  "\[Colon]" -> ":"
}
(* Relation Symbols *)
$RelationSymbols = {
  "\[NotEqual]" -> "\\neq ",
  "\[Equal]" -> "==",
  "\[Congruent]" -> "\\equiv ",
  "\[NotCongruent]" -> "\\not{\\equiv}",
  "\[NotTildeEqual]" -> "\\not{\\simeq}",
  "\[TildeEqual]" -> "\\simeq ",
  "\[TildeFullEqual]" -> "\\cong ",
  "\[NotTildeFullEqual]" -> "\\ncong ",
  "\[NotLess]" -> "\\nless ",
  "\[RawLess]" -> "<",
  "\[RawGreater]" -> ">",
  "\[NotGreater]" -> "\\ngtr ",
  "\[NotLessSlantEqual]" -> "\\nleqslant ",
  "\[LessSlantEqual]" -> "\\leqslant ",
  "\[GreaterSlantEqual]" -> "\\geqslant ",
  "\[NotGreaterSlantEqual]" -> "\\ngeqslant ",
  "\[NotLessEqual]" -> "\\nleq ",
  "\[LessEqual]" -> "\\leq ",
  "\[GreaterEqual]" -> "\\geq ",
  "\[NotGreaterEqual]" -> "\\ngeq ",
  "\[NotLessFullEqual]" -> "\\nleqq ",
  "\[LessFullEqual]" -> "\\leqq ",
  "\[GreaterFullEqual]" -> "\\geqq ",
  "\[NotGreaterFullEqual]" -> "\\ngeqq ",
  "\[NotPrecedes]" -> "\\nprec ",
  "\[Precedes]" -> "\\prec ",
  "\[Succeeds]" -> "\\succ ",
  "\[NotSucceeds]" -> "\\nsucc ",
  "\[NotPrecedesSlantEqual]" -> "\\not{\\precurlyeq}",
  "\[PrecedesSlantEqual]" -> "\\precurlyeq ",
  "\[SucceedsSlantEqual]" -> "\\succurlyeq ",
  "\[NotSucceedsSlantEqual]" -> "\\not{\\succurlyeq}",
  "\[NotPrecedesEqual]" -> "\\npreceq ",
  "\[PrecedesEqual]" -> "\\preceq ",
  "\[SucceedsEqual]" -> "\\succeq ",
  "\[NotSucceedsEqual]" -> "\\nsucceq ",
  "\[NotPrecedesTilde]" -> "\\not{\\precsim}",
  "\[PrecedesTilde]" -> "\\precsim ",
  "\[SucceedsTilde]" -> "\\succsim ",
  "\[NotSucceedsTilde]" -> "\\not{\\succsim}",
  "\[NotSubset]" -> "\\not{\\subset}",
  "\[Subset]" -> "\\subset ",
  "\[Superset]" -> "\\supset ",
  "\[NotSuperset]" -> "\\not{\\supset}",
  "\[NotSquareSubset]" -> "\\not{\\sqsubset}",
  "\[SquareSubset]" -> "\\sqsubset ",
  "\[SquareSuperset]" -> "\\sqsupset ",
  "\[NotSquareSuperset]" -> "\\not{\\sqsupset}",
  "\[NotElement]" -> "\\notin ",
  "\[Element]" -> "\\in ",
  "\[ReverseElement]" -> "\\ni ",
  "\[NotReverseElement]" -> "\\not{\\ni}",
  "\[DotEqual]" -> "\\doteq ",
  "\[LongEqual]" -> "=", (* ? *)
  "\[Proportional]" -> "\\propto ", (* ?\\varpropto *)
  "\[Proportion]" -> "::", (* ? *)
  "\[NotTilde]" -> "\\not{\\sim}",
  "\[Tilde]" -> "\\sim ",
  "\[TildeTilde]" -> "\\approx ",
  "\[NotTildeTilde]" -> "\\not{\\approx}",
  "\[NotHumpEqual]" -> "\\not{\\bumpeq}",
  "\[HumpEqual]" -> "\\bumpeq ",
  "\[HumpDownHump]" -> "\\Bumpeq ",
  "\[NotHumpDownHump]" -> "\\not{\\Bumpeq}",
  "\[NotLessLess]" -> "\\not{\\ll}",
  "\[LessLess]" -> "\\ll ",
  "\[GreaterGreater]" -> "\\gg ",
  "\[NotGreaterGreater]" -> "\\not{\\gg}",
  "\[NotNestedLessLess]" -> "\\not{\\ll}",
  "\[NestedLessLess]" -> "\\ll ",
  "\[NestedGreaterGreater]" -> "\\gg ",
  "\[NotNestedGreaterGreater]" -> "\\not{\\gg}",
  "\[NotLessGreater]" -> "\\not{\\lessgtr}",
  "\[LessGreater]" -> "\\lessgtr ",
  "\[GreaterLess]" -> "\\gtrless ",
  "\[NotGreaterLess]" -> "\\not{\\gtrless}",
  "\[NotLessTilde]" -> "\\not{\\lesssim}",
  "\[LessTilde]" -> "\\lesssim ",
  "\[GreaterTilde]" -> "\\gtrsin ",
  "\[NotGreaterTilde]" -> "\\not{\\gtrsim}",
  "\[NotLeftTriangle]" -> "\\ntriangleleft ",
  "\[LeftTriangle]" -> "\\triangleleft ",
  "\[RightTriangle]" -> "\\triangleright ",
  "\[NotRightTriangle]" -> "\\ntriangleright ",
  "\[NotLeftTriangleBar]" -> "\\ntriangleleft |",
  "\[LeftTriangleBar]" -> "\\triangleleft |",
  "\[RightTriangleBar]" -> "|\\triangleright ",
  "\[NotRightTriangleBar]" -> "|\\ntriangleright ",
  "\[NotLeftTriangleEqual]" -> "\\ntrianglelefteq ",
  "\[LeftTriangleEqual]" -> "\\trianglelefteq ",
  "\[RightTriangleEqual]" -> "\\trianglerighteq ",
  "\[NotRightTriangleEqual]" -> "\\ntrianglerighteq ",
  "\[LessEqualGreater]" -> "\\lesseqgtr ",
  "\[GreaterEqualLess]" -> "\\gtreqless ",
  "\[NotSubsetEqual]" -> "\\nsubseteq ",
  "\[SubsetEqual]" -> "\\subseteq ",
  "\[SupersetEqual]" -> "\\supseteq ",
  "\[NotSupersetEqual]" -> "\\nsupseteq ",
  "\[NotSquareSubsetEqual]" -> "\\not{\\sqsubseteq}",
  "\[SquareSubsetEqual]" -> "\\sqsubseteq ",
  "\[SquareSupersetEqual]" -> "\\sqsupseteq ",
  "\[NotSquareSupersetEqual]" -> "\\not{\\sqsupseteq}",
  "\[NotVerticalBar]" -> "\\nmid ",
  "\[VerticalBar]" -> "\\mid ",
  "\[DoubleVerticalBar]" -> "\\parallel ",
  "\[NotDoubleVerticalBar]" -> "\\nparallel ",
  "\[Divides]" -> "|"
}
$Arrows = {
  "\[LeftArrow]" -> "\\leftarrow ",
  "\[RightArrow]" -> "\\rightarrow ",
  "\[UpArrow]" -> "\\uparrow ",
  "\[DownArrow]" -> "\\downarrow ",
  "\[LowerLeftArrow]" -> "\\swarrow ",
  "\[LowerRightArrow]" -> "\\searrow ",
  "\[UpperLeftArrow]" -> "\\nwarrow ",
  "\[UpperRightArrow]" -> "\\nearrow ",
  "\[ShortLeftArrow]" -> "\\leftarrow ",
  "\[ShortRightArrow]" -> "\\rightarrow ",
  "\[ShortUpArrow]" -> "\\uparrow ",
  "\[ShortDownArrow]" -> "\\downarrow ",
  "\[DoubleLeftArrow]" -> "\\Leftarrow ",
  "\[DoubleRightArrow]" -> "\\Rightarrow ",
  "\[DoubleUpArrow]" -> "\\Uparrow ",
  "\[DoubleDownArrow]" -> "\\Downarrow ",
  "\[LeftArrowRightArrow]" -> "\\leftrightarrows ",
  "\[RightArrowLeftArrow]" -> "\\rightleftarrows ",
  "\[DownArrowUpArrow]" -> "\\downarrow \\uparrow ",
  "\[UpArrowDownArrow]" -> "\\uparrow \\downarrow ",
  "\[LeftRightArrow]" -> "\\leftrightarrow ",
  "\[DoubleLeftRightArrow]" -> "\\Leftrightarrow ",
  "\[UpDownArrow]" -> "\\updownarrow ",
  "\[DoubleUpDownArrow]" -> "\\Updownarrow ",
  "\[LongLeftRightArrow]" -> "\\longleftrightarrow ",
  "\[DoubleLongLeftRightArrow]" -> "\\Longleftrightarrow ",
  "\[LongLeftArrow]" -> "\\longleftarrow ",
  "\[DoubleLongLeftArrow]" -> "\\Longleftarrow ",
  "\[LongRightArrow]" -> "\\longrightarrow ",
  "\[DoubleLongRightArrow]" -> "\\Longrightarrow ",
  "\[LeftVector]" -> "\\leftharpoonup ",
  "\[RightVector]" -> "\\rightharpoonup ",
  "\[LeftUpVector]" -> "\\upharpoonleft ",
  "\[LeftDownVector]" -> "\\downharpoonleft ",
  "\[DownLeftVector]" -> "\\leftharpoondown ",
  "\[DownRightVector]" -> "\\rightharpoondown ",
  "\[RightDownVector]" -> "\\downharpoonright ",
  "\[RightUpVector]" -> "\\upharpoonright ",
  "\[LeftRightVector]" -> "\\leftharpoonup \\rightharpoonup ", (* ? *)
  "\[LeftUpDownVector]" -> "\\overset{\\upharpooneleft}{\\downharpoonleft}", (* ? *)
  "\[DownLeftRightVector]" -> "\\leftharpoondown \\rightharpoondown ", (* ? *)
  "\[RightUpDownVector]" -> "\\overset{\\upharpooneright}{\\downharpoonright}", (* ? *)
  "\[LeftArrowBar]" -> "|\\leftarrow ",
  "\[RightArrowBar]" -> "\\rightarrow |",
  "\[UpArrowBar]" -> "\\bar{\\uparrow}",
  "\[DownArrowBar]" -> "\\underline{\\downarrow}",
  "\[LeftVectorBar]" -> "|\\leftharpoonup ",
  "\[RightVectorBar]" -> "\\rightharpoonup |",
  "\[LeftUpVectorBar]" -> "\\bar{\\upharpoonleft}",
  "\[LeftDownVectorBar]" -> "\\underline{\\downharpoonleft}",
  "\[DownLeftVectorBar]" -> "|\\leftharpoondown ",
  "\[DownRightVectorBar]" -> "\\rightharpoondown |",
  "\[RightUpVectorBar]" -> "\\bar{\\upharpoonright}",
  "\[RightDownVectorBar]" -> "\\underline{\\downharpoonright}",
  "\[ReverseEquilibrium]" -> "\\leftrightharpoons ",
  "\[Equilibrium]" -> "\\rightleftharpoons ",
  "\[UpEquilibrium]" -> "\\upharpoon \\downharpoon ",
  "\[ReverseUpEquilibrium]" -> "\\downharpoon \\upharpoon ",
  "\[LeftTeeArrow]" -> "\\leftarrow |",
  "\[RightTeeArrow]" -> "|\\rightarrow ",
  "\[UpTeeArrow]" -> "\\underline{\\uparrow}",
  "\[DownTeeArrow]" -> "\\bar{\\downarrow}",
  "\[LeftTeeVector]" -> "\\leftharpoonup |",
  "\[RightTeeVector]" -> "|\\rightharpoonup ",
  "\[LeftUpTeeVector]" -> "\\underline{\\upharpoonleft}",
  "\[LeftDownTeeVector]" -> "\\bar{\\downharpoonleft}",
  "\[DownLeftTeeVector]" -> "\\leftharpoondown |",
  "\[DownRightTeeVector]" -> "|\\rightharpoondown ",
  "\[RightUpTeeVector]" -> "\\underline{\\upharpoonright}",
  "\[RightDownTeeVector]" -> "\\bar{\\doenharpoonright}",
  "\[DoubleRightTee]" -> "\\vDash ", (* ?\\models *)
  "\[DoubleLeftTee]" -> "=|", (* ? *)
  "\[LeftTee]" -> "\\dashv ",
  "\[RightTee]" -> "\\vdash ",
  "\[UpTee]" -> "\\bot ", (* ? \\ perp *)
  "\[DownTee]" -> "\\top "
}
$Spaces = {
  "\[NonBreakingSpace]" -> "", (* ? *)
  "\[ThickSpace]" -> "\\thickspace ",
  "\[ThinSpace]" -> "\\, ",
  "\[VeryThinSpace]" -> "\\, ",
  "\[LineSeparator]" -> "", (* ? *)
  "\[ParagraphSeparator]" -> "", (* ? *)
  "\[MediumSpace]" -> "\\medspace ",
  "\[InvisibleSpace]" -> "",
  "\[NegativeVeryThinSpace]" -> "@! ",
  "\[NegativeThinSpace]" -> "\\! ",
  "\[NegativeMediumSpace]" -> "\\nmedspace ",
  "\[NegativeThickSpace]" -> "\\nthickspace ",
  "\[IndentingNewLine]" -> "", (* ? *)
  "\n" -> "", (* ? *)
  "\[AutoSpace]" -> " ",
  "\[InvisiblePrefixScriptBase]" -> "\\, ", (* ? *)
  "\[InvisiblePostfixScriptBase]" -> "\ ", (* ? *)
  "\[AlignmentMarker]" -> "", (* ? *)
  "\[InvisibleApplication]" -> "",
  "\[InvisibleComma]" -> "",
  "\[NoBreak]" -> "",
  "\[Null]" -> "",
  "\[AutoLeftMatch]" -> "",
  "\[AutoRightMatch]" -> "",
  "\[AutoOperand]" -> "",
  "\[SpanFromBoth]" -> "",
  "\[SpanFromLeft]" -> "",
  "\[SpanFromAbove]" -> "",
  "\t" -> "\\quad "
}
$Others = {
  "\[Conjugate]" -> "*",
  "\[ConjugateTranspose]" -> "\\dagger",
  "\[CupCap]" -> "\\overset{\\smile}{\\frown}", (* ? *)
  "\[NotCupCap]" -> "\\not{\\overset{\\smile}{\\frown}}", (* ? *)
  "\[EqualTilde]" -> "\\eqsim ",
  "\[NotEqualTilde]" -> "\\not{\\neqsim}",
  "\:02dc" -> "\\sim ",
  "\[FiLigature]" -> "fi",
  "\[FlLigature]" -> "fl",
  "\:00A8" -> "\\ddot{ }",
  "\:00AA" -> "\\underline{a}",
  "\:00AF" -> "\\overbar{ }",
  "\:00B4" -> "\\acute{ }",
  "\:00B8" -> "\\text{\\c }",
  "\:00BA" -> "\\underline{o}",
  "\:0152" -> "\\text{\\OE}",
  "\:0153" -> "\\text{\\oe}",
  "\:0178" -> "\\ddot{Y}",
  "\:02C6" -> "\\hat{ }",
  "\:02DC" -> "\\tilde{ }",
  "\:201A" -> ",",
  "\:201E" -> ",,",
  "\:2030" -> "0/00",
  "\:2039" -> "\\langle ",
  "\:203A" -> "\\rangle ",
  "\:2212" -> "-",
  "\[RawDoubleQuote]" -> "\\texttt{\"}", 
  "\"" -> "\\texttt{\"}",
  "`" -> "\\grave{ }",
  "\[RawAmpersand]" -> "\\&",
  "\[RawLess]" -> "<",
  "\[RawGreater]" -> ">",
  "\[Transpose]" -> "\\mathsf{T}"
}
$LeftTeXDelimiterReplacements =
    {
      "(" -> {"("},
      "[" -> {"["},
      "\[LeftModified]" -> {"["}, (* ? *)
      "\[LeftDoubleBracket]" -> {"[", "["},
      "{" -> {"\\{"},
      PW -> {"\\{"},
      "\[LeftFloor]" -> {"\\lfloor "},
      "\[LeftCeiling]" -> {"\\lceil "},
      "\[LeftAngleBracket]" -> {"\\langle "},
      "\[LeftSkeleton]" -> {"\\langle", "\\langle "},
      "\[LeftGuillemet]" -> {"\\langle", "\\langle "},
      "\[LeftBracketingBar]" -> {"| "},
      "\[LeftDoubleBracketingBar]" -> {"\\| "}
      }
$RightTeXDelimiterReplacements =
    {
      ")" -> {")"},
      "]" -> {"]"},
      "\[RightModified]" -> {"]"},
      "\[RightDoubleBracket]" -> {"]", "]"},
      "}" -> {"\\}"},
      "\[RightFloor]" -> {"\\rfloor "},
      "\[RightCeiling]" -> {"\\rceil "},
      "\[RightAngleBracket]" -> {"\\rangle "},
      "\[RightSkeleton]" -> {"\\rangle", "\\rangle "},
      "\[RightGuillemet]" -> {"\\rangle", "\\rangle "},
      "\[RightBracketingBar]" -> {"| "},
      "\[RightDoubleBracketingBar]" -> {"\\| "}
      }
$TeXDelimiterReplacements =
  Join[$LeftTeXDelimiterReplacements, $RightTeXDelimiterReplacements, {
      "/" -> {"/"},
      "\\" -> {"\\backslash "},
      "|" -> {"|"},
      "\[VerticalSeparator]" -> {"|"},
      "||" -> {"\\|"}
  }]
$BasicEscapes = {
"#" -> "\\#",
"$" -> "\\$",
"%" -> "\\%",
"&" -> "\\&",
"_" -> "\\_",
"~" -> "\\sim ", (* ? *)
"^" -> "{}^{\\wedge}" (* ? *)
}
$ASCIIUnchanged =
(# -> #)& /@ FromCharacterCode /@ Range[32, 126];

(*$TmaSymbols = {
	"Theorema`Language`Iff$TM" -> "\\unicode{29e6} ",
	"Theorema`Language`And$TM" -> "\\land ",
	"Theorema`Language`Forall$TM" -> "ForAll"
}

... Old idea here: makeBoxes takes care cleaning standard $TM suffixing etc., BUT we do need to consider
	user preferences like custom Iff for example, or new commands ("custom"). In both cases, Texformdump`$customTeXCommands
	should hold these, printing TeX-suffix TM for the template to handle, so, e.g., IffTM and customTM would be expected
*)

(* $TeXReplacements *)
$TeXReplacements = Join[
  $ASCIIUnchanged, $BasicEscapes, $TeXDelimiterReplacements,
  $GreekLetters, (*$GreekWords,*) $AccentedLetters, $Spaces,
  $CaligraphicLetters, $GothicLetters, $DoubleStruckLetters,
  $MiscellaneousSymbols, $Shapes, $TextualForms,
  $Operators, $RelationSymbols, $Arrows, $Others,
  If[MatchQ[$customTeXCommands, {_Rule...}], $customTeXCommands, {}]
  (* checks if $customTeXCommands is a non-empty list of rules,
  	which is necessary to avoid buggy behavior in the case it is empty *)
]
(* create maketex rules for each character *)
SetChar[char_->val_List] := (maketex[char] = StringJoin@val)
SetChar[char_->val_] :=     (maketex[char] = val)
SetChar[char_:>val_List] := (maketex[char] := StringJoin@val)
SetChar[char_:>val_] :=     (maketex[char] := val)

SetChar/@$TeXReplacements

(* if none of the above characters, convert to unicode *)
maketex[str_String/;(StringLength@str===1)] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[str_String/;(StringLength@str===1)]"];
  DebugPrint["str: ", str];
  Print[$customTeXCommands];
  Print[$Others];
  If[$Language === "Japanese" || MemberQ[{"ShiftJIS", "EUC"}, $CharacterEncoding],
     str,
     StringJoin["\\unicode{", ToCharacterHexCode@str, "}"]
  ]
)

(* Fixes bug(138232) and bug(144843) *)
(* Replaced LetterCharacter with [A-Za-z] to fix 292302 *)
maketex[str_String /; StringMatchQ[str, RegularExpression["\"[A-Za-z]\""]]] :=
(
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[str_String /; StringMatchQ[str, RegularExpression[\"\\[A-Za-z]\\\"]]]"];
	DebugPrint["str: ", str];
	Print[str];
	StringJoin["\\text{", StringReplace[str, RegularExpression["\"([A-Za-z])\""] :> "$1"], "}"]
)

(* This function has always assumed it is given a single "character" string *)
ToCharacterHexCode[str_String]:= With[{codepoint = First @ ToCharacterCode[str]},
  StringJoin[IntegerDigits[codepoint, 16, If[codepoint > 65535, 6, 4]]/.$UnicodeDigitsRules]
]

$UnicodeDigitsRules={0->"0", 1->"1", 2->"2", 3->"3", 4->"4",
   5->"5", 6->"6", 7->"7", 8->"8", 9->"9", 10->"a",
   11->"b", 12->"c", 13->"d", 14->"e", 15->"f"}

(* special case because some numbers are weird *)
maketex[str_String /; StringMatchQ[str, NumberString~~"`*^"~~(DigitCharacter..)]] :=
(
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[str_String /; StringMatchQ[str, NumberString~~\"`*^\"~~(DigitCharacter..)]]"];
	DebugPrint["str: ", str];
	ToNumberForm@str
)

maketex[str_String /; StringMatchQ[str, NumberString~~"`"]] :=
(
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[str_String /; StringMatchQ[str, NumberString~~\"`\"]]"];
	DebugPrint["str: ", str];
	ToNumberForm@str
)

maketex[str_String /; StringMatchQ[str, NumberString~~"`"~~NumberString]] :=
(
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[str_String /; StringMatchQ[str, NumberString~~\"`\"~~NumberString]]"];
	DebugPrint["str: ", str];
	ToNumberForm@str
)

ToNumberForm[str_] :=
Convert`TeX`BoxesToTeX[
  ToBoxes[NumberForm[ToExpression[str], NumberPadding->{"", ""}], TraditionalForm]
]

(* multi character string *)
(*maketex[str_String] :=
(
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[str_String]"];
	DebugPrint["str: ", str];
	If[$ShowQuotes, 
		makestring@str, 
		makestring@StringReplace[str, {"\\\""->"\"", "\""->""}]
	]
)*)

(*multi character string*)
maketex[str_String] := (DebugPrint[
   "------------------------------------"];
  DebugPrint["maketex[str_String]"];
  DebugPrint["str: ", str];
  If[$ShowQuotes, makestring@str, 
   makestring@
    StringReplace[str, {"\\\"" -> "\"", "\"" -> ""}]])

$ShowQuotes = False

makestring[str_String] :=
Module[{char},
 Which[
  str===" ",						 " ", (* different char? *)
  str==="\\ ", 						str,
  StringMatchQ[str, NumberString],	str, (* fixes formatting of some types of numbers, such as ScientificForm[10.`20^-5, 3]; bug(144843) *)
  StringLength@str===1, 			MakeTeX[str],
  True, 							char = MapCharacters[str];
								    If[MatchQ[char, {{"$", ___, "$"}}],
										StringJoin[Take[First@char, {2,-2}]],
										CheckTeX@StringJoin["\\text{", char, "}"]
								    ]
 ]
]


TeXTextString[str_String] :=
Module[{new, tmp},
  tmp = str;
  tmp = StringReplace[tmp, "\"" ~~ txt : Shortest[___] ~~ "\"" :> "\[OpenCurlyDoubleQuote]" ~~ txt ~~ "\[CloseCurlyDoubleQuote]"]; 
  tmp = StringReplace[tmp, "\[RawDoubleQuote]" ~~ txt : Shortest[___] ~~ "\[RawDoubleQuote]" :> "\[OpenCurlyDoubleQuote]" ~~ txt ~~ "\[CloseCurlyDoubleQuote]"]; 
  new = Map[TeXCharacters, Characters@tmp];
  new = TextExceptions/@new;
  (* remove adjent $s *)
  new = new //. {a___, {"$", b__, "$"}, {"$", c__, "$"}, d___} :>
    {a, {"$", b, c, "$"}, d};
  StringJoin@new
]

TextExceptions[{"$", str_, "$"}] :=
Module[{new},
  new = StringReplace[str,
    StringExpression["\\", macro:$Accent, "{", base:__, "}"] :>
    StringJoin["\\", macro/.accentrules, "{", base/.baserules, "}"]
  ];
  If[new===str, {"$", str, "$"}, new]
]

$Accent = "acute"|"bar"|"breve"|"check"|"ddot"|"dot"|"grave"|"hat"|"tilde"|"vec"
accentrules = {"acute"->"'", "bar"->"=", "breve"->"u", "check"->"v",
 "ddot"->"\"", "dot"->".", "grave"->"`", "hat"->"^", "tilde"->"~"}
baserules = {"\\text{\\i}" -> "\\i", "\\text{\\j}" -> "\\j"}

TextExceptions[else_] := else

MapCharacters[str_]:=
Module[{new, tmp},
  tmp = str;
  tmp = StringReplace[tmp, "\"" ~~ txt : Shortest[___] ~~ "\"" :> "\[OpenCurlyDoubleQuote]" ~~ txt ~~ "\[CloseCurlyDoubleQuote]"]; 
  tmp = StringReplace[tmp, "\[RawDoubleQuote]" ~~ txt : Shortest[___] ~~ "\[RawDoubleQuote]" :> "\[OpenCurlyDoubleQuote]" ~~ txt ~~ "\[CloseCurlyDoubleQuote]"]; 
  new = Map[TeXCharacters, Characters@tmp];
  (* remove adjent $s *)
  new = new //. {a___, {"$", b__, "$"}, {"$", c__, "$"}, d___} :>
    {a, {"$", b, c, "$"}, d}
]

TeXCharacters["\n"|"\[IndentingNewLine]"] := "\n"

TeXCharacters[char_] :=
Module[{new},
  new = MakeTeX[char];
  Which[
    StringMatchQ[new, "<"|">"|"|"], (* ensure special characters are in math mode *)
      {"$", new, "$"},
    StringLength@new===1, (* single char should always work in text mode *)
      new,
    StringMatchQ[new, "\\text{*}"], (* no need to double escape *)
      StringDrop[new, 5], (* keep {} to keep macros separate *)
    new==="",
      "",
    True, (* ensure special characters are in math mode *)
      {"$", new, "$"}
  ]
]

(* RowBox *)

(* log-like functions *)
LogLikeFunction[str_String] :=
  (maketex[str] := "\\"<>str<>" ")

LogLikeFunction/@{
 "arccos", "arcsin", "arctan", "arg", "cos", "cosh", "cot",
 "coth", "csc", "deg", "det", "dim", "exp", "gcd", "hom", "inf",
 "injlim", "ker", "lg", "lim", "liminf", "limsup",
 "ln", "log", "max", "min", "Pr", "projlim", "sec",
 "sin", "sinh", "sup", "tan", "tanh"
}

(* other special log-like cases: Im, Re, Arg, Max, Min and Mod *)

maketex[RowBox[{"Im", "(", rest___, ")"}]] := (
    DebugPrint["------------------------------------"];
    DebugPrint["maketex[RowBox[{\"Im\", \"(\", rest___, \")\"}]]"];
    DebugPrint["rest: ", rest];
    StringJoin[CheckTeX@"\\Im", MakeTeX@RowBox@{"(", rest, ")"}]
)

maketex[RowBox[{"Re", "(", rest___, ")"}]] := (
    DebugPrint["------------------------------------"];
    DebugPrint["maketex[RowBox[{\"Re\", \"(\", rest___, \")\"}]]"];
    DebugPrint["rest: ", rest];
    StringJoin[CheckTeX@"\\Re", MakeTeX@RowBox@{"(", rest, ")"}]
)

maketex[RowBox[{"Arg", "(", rest___, ")"}]] := (
    DebugPrint["------------------------------------"];
    DebugPrint["maketex[RowBox[{\"Arg\", \"(\", rest___, \")\"}]]"];
    DebugPrint["rest: ", rest];
    StringJoin[CheckTeX@"\\arg", MakeTeX@RowBox@{"(", rest, ")"}]
)

maketex[RowBox[{"Max", "(", rest___, ")"}]] := (
    DebugPrint["------------------------------------"];
    DebugPrint["maketex[RowBox[{\"Max\", \"(\", rest___, \")\"}]]"];
    DebugPrint["rest: ", rest];
    StringJoin[CheckTeX@"\\max", MakeTeX@RowBox@{"(", rest, ")"}]
)

maketex[RowBox[{"Min", "(", rest___, ")"}]] := (
    DebugPrint["------------------------------------"];
    DebugPrint["maketex[RowBox[{\"Min\", \"(\", rest___, \")\"}]]"];
    DebugPrint["rest: ", rest];
    StringJoin[CheckTeX@"\\min", MakeTeX@RowBox@{"(", rest, ")"}]
)

maketex[RowBox[{l__, " ", "mod", " ", r__}]] := (
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[RowBox[{l__, \" \", \"mod\", \" \", r__}]]"];
  DebugPrint["l: ", l];
  DebugPrint["r: ", r];
  StringJoin["(", MakeTeX@RowBox@{l}, " ", "\\bmod", " ", MakeTeX@RowBox@{r}, ")"]
)

(* \left and \right delimiters *)

MapTeX[stuff_List] := Map[MakeTeX, stuff]
MapTeX[stuff___] := MapTeX[{stuff}]

DelimiterBoxQ[boxes_List] :=
  !FreeQ[boxes, GridBox|SuperscriptBox|SubscriptBox|SubsuperscriptBox|
    OverscriptBox|UnderscriptBox|UnderoverscriptBox|FractionBox|
    SqrtBox|RadicalBox|FrameBox|ButtonBox|PaneBox|PanelBox
  ]
DelimiterBoxQ[boxes___] := DelimiterBoxQ[{boxes}]

DelimiterPattern = Apply[Alternatives, First /@ $TeXDelimiterReplacements]
LeftDelimiter = Apply[Alternatives, First /@ $LeftTeXDelimiterReplacements]
RightDelimiter = Apply[Alternatives, First /@ $RightTeXDelimiterReplacements]

InsertDelimiters[dir_, delimiter_] :=
Module[{lst},
  lst = delimiter /. $TeXDelimiterReplacements;
  Map[{"\\", dir, #}&, lst]
]

OppositeDelimiters[dir_, delimiter_] :=
Module[{lst},
  lst = delimiter /. $TeXDelimiterReplacements;
  Table[{"\\", dir, "."}, {Length@lst}]
]

maketex[RowBox[{l___, lb:DelimiterPattern, mid___, rb:DelimiterPattern, r___}]] :=
Module[{delimQ},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[RowBox[{l___, lb:DelimiterPattern, mid___, rb:DelimiterPattern, r___}]]"];
  DebugPrint["l: ", l];
  DebugPrint["lb: ", lb];
  DebugPrint["mid: ", mid];
  DebugPrint["rb: ", rb];
  DebugPrint["r: ", r];
  delimQ = DelimiterBoxQ[mid];
  StringJoin[
    MapTeX[l],
    If[delimQ, InsertDelimiters["left", lb], MakeTeX[lb]],
    MapTeX[mid],
    If[delimQ, InsertDelimiters["right", rb], MakeTeX[rb]],
    MapTeX[r]
 ]
]

maketex[RowBox[{lb:DelimiterPattern, mid___}]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[RowBox[{lb:DelimiterPattern, mid___}]]"];
  DebugPrint["lb: ", lb];
  DebugPrint["mid: ", mid];
StringJoin[
  InsertDelimiters["left", lb],
  MapTeX[mid],
  OppositeDelimiters["right", lb]
]) /; DelimiterBoxQ[mid]

maketex[RowBox[{mid___, rb:DelimiterPattern}]] := (
DebugPrint["------------------------------------"];
DebugPrint["maketex[RowBox[{mid___, rb:DelimiterPattern}]]"];
DebugPrint["mid: ", mid];
DebugPrint["rb: ", rb];
StringJoin[
  OppositeDelimiters["left", rb],
  MapTeX[mid],
  InsertDelimiters["right", rb]
]) /; DelimiterBoxQ[mid]

maketex[RowBox[{l___, lb:LeftDelimiter, mid___}]] := (
DebugPrint["------------------------------------"];
DebugPrint["maketex[RowBox[{l___, lb:LeftDelimiter, mid___}]]"];
DebugPrint["l: ", l];
DebugPrint["lb: ", lb];
DebugPrint["mid: ", mid];
StringJoin[
  MapTeX[l],
  InsertDelimiters["left", lb],
  MapTeX[mid],
  OppositeDelimiters["right", lb]
]) /; DelimiterBoxQ[mid]

maketex[RowBox[{mid___, rb:RightDelimiter, r___}]] := (
DebugPrint["------------------------------------"];
DebugPrint["maketex[RowBox[{l___, lb:LeftDelimiter, mid___}]]"];
DebugPrint["rb: ", rb];
DebugPrint["mid: ", mid];
DebugPrint["r: ", r];
StringJoin[
  OppositeDelimiters["left", rb],
  MapTeX[mid],
  InsertDelimiters["right", rb],
  MapTeX[r]
]) /; DelimiterBoxQ[mid]

maketex[RowBox[{l___, lb:DelimiterPattern, mid___}]] := (
DebugPrint["------------------------------------"];
DebugPrint["maketex[RowBox[{l___, lb:DelimiterPattern, mid___}]]"];
DebugPrint["l: ", l];
DebugPrint["lb: ", lb];
DebugPrint["mid: ", mid];
StringJoin[
  MapTeX[l],
  InsertDelimiters["left", lb],
  MapTeX[mid],
  OppositeDelimiters["right", lb]
]) /; (DelimiterBoxQ[mid] && !DelimiterBoxQ[l])

maketex[RowBox[{mid___, rb:DelimiterPattern, r___}]] := (
DebugPrint["------------------------------------"];
DebugPrint["maketex[RowBox[{mid___, rb:DelimiterPattern, r___}]]"];
DebugPrint["mid: ", mid];
DebugPrint["rb: ", rb];
DebugPrint["r: ", r];
StringJoin[
  OppositeDelimiters["left", rb],
  MapTeX[mid],
  InsertDelimiters["right", rb],
  MapTeX[r]
]) /; (DelimiterBoxQ[mid] && !DelimiterBoxQ[r])

(* comments *)
maketex[RowBox[{"(*", c1___String, RowBox[{c2___String}], c3___String, "*)"}]] := (
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[RowBox[{\"(*\", c1___String, RowBox[{c2___String}], c3___String, \"*)\"}]]"];
  DebugPrint["c1: ", c1];
  DebugPrint["c2: ", c2];
  DebugPrint["c3: ", c3];
  MakeTeX@StringJoin["(*", c1, c2, c3, "*)"]
)

maketex[RowBox[elem_List]] :=
Module[{new},
	new = fixWhitespace[elem];
	StringJoin@MapTeX[new]
]

(* Cases where Whitespace appears in a RowBox between two numbers *)

fixWhitespace[elem_List] :=
Module[{new, loclist},
	new = elem;
	(* Find all of the locations of " " in the top level of the list *)
	loclist = Flatten[Position[new, " ", 1]];
	If[Length[loclist] > 0,
		new = processWhitespace[new, loclist]
	];
	new
]

processWhitespace[elem_List, loc_List] :=
Module[{new, i, curr, prev, next},
	new = elem;
	(* Iterate through loc list *)
	For[i = 1, i <= Length[loc], i++,
		(* check if loc is within the acceptable range of values *)
		If[loc[[i]] > 1 && loc[[i]] < Length[new],
			curr = loc[[i]];
			prev = curr - 1;
			next = curr + 1;
			(* Check if previous element is a Number *)
			If[StringQ[new[[prev]]] && StringMatchQ[new[[prev]], NumberString],
				(* Test next element *)
				If[testNextLocation[new[[next]]],
					new = ReplacePart[new, curr -> "\\ "]
				]
			]
		]
	];
	new
]

testNextLocation[elem_String] :=
Module[{result = False},
	If[StringMatchQ[elem, NumberString],
		result = True
	];
	result
]

testNextLocation[RowBox[{}]] :=
Module[{result = False},
	result
]

(* This function handles all of the box structures we expect may be seen in this context, except TemplateBox which we don't handle yet. *)
testNextLocation[(RowBox|SuperscriptBox|SubscriptBox|OverscriptBox|UnderscriptBox|SubsuperscriptBox|UnderoverscriptBox|
	StyleBox|AdjustmentBox|InterpretationBox|TagBox|FormBox|Cell|BoxData|TextData|TooltipBox|ValueBox|OptionValueBox)[elem_, ___]] :=
Module[{result = False},
	result = testNextLocation[elem];
	result
]

testNextLocation[elem_] :=
Module[{result = False},
	result
]

(* Scripts *)

MakeScript[in_] :=
Module[{script},
  script = MakeTeX@in;
  If[StringLength@script=!=1,
    StringJoin["{", script, "}"],
    script
  ]
]

maketex[SuperscriptBox[base_, super_, ___]] :=
Module[{b},
  b = MakeTeX[base];
  If[!StringFreeQ[b, "_"], b = b<>"{}"];
  StringJoin[b, "^", MakeScript@super]
]

maketex[SuperscriptBox[base_, super_String, ___]] :=
  StringJoin[MakeTeX[base], Table["'", {StringLength@super}]] /;
    Complement[Union@Characters@super, {"'", "\[Prime]"}]==={}

maketex[SubscriptBox[base_, sub_, ___]] :=
Module[{b},
  b = MakeTeX[base];
  If[!StringFreeQ[b, "^"], b = b<>"{}"];
  StringJoin[b, "_", MakeScript@sub]
]

(* it's possible that GetLinebreakInformationPacket can split a superscript *)
maketex[SuperscriptBox[base_, ___?OptionQ]] :=
  MakeTeX[base]

maketex[SubsuperscriptBox[base_, sub_, super_, ___]] :=
  StringJoin[MakeTeX[base], "_", MakeScript@sub, "^", MakeScript@super]

(* it's possible that GetLinebreakInformationPacket can split a superscript *)
maketex[SubsuperscriptBox[base_, ___?OptionQ]] :=
  MakeTeX[base]

maketex[OverscriptBox[base_, over_, ___]] :=
StringJoin[
  "\\overset{", MakeTeX[over], "}{", MakeTeX[base], "}"
]

maketex[UnderscriptBox[base_, under_, ___]] :=
StringJoin[
  CheckTeX["\\underset{"], MakeTeX[under], "}{", MakeTeX[base], "}"
]

maketex[UnderoverscriptBox[base_, under_, over_, ___]] :=
  MakeTeX[UnderscriptBox[OverscriptBox[base, over], under]]

maketex[SqrtBox[x_, ___]] :=
  StringJoin["\\sqrt{", MakeTeX[x], "}"]

maketex[RadicalBox[x_, y_, ___]] :=
StringJoin[
  "\\sqrt[", MakeTeX[y], "]{", MakeTeX[x], "}"
]

maketex[FractionBox[num_, den_, ___]] :=
StringJoin[
  "\\frac{", MakeTeX[num], "}{", MakeTeX[den], "}"
]

(* special case over/underscripts *)

maketex[OverscriptBox[base_String/;StringLength[base]==1, "_", ___]] :=
  StringJoin["\\bar{", MakeTeX[base], "}"]
maketex[OverscriptBox[base_, "_"|"\[HorizontalLine]", ___]] :=
  StringJoin["\\overline{", MakeTeX[base], "}"]

maketex[UnderscriptBox[base_, "_"|"\[HorizontalLine]", ___]] :=
  StringJoin["\\underline{", MakeTeX[base], "}"]

maketex[(StyleBox|Cell)[str_, sty_String:"", opts__?OptionQ]] :=
Module[{fv, und, fw, fs, pre="", post="", mid},
  fv = FontVariations /. {opts} /. FontVariations->{};
  und = "Underline" /. fv /. "Underline" -> False;
  {fw, fs} = {FontWeight, FontSlant} /.{opts} /.
    {FontWeight|FontSlant -> "Plain"};
  mid = MakeTeX[StyleBox[str, sty]];
  If[fs==="Italic",
    mid = If[StringMatchQ[mid, "\\text{*}"],
      StringTake[mid, {7, -2}],
      "$"<>mid<>"$"
    ];
    pre = "\\text{\\textit{"<>pre;
    post = post<>"}}";
  ];
  If[und,
    pre = "\\underline{"<>pre;
    post = post<>"}";
  ];
  If[fw==="Bold",
    pre = "\\pmb{"<>pre;
    post = post<>"}";
  ];
  StringJoin[pre, mid, post]
]

larr = "\[LeftArrow]"|"\[LongLeftArrow]"|"\[ShortLeftArrow]"
rarr = "\[RightArrow]"|"\[LongRightArrow]"|"\[ShortRightArrow]"
lrarr = "\[LeftRightArrow]"|"\[LongLeftRightArrow]"

maketex[OverscriptBox[base_, larr, ___]] :=
  StringJoin["\\overleftarrow{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_String/;StringLength[base]==1, rarr, ___]] :=
  StringJoin["\\vec{", MakeTeX[base], "}"]
maketex[OverscriptBox[base_, rarr, ___]] :=
  StringJoin["\\overrightarrow{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, lrarr, ___]] :=
  StringJoin["\\overleftrightarrow{", MakeTeX[base], "}"]

maketex[UnderscriptBox[base_, lrarr, ___]] :=
  StringJoin["\\underleftarrow{", MakeTeX[base], "}"]

maketex[UnderscriptBox[base_, rarr, ___]] :=
  StringJoin["\\underrightarrow{", MakeTeX[base], "}"]

maketex[UnderscriptBox[base_, lrarr, ___]] :=
  StringJoin["\\underleftrightarrow{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "^", ___]] :=
  StringJoin["\\hat{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "\[Hacek]", ___]] :=
  StringJoin["\\check{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "~"|"\[Tilde]", ___]] :=
  StringJoin["\\tilde{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "'"|"\[Prime]", ___]] :=
  StringJoin["\\acute{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "`"|"\[ReversePrime]", ___]] :=
  StringJoin["\\grave{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "\[Breve]", ___]] :=
  StringJoin["\\breve{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "."|"\[Bullet]"|"\[CenterDot]", ___]] :=
  StringJoin["\\dot{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, ".."|"\[Bullet]\[Bullet]"|
    "\[CenterDot]\[CenterDot]"|"\[DoubleDot]", ___]] :=
  StringJoin["\\ddot{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "..."|"\[Bullet]\[Bullet]\[Bullet]"|
    "\[CenterDot]\[CenterDot]\[CenterDot]"|"\[TripleDot]"|
    "\[Ellipsis]"|"\[CenterEllipsis]", ___]] :=
  StringJoin["\\dddot{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "...."|"\[Bullet]\[Bullet]\[Bullet]\[Bullet]"|
    "\[CenterDot]\[CenterDot]\[CenterDot]\[CenterDot]"|
    "\[DoubleDot]\[DoubleDot]", ___]] :=
  StringJoin["\\ddddot{", MakeTeX[base], "}"]

maketex[OverscriptBox[base_, "\[OverBrace]", ___]] :=
  StringJoin["\\overbrace{", MakeTeX[base], "}"]
maketex[OverscriptBox[OverscriptBox[base_, "\[OverBrace]", ___], script_, ___]] :=
  StringJoin["\\overbrace{", MakeTeX[base], "}^", MakeScript[script]]
maketex[OverscriptBox[base_, OverscriptBox["\[OverBrace]", script_, ___], ___]] :=
  StringJoin["\\overbrace{", MakeTeX[base], "}^", MakeScript[script]]

maketex[UnderscriptBox[base_, "\[UnderBrace]", ___]] :=
  StringJoin["\\underbrace{", MakeTeX[base], "}"]
maketex[UnderscriptBox[UnderscriptBox[base_, "\[UnderBrace]", ___], script_, ___]] :=
  StringJoin["\\underbrace{", MakeTeX[base], "}_", MakeScript[script]]
maketex[UnderscriptBox[base_, UnderscriptBox["\[UnderBrace]", script_, ___], ___]] :=
  StringJoin["\\underbrace{", MakeTeX[base], "}_", MakeScript[script]]

maketex[UnderoverscriptBox[sp:"\[Sum]"|"\[Product]", under_, over_, ___]] :=
  StringJoin[MakeTeX@sp, "_", MakeScript@under, "^", MakeScript@over, " "]
maketex[UnderscriptBox[sp:"\[Sum]"|"\[Product]", under_, ___]] :=
  StringJoin[MakeTeX@sp, "_", MakeScript@under, " "]
maketex[OverscriptBox[sp:"\[Sum]"|"\[Product]", over_, ___]] :=
  StringJoin[MakeTeX@sp, "^", MakeScript@over, " "]

maketex[UnderscriptBox["lim", under_, ___]] :=
  StringJoin["\\lim_", MakeScript@under, " "]

maketex[RowBox[{"\[Integral]",
  RowBox[{integrand_, RowBox[{"\[DifferentialD]", var_}] }]}]
] :=
StringJoin["\\int ",
  MakeTeX@integrand, " \\, d", MakeTeX@var
]
maketex[RowBox[{SubsuperscriptBox["\[Integral]", sub_, super_, ___],
  RowBox[{integrand_, RowBox[{"\[DifferentialD]", var_}] }]}]
] :=
StringJoin["\\int_", MakeScript@sub, "^", MakeScript@super, " ",
  MakeTeX@integrand, " \\, d", MakeTeX@var
]

(* GridBox *)
(* spanning rows and cols? have to convert to equivalent normal grids. *)
maketex[GridBox[grid_, opts___?OptionQ]] :=
Module[{colaln, rowdivs, outstr, i, cols, rows},
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[GridBox[grid_, opts___?OptionQ]]"];
	DebugPrint["grid: ", grid];
	cols = Dimensions[grid][[2]];
	rows = Dimensions[grid][[1]];
 	colaln = processColumnOptions[{opts}, cols];
	rowdivs = processRowOptions[{opts}, rows];
	outstr = StringJoin["\n", "\\begin{array}{", colaln, "}", "\n"];
	For[i = 1, i <= rows, i++, 
		If[rowdivs[[i]] == True, outstr = StringJoin[outstr, "\\hline", "\n"] ];
		outstr = StringJoin[outstr, MakeRow[grid[[i]]]];
	];
	If[ Last[rowdivs]  == True,
		outstr = StringJoin[outstr, "\\hline", "\n"]
	];
	outstr = StringJoin[outstr, "\\end{array}", "\n"];
	outstr
]

processDividersOptions[{opts___}, s1_, s2_, num_]:=
Module[{rowopts, v1, v2, vals},
	rowopts = GridBoxDividers /. Flatten[{opts, Options[GridBox]}];
	If[rowopts == {}, Return[ConstantArray[False, {num+1}]] ];
	v1 = s1 /. rowopts;
	v2 = s2 /. rowopts;
	(* there's gotta be a better way *)
	If[StringQ[v1] && StringQ[v2], Return[ConstantArray[False, {num+1}]] ];

	If[ StringQ[v1],
		vals = Map[ If[MemberQ[First/@v2, #], !(MatchQ[(#/.v2),False|None]), False]&, Range[num+1] ];
		Return[vals/. {Automatic -> True, None -> False}];		
	];
	
	If[ StringQ[v2],
		If[ MatchQ[v1, {{_Symbol}}], v1 = ConstantArray[v1[[1,1]],{num+1}] ];
		If[ Length[v1] < num+1, v1 = Join[v1, ConstantArray[False, {num+1-Length[v1]}] ] ];
		vals = Map[ !MatchQ[#,False|None]&, v1 ];
		Return[vals/. {Automatic -> True, None -> False}];		
	];

	vals = If[ MemberQ[First/@v2, #], !(MatchQ[(#/.v2),False|None]),
		If[ MatchQ[v1, {{_Symbol}}],
			v1[[1,1]],
			If[ Length[v1] >= #,
				!MatchQ[ v1[[#]], False|None],
				False
			]
		]
	]&/@ Range[num+1];
	vals/. {Automatic -> True, None -> False}
]

processRowOptions[{opts___}, rows_] := processDividersOptions[{opts}, "Rows", "RowsIndexed", rows];

processColumnOptions[{opts___}, cls_] :=
Module[{alnopts, alnvals, divopts, divvals, colstr},
	(* Process GridBoxAlignment options to get a list of column alignment values *)
	alnopts = GridBoxAlignment /. Flatten[{opts, Options[GridBox]}];
	If[alnopts == {}, alnopts = {"Columns" -> {{Center}}, "Rows" -> {{Baseline}}}];
	alnopts = "Columns" /. alnopts;
	alnvals = getoptvals[alnopts, cls];
	(* Convert Automatic to Right *)
	alnvals = alnvals /. Automatic -> Right;
	(* Process GridBoxDividers options to get a list of column divider values *)
	divopts = GridBoxDividers /. Flatten[{opts, Options[GridBox]}];
	divvals = processDividersOptions[{opts}, "Columns", "ColumnsIndexed", cls];
	(* Process the column alignment list and the column divider list to produce a suitable string to return *)
	colstr = Flatten[Riffle[divvals, alnvals]];
	colstr = colstr /. {Center -> "c", Left -> "l", Right -> "r", True -> "|", False -> ""};
	colstr = cleanColumnOptions[colstr];
	colstr
]

cleanColumnOptions[colstr_List] :=
Module[{length, i, value, testlist, outlist},
	testlist = {"c", "l", "r", "|", ""};
	outlist = {};
	length = Length[colstr];
	i = 1;
	While[i <= length,
		value = colstr[[i]];
		If[MemberQ[testlist, value],
			outlist = Append[outlist, value],
			outlist = Append[outlist, "c"]
		];
		i = i + 1;
	];
	outlist
]

getoptvals[opts_, cls_] :=
Module[{vals, i},
	vals = {};
	For[i = 1, i <= cls, i++, vals = Append[vals, GetOptionValue[i, cls, opts]]];
	vals
]

GetOptionValue[i_, max_, opt_] :=
Module[{innerlstpos, lastseq, lastseqlen, innerlstlen, tmp}, 
	Switch[opt,
        {{v_}},	
        	opt[[1, 1]],
        {{v1_, v2_}},
        	If[OddQ@i, opt[[1, 1]], opt[[1, 2]]],
        {{v1_, v2_, v3__}},
        	opt[[1, Mod[i, Length[opt[[1]]]]]],
        {Except[_Rule].., {__Rule}},
        	tmp = i /. opt[[2]] /. {i -> None};
        	If[tmp === None, GetOptionValue[i, max, opt[[1]]], tmp],
        {v1_, {v2_}},
        	If[i == 1, opt[[1]], opt[[2, 1]]],
        {v1_, {v2_}, v3_},
        	Which[
        		i == 1, 	opt[[1]],
        		i == max, 	Last@opt,
        		True, 		opt[[2]]
        	],
        {v1__, {v2__}, v3__},
        	innerlstpos = First@Flatten@Position[opt, _List, 1];
        	innerlstlen = Length[opt[[innerlstpos]]];
        	lastseq = Position[opt, Except[_List], 1][[innerlstpos + 1 ;;]];
        	lastseqlen = Length[lastseq];
        	Which[
        		i < innerlstpos, 		opt[[i]],
        		i > max - lastseqlen, 	opt[[((max - i) + 1)*(-1)]],
        		True, 					opt[[innerlstpos, Mod[i, innerlstlen] + 1]]
        	],
        {v1__, {}, v3__},
        	innerlstpos = First@Flatten@Position[opt, {}, 1];
        	lastseq = Position[opt, Except[_List], 1][[innerlstpos + 1 ;;]];
        	lastseqlen = Length[lastseq];
        	Which[
        		i < innerlstpos, 		opt[[i]],
        		i > max - lastseqlen,	opt[[((max - i) + 1)*(-1)]],
        		True, 					""
        	],
        {__Rule},
        	i /. opt /. {i -> ""},
        {Except[_List] ..},
        	If[i > Length@opt, Last@opt, opt[[i]]],
        _,
        	tmp = If[ListQ@opt, opt, {opt}];
        	tmp = If[Length@tmp <= i, Last@tmp, tmp[[i]]];
        	tmp
	]
]

Trim[x_] := Drop[Flatten@x, -1]

MakeRow[row_] :=
  {" ", Trim@Map[MakeElement, row], " \\\\\n"}

MakeElement[el_] := {MakeTeX[el], " & "}

(* Misc Boxes *)

maketex[(h:PaneBox|PanelBox)[boxes_, opts___]] :=
Module[{frame, tex},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[" <> ToString[h] <> "[boxes_, opts___]]"];
  DebugPrint["boxes: ", boxes];
  frame = h === PanelBox;
  tex = MakeTeX[boxes];
  If[frame, StringJoin["\\fbox{$", tex, "$}"], tex]
]

maketex[FrameBox[boxes_, opts___]] :=
Module[{frame, tex},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[FrameBox[boxes_, opts___]]"];
  DebugPrint["boxes: ", boxes];
  frame = TrueQ[BoxFrame /. {opts} /. BoxFrame->True];
  tex = MakeTeX[boxes];
  If[frame, StringJoin["\\fbox{$", tex, "$}"], tex]
]

maketex[ErrorBox[boxes_, ___]] := (
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[ErrorBox[boxes_, ___]]"];
  DebugPrint["boxes: ", boxes];
  StringJoin["\\underline{", MakeTeX[boxes], "}"]
)

(*
CounterBox, ValueBox, and OptionValuebox should be resolved before being
passed to TeXSave (unless TeXSave is acting on a nb expression, not a
NotebookObject). Also there's a bug that ReleaseHold doesn't convert counters
that are inside deeply nested cells.
*)
maketex[CounterBox[style_, opts___?OptionQ]] :=
Module[{cf, format},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[CounterBox[style_, opts___?OptionQ]]"];
  DebugPrint["style: ", style];
  cf = CounterFunction /. {opts};
  format = Switch[cf,
    RomanNumeral, "roman",
    CapitalRomanNumeral, "Roman",
    (Part[CharacterRange["a", "z"], #]& ), "alph",
    (Part[CharacterRange["A", "Z"], #]& ), "Alph",
    _, "arabic"
  ];
  counter = Switch[style,
    "SectionFirst", "section",
    "NumberedEquation", "equation",
    _, ToLowerCase@style
  ];
  StringJoin["\\", format, "{mathematica", counter, "}"]
]

maketex[CounterBox[counter_, tag_, opts___?OptionQ]] := (
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[CounterBox[counter_, tag_, opts___?OptionQ]]"];
  DebugPrint["counter: ", counter];
  DebugPrint["tag: ", tag];
  StringJoin["\\ref{", tag, "}"]
)

maketex[CounterBox["Page", tag_, opts___?OptionQ]] := (
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[CounterBox[\"Page\", tag_, opts___?OptionQ]]"];
  DebugPrint["tag: ", tag];
  StringJoin["\\pageref{", tag, "}"]
)
(*
CounterBoxes require setting up counters in style sheet? Or at the beginning
of the file? Or maybe when first used? Some are automatically set up
already in the style sheet (like sections, subsections, figures, pages,
etc.). But many of them are not incremented because we use * styles.
*)
maketex[ValueBox[val_]] :=
Module[{value},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[ValueBox[val_]]"];
  DebugPrint["val: ", val];
  value = MathLink`CallFrontEnd[FrontEnd`Value[val]];
  MakeTeX[value]
] /; (MemberQ[$Values, val] && Developer`InstallFrontEnd[]=!=$Failed)

$Values =  {
 "$NotebookBaseDirectory",
 "$NotebookCreationDate",
 "$NotebookFullVersion",
 "$NotebookHomeDirectory",
 "$NotebookInitialDirectory",
 "$NotebookInput",
 "$NotebookInstallationDate",
 "$NotebookInstallationDirectory",
 "$NotebookLaunchDirectory",
 "$NotebookLicenseID",
 "$NotebookMachineID",
 "$NotebookMachineName",
 "$NotebookMachineType",
 "$NotebookMinorReleaseNumber",
 "$NotebookOperatingSystem",
 "$NotebookPasswordFile",
 "$NotebookProcessID",
 "$NotebookProcessorType",
 "$NotebookProductInformation",
 "$NotebookProgramName",
 "$NotebookReleaseNumber",
 "$NotebookSystem",
 "$NotebookSystemID",
 "$NotebookTopDirectory",
 "$NotebookUserBaseDirectory",
 "$NotebookUserDocumentsDirectory",
 "$NotebookUserName",
 "$NotebookVersion",
 "$NotebookVersionNumber",
 "$PreferencesDirectory",
 "$LicenseExpirationDate",
 "MemoryInUse",
 "FreeMemory",
 "ClipboardMemoryInUse",
 "Date",
 "DateLong",
 "Time",
 "Year",
 "YearShort",
 "Month",
 "MonthName",
 "MonthNameShort",
 "Day",
 "DayName",
 "DayNameShort",
 "Hour",
 "Minute",
 "Second"
}

maketex[OptionValueBox[val_]] :=
Block[{opt, value, $Messages = {}},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[OptionValueBox[val_]] "];
  DebugPrint["val: ", val];
  opt = ToExpression@val;
  Developer`UseFrontEnd@MakeTeX[MakeBoxes @@ {opt /. Options[$FrontEnd, opt]}] /;
    opt =!= $Failed
] /; (Developer`InstallFrontEnd[]=!=$Failed)
(* I don't think we can do 2 arg forms. *)

maketex[FormBox[boxes_, form_, ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[FormBox[boxes_, form_, ___]]"];
  DebugPrint["boxes: ", boxes];
  DebugPrint["form: ", form];
  MakeTeX[StyleBox[boxes, ToString@form]]
)


(* AdjustmentBox, InterpretationBox, TagBox are handled by StripIdentityBoxes *)

(* Special case for ButtonBox.  This it to handle output of Hyperlink[] function,
	which generates a ButtonBox of PaneSelectorBox, in a kernel-only session.
	I'm mimicking the behavior of HTMLConvert.m (as of 1.283), which also has a
	special case for handling output of Hyperlink. I hope that the pattern

		ButtonBox[PaneSelectorBox[...], BaseStyle->"Hyperlink"]

	is strict enough that will only really fit the output of Hyperlink[] function
	and nothing more. IgorA.  May/2009. *)
maketex[ButtonBox[PaneSelectorBox[contents_, paneSelectorOpts___],  buttonOpts___ /; MemberQ[{buttonOpts}, BaseStyle->"Hyperlink"]]] := (
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[ButtonBox[PaneSelectorBox[contents_, paneSelectorOpts___],  buttonOpts___ /; MemberQ[{buttonOpts}, BaseStyle->\"Hyperlink\"]]]"];
  DebugPrint["contents: ", contents];
  DebugPrint["paneSelectorOpts: ", paneSelectorOpts];
  MakeTeX[ButtonBox[False/.contents/.False->"link", buttonOpts]]
)

maketex[ButtonBox[boxes_, opts___?OptionQ]] :=
Module[{bsty, bframe, tex},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[ButtonBox[boxes_, opts___?OptionQ]]"];
  DebugPrint["boxes: ", boxes];
  {bsty, bframe}  = {BaseStyle, ButtonFrame} /. {opts} /.
    {BaseStyle -> "Paste", ButtonFrame -> "Palette"};
  tex = MakeTeX[boxes];
  (* add underline or frame *)
  Which[
   MatchQ[bsty, "Hyperlink" | "AddOnsLink" | "MainBookLink" |
   	"OtherInformationLink" | "GettingStartedLink" | "RefGuideLink" | "Link"],
     tex = StringJoin["\\underline{", tex, "}"],

   !MatchQ[bframe, None|"None"],
     tex = StringJoin["\\fbox{$", tex, "$}"]
  ];
  tex
]

maketex[(StyleBox|Cell)[boxes_, "Input"|"InputOnly"|"InlineInput", ___]] :=
Block[{$ShowQuotes=True},
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[(StyleBox|Cell)[boxes_, \"Input\"|\"InputOnly\"|\"InlineInput\", ___]]"];
  DebugPrint["boxes: ", boxes];
  StringJoin["\\pmb{", MakeTeX[boxes], "}"]
]

(* add rules for inline styles *)
maketex[(StyleBox|Cell)[boxes_, "IT"|"TI"|"MO"|"SB", ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[(StyleBox|Cell)[boxes_, \"IT\"|\"TI\"|\"MO\"|\"SB\", ___]]"];
  DebugPrint["boxes: ", boxes];
  MakeTeX[StyleBox[boxes, FontSlant->"Italic"]]
)

maketex[(StyleBox|Cell)[boxes_, "BF"|"TB"|"MB", ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[(StyleBox|Cell)[boxes_, \"BF\"|\"TB\"|\"MB\", ___]]"];
  DebugPrint["boxes: ", boxes];
  MakeTeX[StyleBox[boxes, FontWeight->"Bold"]]
)

maketex[(StyleBox|Cell)[boxes_, "TBI"|"MBO"|"SBO", ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[(StyleBox|Cell)[boxes_, \"TBI\"|\"MBO\"|\"SBO\", ___]]"];
  DebugPrint["boxes: ", boxes];
  MakeTeX[StyleBox[boxes, FontSlant->"Italic", FontWeight->"Bold"]]
)

(* general styles *)
maketex[(StyleBox|Cell)[boxes_, ___]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[(StyleBox|Cell)[boxes_, ___]]"];
  DebugPrint["boxes: ", boxes];
  MakeTeX[boxes]
)
(* Treat TextData like a RowBox in inline cells *)
maketex[TextData[text_]] :=
(
  DebugPrint["------------------------------------"];
  DebugPrint["maketex[TextData[text_]]"];
  DebugPrint["text: ", text];
  MakeTeX[RowBox@Flatten@{text}]
)

(******************************************************************************)
(* new v6 boxes                                                               *)
(******************************************************************************)

(* Other *)
maketex[expr:(_AnimatorBox | _SliderBox)] :=
	(* TODO:  Is there some sort of representation for AnimatorBox and SliderBox
		that we could use?  I don't know.  Need someone familiar with TeX to
		resolve this. *)
  (Message[TeXForm::unspt, expr]; "")

(* DynamicBox: handled by StripIdentityBoxes *)

(* CheckboxBox *)
maketex[CheckBox[Dynamic[expr_]]] := maketex[CheckBox[expr]]
maketex[CheckBox[True]] := MakeTeX["\[CheckmarkedBox]"]
maketex[CheckBox[False]] := MakeTeX["\[EmptySquare]"]

(* OpenerBox *)
maketex[OpenerBox[Dynamic[expr_]]] := maketex[OpenerBox[expr]]
maketex[OpenerBox[True]] := MakeTeX["\[FilledDownTriangle]"]
maketex[OpenerBox[False]] := MakeTeX["\[FilledRightTriangle]"]

(* RadioButtonBox *)
maketex[RadioButtonBox[Dynamic[expr_], val_]] := maketex[RadioButtonBox[expr, val]]
maketex[RadioButtonBox[val_, val_]] := MakeTeX["\[CircleDot]"]
maketex[RadioButtonBox[expr_, val_]] := MakeTeX["\[EmptyCircle]"]

(* TooltipBox *)
maketex[TooltipBox[boxes_, ___]] :=
  MakeTeX[boxes]

(* InputFieldBox *)
maketex[InputFieldBox[boxes_, opts___]] :=
Module[{frame, tex},
  frame = TrueQ[Frame /. {opts} /. Frame->True];
  tex = MakeTeX[boxes];
  If[frame, StringJoin["\\fbox{$", tex, "$}"], tex]
]

(* PopupMenuBox *)
maketex[PopupMenuBox[val_, {labels__?OptionQ}, ___]] :=
	MakeTeX[FrameBox[val /. {labels}]]

maketex[PopupMenuBox[val_, ___]] :=
	MakeTeX[FrameBox[ToBoxes[val]]]


(* PaneSelectorBox *)
maketex[PaneSelectorBox[rules_, Dynamic[expr_], opts___]] :=
	maketex[PaneSelectorBox[rules, expr, opts]]

maketex[PaneSelectorBox[{rules__?OptionQ}, val_, opts___]] :=
	MakeTeX[val/.{rules}]

(* End of new V6 boxes *)

(******************************************************************************)
(* new V7 boxes                                                               *)
(******************************************************************************)

(* TemplateBox *)

(*	We need to intercept TemplateBox for Binomial before using default maketex[]
 *	function for handling TemplateBox because BoxForm`TemplateBoxToDisplayBoxes will
 *	generate regular boxes for Binomial, which won't be distinguishable from matrix
 *)
maketex[TemplateBox[{n_,k_}, "Binomial", ___]] :=
	StringJoin["\\binom{", MakeTeX[n], "}{", MakeTeX[k], "}"]

(*  Handle QBinomial like we do Binmial above. *)
maketex[TemplateBox[{n_, m_, q_}, "QBinomial", ___]] :=
    StringJoin["\\binom{", MakeTeX[n], "}{", MakeTeX[m], "}_", MakeScript[q]]

(*  We need to intercept TemplateBox for Superscript before using default maketex[]
	function just like with the Binomial TemplateBox above. Addresses Bug 120130 *)
maketex[TemplateBox[{base_, super_}, "Superscript", ___]] :=
Module[{b},
  b = MakeTeX[base];
  If[!StringFreeQ[b, "_"], b = b<>"{}"];
  StringJoin[b, "^", MakeScript@super]
]

maketex[TemplateBox[{base_, super_String}, "Superscript", ___]] :=
  StringJoin[MakeTeX[base], Table["'", {StringLength@super}]] /;
    Complement[Union@Characters@super, {"'", "\[Prime]"}]==={}

(*	TODO:  Are there more box expressions that need to be converted to TeX
	in a special way, like TemplateBox[..., "Binomial"]? *)

(*	Default maketex for TemplateBoxes *)
maketex[tbox_TemplateBox, opts___] :=
(
	DebugPrint["------------------------------------"];
	DebugPrint["maketex[tbox_TemplateBox, opts___]"];
	DebugPrint["tbox: ", tbox];
	(* We need to call StripIdentityBoxes again because the stylesheet made add
	 * boxes that normally would have been stripped prior to calling MakeTeX. *)
	MakeTeX[StripIdentityBoxes[BoxForm`TemplateBoxToDisplayBoxes[tbox]], opts]
)

(* CheckboxBox *)
maketex[System`TemplateArgBox[expr_]] := MakeTeX[expr]; 
maketex[System`TemplateArgBox[lst_List]] := MakeTeX[RowBox@lst]

(******************************************************************************)
(* End of new V7 boxes                                                        *)
(******************************************************************************)

(* raw lists are sometimes produced by GetLinebreakInformationPacket *)
maketex[lst_List] := MakeTeX[RowBox@lst]

(* Default *)
(* For any other boxes, we have to assume there's no TeX equivalent. *)
maketex[expr_] :=
  (Message[TeXForm::unspt, expr]; "")

End[] (* End Private Context *)

EndPackage[]