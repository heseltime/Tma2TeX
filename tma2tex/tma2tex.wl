

(* Wolfram Language Raw Program *)

BeginPackage["Tma2tex`"];

(* ---- written by Jack Heseltine, July 2023 - July 2024
	Updates 
		- August - December 2023: Set up project structure and basic recursion rule tests
		- January 2024: Introduce common WL-Package structure
		- February 2024: Add convertToLatexFromString for Cloud-testing
		- March/April 2024: Split recursion into parseNbContent and getTmaData/parseTmaData
		- May/June 2024: try approach with TeXForm transformation
	
	Purpose: This program recurses over the Theorema notebook structure to produce a LaTeX representation, including of the 
		underlying Theorema-Datastructure: to this end it inserts the appropriate LaTeX-commands into an output file, mediated
		by a template in the $resDir. Both the notebook- and Theorema-level content is rendered using the relevant LaTeX packages
		and should be modified in LaTeX for the output syntax.
		
		Part 1 of this package is concerned with the described double-recursion, drawing mainly on the Theorema-data provisioned in Part 0.
		For academic purposes, Part 1 is subdivided in Parts
		
			* A: parseNbContent, the main recursive function, with the output-nearer LaTeX-commands,
			* B: also parseNbContent, higher level, Theorema-notebook specific pattern-recursion rules,
			* C: getTmaData and parseTmaData, concerned with establishing the connection between the appropriate part in the input notebook/
				output LaTeX and the given Theorema data, and parsing, again recursively, the formula structure, respectively.
			
		Part 0 is also subdivided in an out/inside-of-package Part A and B respectively, to illustrate packaging in Wolfram Language.
			
		The result is inserted into the appropriate LaTeX template (Part 2, Filehandling): the main functions intended 
		for use by the client is at the end of the program, Part 3, specifically convertToLatexAndPdfDocs[] as the all-in-one transformation 
		function for stand-alone-calling - but convertToLatexDoc[] for the basic Theorema use case. Both functions take the path to the relevant
		Theorema notebook as their single parameter.
		
	 ---- *)
	 
	 
	 

(* -- Part 0, Imports and Global Variables as per Theorema Specification -- *)

(* -- Part 0.A, Imports and Global Variables OUTSIDE-OF-PACKAGE -- *)

(* -- Part 0.A.1 Optional Theorema-Import with Get -- *)

(* << Theorema` *)
(* Uncomment the Tma-Get call if NOT called from inside the Tma-Package or an environment that loads Tma already *)

Needs["Theorema`"]

(* -- Part 0.A.2 Global Variables: Important for interfacing with Theorema. -- *)
Tma2tex`$resDir::usage = "Defines the directory for LaTeX-templates and any other resources."

Tma2tex`$resDir = "C:\\Users\\jackh\\git\\repository\\tma2tex\\res"

	
Tma2tex`$tmaData::usage = "Containes the Theorema-Datastructure that holds formula-experessions and is therefore typically equivalent to
	Theorema`Common`$tmaEnv on the Theorema-side, but can be used to show the content according to Tma2Tex (as a separate package)."

Tma2tex`$tmaData = Theorema`Common`$tmaEnv;


(* -- Part 0.A.3 Client-Function-Usage Messages *)
convertToLatexDoc::usage="convertToLatexDoc[notebookPath] transforms a given WL notebook (by file path) to TeX output, creating a new TeX file from a specified resource template."
convertToLatexAndPdfDocs::usage="convertToLatexAndPdfDocs[notebookPath] transforms a given WL notebook (by file path) to PDF file as final output, with TeX file as intermediary step, from a specified resource template."
convertToLatexFromString::usage="convertToLatexFromString[nbContentString_, resourceDir_Optional]: Tma2tex`$resDir] is experimental and intended be called from the Cloud, simply transofrming Wolfram Language String Input to TeX Output (returned directly, not via file). Also uses a template, the resource for which can be passed as the second argument."


Begin["`Private`"]

(* -- Part 0.B, Imports and Global Variables INSIDE-OF-PACKAGE -- *)

(* Custom message for problem importing Theorema-Datastructure, issued in main client function *)
tmaDataImport::empty = "`1`";

(* The following holds the Tma-Formula-List as an association with keys from the IDs, gets filled in getTmaData[] *)
tmaDataAssoc = <||>;



(* -- Part 1.A, Recursive Pattern Matching: parseNbContent[] with a focus on (mathematical) symbol-level transformations -- *)

(* -- Part 1.A.0 -- Structural Expressions: \light{}-TeX Command available in Frontend, to demarcate structural text output from content *)

(*parseNbContent[Notebook[l_List, ___]] := "NB reached " <> parseNbContent /@ l*) 
	(* Careful with Map: Goes to parseNbContent[c_Cell] *)
parseNbContent[Notebook[l_List, ___]] := "\\light{NB reached} " <> parseNbContent[l] 
	(* goes to parseNbContent[l_List], this our entry point to parsing *)

parseNbContent[c_Cell] := "\\light{Cell reached} " (* matches Cells that are not further specified (as relevant WL or TMA cells) below *)

parseNbContent[l_List] := "\\light{List reached} "
parseNbContent[l_List] /; MemberQ[l, _Cell] := StringJoin["\\light{List of cells reached} ", ToString /@ parseNbContent /@ l] 


parseNbContent[Cell[CellGroupData[l_List, ___], ___]] := "\\light{CellGroupData reached} " <> parseNbContent[l]


(* -- Part 1.A.1 -- Text Expressions (at the Cell Level) *)

parseNbContent[Cell[text_String, "Text", ___]] := "\\begingroup \\section*{} " <> text <> "\\endgroup \n\n"

parseNbContent[Cell[text_String, "Section", ___]] := "\\section{" <> text <> "}\n\n"


(* -- Part 1.A.2 -- Text/Math/Symbols at the String Level *)

(* Operators *)
parseNbContent["<"] := "\\textless"

parseNbContent[">"] := "\\textgreater"

(* Greek Letters *)
parseNbContent["\[CapitalDelta]"] := "\\Delta"


(* -- Part 1.A.3 -- Boxes *)

parseNbContent[Cell[BoxData[FormBox[content_, TraditionalForm]], "DisplayFormula", ___]] := 
    StringJoin["\\begin{center}", parseNbContent[content], "\\end{center}\n"]

(* This particular rule does a lot of the parsing through the Tma-Env. *)
parseNbContent[RowBox[list_List]] := 
    StringJoin[parseNbContent /@ list] 

(* Underscriptboxes *)
parseNbContent[UnderscriptBox[base_, script_]] := 
    StringJoin["\\underset{", parseNbContent[script], "}{", parseNbContent[base], "}"]
    
parseNbContent[UnderscriptBox["\[Exists]", cond_]] :=
    "\\underset{" <> parseNbContent[cond] <> "}{\\exists}"
parseNbContent[UnderscriptBox["\[ForAll]", cond_]] :=
    "\\underset{" <> parseNbContent[cond] <> "}{\\forall}"
    
(* -- Part 1.A.4 -- Symbols Dependent on Boxes (TODO: Complete list needed? First Order Logic?) *)

parseNbContent[RowBox[{left_, "\[And]", right_}]] := 
    StringJoin[parseNbContent[left], " \\land ", parseNbContent[right]]

parseNbContent[RowBox[{left_, "\[Or]", right_}]] := 
    StringJoin[parseNbContent[left], " \\lor ", parseNbContent[right]]

parseNbContent[RowBox[{left_, "\[DoubleLeftRightArrow]", right_}]] := 
    StringJoin[parseNbContent[left], " \\Leftrightarrow ", parseNbContent[right]]

parseNbContent[RowBox[{left_, "\[Implies]", right_}]] := 
    StringJoin[parseNbContent[left], " \\Rightarrow ", parseNbContent[right]]
    
parseNbContent[RowBox[{left_, "<", right_}]] := 
    parseNbContent[left] <> " < " <> parseNbContent[right]
    
parseNbContent[RowBox[{left_, ">", right_}]] := 
    parseNbContent[left] <> " >  " <> parseNbContent[right]
    
parseNbContent[RowBox[{left_, "\[Equal]", right_}]] := 
    parseNbContent[left] <> " = " <> parseNbContent[right]

parseNbContent[RowBox[{left_, "\[SubsetEqual]", right_}]] := 
	parseNbContent[left] <> "\\subseteq" <> parseNbContent[right]

parseNbContent[RowBox[{left_, "\[Element]", right_}]] := 
	parseNbContent[left] <> "\\in" <> parseNbContent[right]

(* -- Part 1.A.5 -- Brackets: this rule might be contentious *)

parseNbContent[RowBox[{func_, "[", arg_, "]"}]] := 
    StringJoin[parseNbContent[func], "(", parseNbContent[arg], ")"]


    
(* -- Part 1.B.0 -- Theorema-Language/-Notebook-specific Expressions,
	these are the jumping off point to the second kind of recursive descent in this program,
	parsing throught the Theorema-Datastructure *)

(* separating line in the tmanotebook *)
parseNbContent[Cell["", "OpenEnvironment", ___]] := 
    "\\begin{openenvironment}\n\\end{openenvironment}"

(* the following cell -> cellgroup -> list of cells is the environment, with environment cells after the header-cell *)     
(*parseNbContent[Cell[CellGroupData[{Cell[headertext_, "EnvironmentHeader", options___], envcells___}, ___]]] :=
    Module[{contentStrings},
        contentStrings = StringJoin[parseNbContent /@ {envcells}]; (* Apply parsing to each cell *)
        StringJoin[
            "\\begin{tmaenvironment}\n", 
            "\\subsection{", parseNbContent[headertext], "}\n", 
            contentStrings, 
            "\\end{tmaenvironment}\n"
        ]
    ] *)
    

    
(* Propositions *)
parseNbContent[Cell[CellGroupData[{Cell[headertext_, "EnvironmentHeader", headeroptions___], Cell[formulaboxdata_, "FormalTextInputFormula", options___], 
    furtherNotebookEnvCells___}, envOptions___]]] :=
    Module[{contentStrings, cellID, optionsAssociation},
        contentStrings = StringJoin[parseNbContent /@ {furtherNotebookEnvCells}];
        optionsAssociation = Association[options];
        cellID = optionsAssociation[CellID];
        
        (* Data processing print statements could be included here, for tracking success: include for development stage *)
	    Print["cellID " <> ToString[cellID] <> " found for Proposition, linking to Tma-Data ..."];

        StringJoin[
            "\\begin{tmaenvironment}\n", 
            "\\subsection{", parseNbContent[headertext], "}\n", 
            (*parseTmaData[formulaboxdata],*) (* 2nd Recursive Descent Entry Point *)
            If[cellID =!= None, "\\text{Cell ID: " <> ToString[cellID] <> "}\n"; parseTmaData[getTmaData[cellID]], StringJoin["\\textcolor{red}{", "No ID Found: Did you load Theorema 
                and evaluate the Theorema cells from the same kernel as this call?", "}\n"]], (* TODO: Messaging *)
            (* contentStrings, *)
            "\\end{tmaenvironment}\n"
        ]
    ]
    
(* Semantics for this? *)
parseNbContent[Cell[BoxData[content_], "FormalTextInputFormula", options___]] :=
    Module[{contentStrings, cellID, optionsAssociation},
        (*contentStrings = StringJoin[parseNbContent /@ {furtherNotebookEnvCells}];*)
        optionsAssociation = Association[options];
        cellID = optionsAssociation[CellID];
        
        (* Data processing print statements could be included here, for tracking success: include for development stage *)
	    Print["cellID " <> ToString[cellID] <> " found for [?], linking to Tma-Data ..."];

        StringJoin[
            "\\begin{tmaenvironment}\n", (* TODO *)
            "\\subsection{[?]}\n", (* TODO *)
            (*parseTmaData[formulaboxdata],*) (* 2nd Recursive Descent Entry Point *)
            If[cellID =!= None, "\\text{Cell ID: " <> ToString[cellID] <> "}\n"; parseTmaData[getTmaData[cellID]], StringJoin["\\textcolor{red}{", "No ID Found: Did you load Theorema 
                and evaluate the Theorema cells from the same kernel as this call?", "}\n"]], (* TODO: Messaging *)
            (* contentStrings, *)
            "\\end{tmaenvironment}\n"
        ]
    ]
    
(* Similar to Tma-Envs, but no Header Text *)
(*parseNbContent[Cell[BoxData[
 RowBox[{envcells___}]], "GlobalDeclaration", ___]] :=
  Module[{contentStrings},
    contentStrings = StringJoin[parseNbContent /@ {envcells}];
        StringJoin[
            "\\begin{tmaenvironmentgd}\n", 
            "\\subsubsection{Global Declaration}\n", (* Maybe *)
            contentStrings, 
            "\\end{tmaenvironmentgd}\n"
        ]
  ]*)
  
parseNbContent[Cell[BoxData[content_], "GlobalDeclaration", ___]] :=
    Module[{contentStrings},
        contentStrings = parseNbContent[content]; (* Directly pass the content to parseNbContent *)
        StringJoin[
            "\\begin{tmaenvironmentgd}\n", 
            "\\subsubsection{Global Declaration}\n", (* Optional title *)
            contentStrings, 
            "\\end{tmaenvironmentgd}\n"
        ]
    ]


    
(* Parse the cells in the theorema environment list one by one: the empty string below generally marks the beginning of a Tma Cell in the TeX *)
parseNbContent[Cell[BoxData[rowboxes___], "FormalTextInputFormula", ___]] := "" <> StringJoin[parseNbContent /@ {rowboxes}]

(* ensure we handle nested RowBox instances correctly by recursively parsing their content *)
parseNbContent[RowBox[list_List]] := 
    StringJoin[parseNbContent /@ list]

(* rowbox on list in Part 1A.0.2.0 *)

(* Tma-Env elements that occur within {lists}, often inside RowBox[] *)
parseNbContent[TagBox["(","AutoParentheses"]] := "\\left("
parseNbContent[TagBox[")","AutoParentheses"]] := "\\right)"
(*parseNbContent[UnderscriptBox["\[ForAll]", "x"]] := "forAll "*)
parseNbContent[RowBox[{n_, "[", var_, "]"}]] := n <> "[" <> var <> "]"
parseNbContent[TagBox["\[DoubleLeftRightArrow]", ___]] := " \\Leftrightarrow "

(* Subscriptboxes *)
parseNbContent[SubscriptBox[base_, subscript_]] := 
    parseNbContent[base] <> "_{" <> parseNbContent[subscript] <> "}"
    
parseNbContent[TagBox[content_, _, SyntaxForm -> "a\[Implies]b"]] := 
    "\\rightarrow "


parseNbContent[Cell["\[GraySquare]", "EndEnvironmentMarker", ___]] := 
    " \\graysquare{}" 


(* -- Part 1.B.1 -- Out-of-Flow Expressions: Reap and Sow mechanism to process in a different order than the expressions are encountered in *)

parseNbContent[Cell[t_String, "Title", ___]] := (Sow[t, "title"]; Sow["", "author"]; Sow["", "date"];) (* author and date currently not included in sample doc *)



(* -- Part 1.B.2 -- Key for Testing? Highlight Unclaimed Expressions *)

parseNbContent[other_] := StringJoin["\\textcolor{red}{", "Pattern not found! ", ToString[other], "}"]


(* -- Part 1.B.3 -- String Processing for Symbols Occuring In-text in the Notebook *)
parseNbContent[s_String] := StringReplace[s, "\[SubsetEqual]" -> "\\subseteq"]
	
	

(* -- Part 1.C.0, Recursive Pattern Matching: getTmaData[] selects the relevant part in Theorema`Common`FML$ in preperation
	for a second recursive descent, see 1.B.2 -- *)

getTmaData[id_Integer] := Module[{assoc, cleanStringKeysAssoc, numericKeysAssoc},
    assoc = Association[Cases[$tmaData, Theorema`Common`FML$[{idFormula_, _}, expr_, no_] :> (idFormula -> expr), {1}]];
    cleanStringKeysAssoc = Association[StringReplace[#, "ID:" -> ""] -> assoc[#] & /@ Keys[assoc]];
    numericKeysAssoc = Association[ToExpression[#] -> cleanStringKeysAssoc[#] & /@ Keys[cleanStringKeysAssoc]];
    numericKeysAssoc[id]
]


(* -- Part 1.C.1-Beta, Recursive Pattern Matching: parseTmaData[] for second recursive descent through formula structure -- *)

parseTmaData[expr___] := ToString[expr] <> " (general case applied) " (*""*) (* most general *)

(*parseTmaData[Theorema`Language`Iff$TM[l_,r_]] := "\\IffTM{" <> parseTmaData[l] <> "}{" <> parseTmaData[r] <> "}"

parseTmaData[Theorema`Language`And$TM[l_, r_]] := "\\AndTM{" <> parseTmaData[l] <> "}{" <> parseTmaData[r] <> "}"


parseTmaData[Theorema`Language`Forall$TM[var_, qual_, expr_]] := "\\ForallTM{" <> parseTmaData[var] <> "}{" <> parseTmaData[expr] <> "}" (* TODO: Include qual*)


parseTmaData[Theorema`Language`RNG$[a_]] := "\\RNG{" <> parseTmaData[a] <> "}"

parseTmaData[Theorema`Language`SIMPRNG$[a_]] := "\\SIMPRNG{" <> parseTmaData[a] <> "}"
    
(* Specific case: Extract x from Theorema`Knowledge`VAR$x$TM,
	assumes such a an atomic variable is contained in Theorema`Language`VAR$[] *)
parseTmaData[Theorema`Language`VAR$[a_]] := 
    Module[{varName = SymbolName[Unevaluated[a]]},
        "\\VarTM{" <> StringReplace[varName, {"VAR$" -> "", "$TM" -> ""}] <> "}"
    ];

parseTmaData[Theorema`Language`Or$TM[l_, r_]] := "\\OrTM{" <> parseTmaData[l] <> "}{" <> parseTmaData[r] <> "}"

parseTmaData[sym_[a__] /; Context[sym] === "Theorema`Knowledge`"] := 
	Module[{varName = SymbolName[Unevaluated[sym]]},
        "\\Predicate{" <> StringReplace[varName, {"Theorema`Knowledge`" -> "", "$TM" -> ""}] <> "}{" <> parseTmaData[a] <> "}"
    ]; (*  pTD[...&& StringStartsQ[SymbolName[Unevaluated[sym]], "Q"]] *)
    
parseTmaData[Theorema`Language`Implies$TM[l_,r_]] := "\\ImpliesTM{" <> parseTmaData[l] <> "}{" <> parseTmaData[r] <> "}"*)


(* -- Part 1.C.1, Recursive Pattern Matching: second recursive descent more generalized -- *)

(* Generalized parsing function *)
parseTmaData[op_[args___]] := (* always seems to have list length 1 *)
  Module[{nextOp, argList, parsedArgs},
  nextOp = tmaToTeXable[op];
  argList = {args};
    parsedArgs = Switch[
      Length[argList], (* expected to be 1 *)
      1, parseTmaData[argList[[1]]],
      _, "unexpected number of arguments"
    ];
    " " <> ToString[nextOp] (* TODO: LaTeX Conversion *) <> parsedArgs 
  ]

(* Parsing function for expressions with standard operators *)
(*parseTmaData[(op_?isStandardOperatorName)[args___]] := 
  With[{nextOp = tmaToTeXable[op]},
    ToString[nextOp] <> " " <> 
      StringJoin[parseTmaData /@ {args}, ", "]
  ]*)
parseTmaData[(op_?isStandardOperatorName)[args___]] := 
  Module[{nextOp, argList, parsedArgs},
    nextOp = tmaToTeXable[op];
    argList = {args};
    parsedArgs = Switch[
      Length[argList],
      1, parseTmaData[argList[[1]]],
      2, parseTmaData[argList[[1]]] <> ", " <> parseTmaData[argList[[2]]],
      3, parseTmaData[argList[[1]]] <> (* True/False discarded *) ", " <> parseTmaData[argList[[3]]], 
      _, "unexpected number of arguments"
    ];
    (*Print[ToString[nextOp] <> "[" <> parsedArgs <> "]"];*)
    " " <> ToString[nextOp] <> "[" <> parsedArgs <> "]" (*// TeXForm*)
  ]
  
parseTmaData[(op_?isVarOp)[args___]] := (* processes VAR$ outer op to get inner VAR$var$TM *)
  Module[{nextOp, argList, parsedArgs},
    nextOp = tmaVarToTeXable[op];
    argList = {args};
    parsedArgs = Switch[
      Length[argList],
      1, parseTmaData[argList[[1]]], (* call with VAR$var$TM *)
      _, "unexpected number of arguments"
    ];
    " " <> parsedArgs (* VAR$ is ignored *)
  ]
  
parseTmaData[(op_?isVarName)] := (* processes VAR$var$TM *)
  Module[{nextOp},
    nextOp = tmaVarToTeXable[op];
    " " <> "\\Variable{" <> ToString[nextOp] <> "}"
  ]
  
parseTmaData[(op_?isPredicate)[args___]] := (* processes VAR$ outer op to get inner VAR$var$TM *)
  Module[{nextOp, argList, parsedArgs},
    nextOp = tmaPredToTeXable[op];
    argList = {args};
    parsedArgs = Switch[
      Length[argList],
      1, parseTmaData[argList[[1]]], (* whatever the predicate is applied to *)
      _, "unexpected number of arguments"
    ];
    " " <> ToString[nextOp] (* no LaTeX conversion needed here actually, just bracketing *) <> "[ " <> parsedArgs <> " ]"
  ]


(* -- Part 1.C.2, Tma-Syntax(.m) auxilliary functionality used: needed for standalone package implementation, 
	otherwise Tma2Tex Needs[] Syntax.m (if integrating into Tma directly) -- *)

isStandardOperatorName[f_Symbol] := (* Context required here to distinguish from predicates like Theorema`Knowledge`P$TM  *)
    With[ {n = SymbolName[ f], c = Context[ f]},
        (*Print["isStdOpName? "]; Print[ StringTake[ n, -3] === "$TM"]; Print[ n]; *)c === "Theorema`Language`" && StringLength[ n] > 3 && StringTake[ n, -3] === "$TM"
    ]
isStandardOperatorName[f_] := False

tmaToTeXable[op_Symbol] :=
    With[ {n = SymbolName[op]},
        If[ StringTake[ n, -3] == "$TM",
        	ToExpression[ StringDrop[ n, -3]],
        (*else*)
            ToExpression[ n]
        ]
    ]
    
    
isVarOp[f_Symbol] := (* targets VAR$ outer op but not inner VAR$var$TM *)
    With[ {n = SymbolName[ f]},
        (*Print["isVarOp? "]; Print[ StringTake[ n, 4] === "VAR$" && StringTake[ n, -1] === "$"]; Print[ n]; *)StringTake[ n, 4] === "VAR$" && StringTake[ n, -1] === "$"
    ]
isVarOp[f_] := False

isVarName[f_Symbol] := (* targets VAR$ outer op but not inner VAR$var$TM, could also be hooked upon Theorema`Knowledge context *)
    With[ {n = SymbolName[ f]},
        (*Print["isVarName? "]; Print[ StringTake[ n, 4] === "VAR$" && StringTake[ n, -3] === "$TM"]; Print[ n]; *)StringTake[ n, 4] === "VAR$" && StringTake[ n, -3] === "$TM"
    ]
isVarName[f_] := False

tmaVarToTeXable[op_Symbol] := (* transforms VAR$ outer op (by cancelling it)/inner VAR$var$TM to just var *)
    With[ {n = SymbolName[op]},
        If[ StringTake[ n, -3] == "$TM",
        	ToExpression[ StringDrop[ StringDrop[ n, 4], -3]],
        (*else*)
            ToExpression[ StringDrop[ n, 4]] (* "" *)
        ]
    ]
    
    
isPredicate[f_Symbol] := (* targets expressions of the form Theorema`Knowledge`P$TM *)
    With[ {n = SymbolName[ f], c = Context[ f]},
        (*Print["isPred? "]; Print[ c === "Theorema`Knowledge`" && StringTake[ n, -3] === "$TM"]; Print[ c]; Print[ n]; *)c === "Theorema`Knowledge`" && StringTake[ n, -3] === "$TM"
    ]
isPredicate[f_] := False

tmaPredToTeXable[pred_Symbol] := (* transforms something like Theorema`Knowledge`P$TM to P[] *)
    With[ {n = SymbolName[pred]},
        If[ StringTake[ n, -3] == "$TM",
        	ToExpression[ StringDrop[ n, -3]],
        (*else*)
            ToExpression[ n] (* "" *)
        ]
    ]
    
(* -- Part 1.C.3, custom auxiliary functions for TeX-transformation -- *)

(*makeTex[s_String] := 
 If[hasTexForm[s], TexForm, makeCustomTex]*) (* add backlash prefix and TM suffix, command has to be added to template *)

(* -- Part 2, Filehandling -- *)

writeToLatexDoc[latexPath_, nbContent_] := 
 Module[{strm }, strm = OpenWrite[latexPath];
  WriteString[strm, parseNbContent[nbContent]]; 
  Close[strm]] (* stream handling, call to pattern Matching part *)
  
getLatexPath[notebookPath_String] := 
 Module[{directory, baseName, latexExtension = ".tex"}, 
  directory = DirectoryName[notebookPath];
  baseName = FileBaseName[notebookPath];
  FileNameJoin[{directory, 
    baseName <> 
     latexExtension}]] (* get the latex file name (the new .tex file \
that is being filled with the nb-content) with full path from a given \
notebook path*)

getLatexTemplatePath[notebookPath_String] := 
 Module[{directory, baseName, latexExtension = ".tex"}, 
  directory = DirectoryName[notebookPath];
  baseName = FileBaseName[notebookPath] <> "TheoremaTemplate";
  FileNameJoin[{directory, 
    baseName <> 
     latexExtension}]] (* get the latex file name (the .tex-template \
which includes the main nb-content-.tex-file correctly) with full \
path from a given notebook path*)

fillLatexTemplate[resDir_String, data_Association] := 
 Module[{texContent, template, 
   filledContent},(*Import the LaTeX template*)
  texContent = 
   Import[FileNameJoin[{resDir, "tmaTemplate.tex"}], "Text"];
  (*no template object needed here*)template = texContent;
  (*Apply the data to the template*)
  filledContent = TemplateApply[template, data];
  (*Return the filled content*)filledContent]
  
  
  
(* -- Part 3, Main Functions for Client -- *)
  
convertToLatexDoc[notebookPath_] :=  Module[{nb, content, latexPath, latexTemplatePath, 
   resourceDir = $resDir, texResult, sownData, filledContent},
  If[Length[$tmaData] == 0, (* Issue message if Theorema-Formula-Data not provisioned *)
	Message[tmaDataImport::empty, "The Theorema-Formula-Datastructure is empty. 
		Did you evaluate a Theorema notebook before loading the package and calling the conversion function?"];
	(* Additional handling for empty data can be added here *)
	Return[$Failed]
  ];
  nb = NotebookOpen[notebookPath, Visible->False];
  content = NotebookGet[nb];
  NotebookEvaluate[content]; (* on content: important, 
    so that Tma env. variables are available in any case *)
  latexPath = getLatexPath[notebookPath];
  latexTemplatePath = getLatexTemplatePath[notebookPath]; 
  (*filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbName" -> FileBaseName[notebookPath]|>];*)
  {texResult, sownData} = Reap[parseNbContent[content], {"title", "author", "date"}];
  filledContent = fillLatexTemplate[resourceDir,
  <|
    "nbContent" -> texResult,
    "nbTitle" -> First[sownData[[1, 1]]],
    "nbAuthor" -> First[sownData[[2, 1]]],
    "nbDate" -> First[sownData[[3, 1]]]
  |>];
  Export[latexPath, filledContent, "Text"];
  (*Print[Theorema`Common`$tmaEnv];*)
]

convertToLatexAndPdfDocs[notebookPath_] :=  Module[{latexPath, pdfPath, compileCmd, conversionResult},
  conversionResult = convertToLatexDoc[notebookPath];
  If[conversionResult === $Failed,
    Return[$Failed]
  ];
  (* Compile LaTeX to PDF using pdflatex *)
  latexPath = getLatexPath[notebookPath];
  pdfPath = StringReplace[latexPath, ".tex" -> ".pdf"];
  compileCmd = 
   "pdflatex -interaction=nonstopmode -output-directory=" <> 
    DirectoryName[latexPath] <> " " <> latexPath;
  RunProcess[{"cmd", "/c", compileCmd}];
]

convertToLatexFromString[nbContentString_, resourceDir_Optional: Tma2tex`$resDir] := Module[
    {nbContent, texResult, sownData, filledContent},
    
    (* Convert the string representation to a Wolfram Language expression *)
    nbContent = ToExpression[nbContentString, InputForm];

    (* Process the notebook content *)
    {texResult, sownData} = Reap[parseNbContent[nbContent], {"title", "author", "date"}];

    (* Fill in the LaTeX template with parsed content *)
    filledContent = fillLatexTemplate[resourceDir,
        <|
            "nbContent" -> texResult,
            "nbTitle" -> First[sownData[[1, 1]]],
            "nbAuthor" -> First[sownData[[2, 1]]],
            "nbDate" -> First[sownData[[3, 1]]]
        |>
    ];

    (* Return the filled LaTeX content as a string *)
    filledContent
]

End[]

EndPackage[];
  