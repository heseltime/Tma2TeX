(* Wolfram Language Test file *)

(* Code-Under-Test Part, as of 2024/08/30 *)

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
            If[cellID =!= None, "\\text{Cell ID: " <> ToString[cellID] <> "}\n"; formatTmaData@parseTmaData[getTmaData[cellID]], StringJoin["\\textcolor{red}{", "No ID Found: Did you load Theorema 
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
            If[cellID =!= None, "\\text{Cell ID: " <> ToString[cellID] <> "}\n"; formatTmaData@parseTmaData[getTmaData[cellID]], StringJoin["\\textcolor{red}{", "No ID Found: Did you load Theorema 
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
	
	

(* -- Part 1.C.0, Recursive Pattern Matching: getTmaData[] selects the relevant part in Theorema`Common`FML$ in preparation
	for a second recursive descent, see 1.B.2 -- *)

getTmaData[id_Integer] := Module[{assoc, cleanStringKeysAssoc, numericKeysAssoc},
    assoc = Association[Cases[$tmaData, Theorema`Common`FML$[{idFormula_, _}, expr_, no_] :> (idFormula -> expr), {1}]];
    cleanStringKeysAssoc = Association[StringReplace[#, "ID:" -> ""] -> assoc[#] & /@ Keys[assoc]];
    numericKeysAssoc = Association[ToExpression[#] -> cleanStringKeysAssoc[#] & /@ Keys[cleanStringKeysAssoc]];
    numericKeysAssoc[id]
]

(* -- Part 1.C.1, Recursive Pattern Matching: Second Recursive Descent  -- *)

(* Alternative 1: Operation is known, it is in the language:
	In this case, there should be a TeX-macro, i.e. Curly-Brackets-Case *)
parseTmaData[op_?isTmaLanguageSymbol[args___]] := 
  Module[{nextOp, argList, parsedArgs},
  nextOp = prepareSymbolName[op]; 
  argList = {args};
    parsedArgs = Switch[
      Length[argList], (* 1 argument in the majority of cases *)
      1, "{" <> parseTmaData[argList[[1]]] <> "}",
      2, "{" <> parseTmaData[argList[[1]]] <> "}{" <> parseTmaData[argList[[2]]] <> "}",
      3, "{" <> parseTmaData[argList[[1]]] <> "}{" <> parseTmaData[argList[[3]]] <> "}",
      _, "" (* unexpected number of arguments: abort the parse tree here *)
    ];
    " \\" <> ToString[nextOp] <> parsedArgs
  ]
  
(* Alternative 2: Knowledge-case - Predicate or Function Symbol, not Language-Operator *)
parseTmaData[op_[args___]] := 
  Module[{nextOp, argList, parsedArgs},
  nextOp = prepareSymbolName[op]; 
  argList = {args};
    parsedArgs = Switch[
      Length[argList], (* 1 argument in the majority of cases *)
      1, "[" <> parseTmaData[argList[[1]]] <> "]",
      2, "[" <> parseTmaData[argList[[1]]] <> ", " <> parseTmaData[argList[[2]]] <> "]",
      _, "" (* unexpected number of arguments: abort the parse tree here *)
    ];
    " " <> ToString[nextOp] <> parsedArgs (* Does not get prefixed with "\"! *)
  ]
  
(* Alternative 3: Special Case Two-Agument-Sets e.g. 

	Theorema`Language`Annotated$TM[Theorema`Language`Less$TM, 
  Theorema`Language`SubScript$TM[Theorema`Knowledge`lex$TM]]
  	[Theorema`Language`VAR$[Theorema`Knowledge`VAR$a$TM], 
 	Theorema`Language`VAR$[Theorema`Knowledge`VAR$b$TM]] 
 	
 	... transforms to more complex macro *)
parseTmaData[op_?isTmaLanguageSymbol[args___][args2___]] := 
  Module[{nextOp, argList, argList2, parsedArgs, parsedArgs2},
    nextOp = prepareSymbolName[op];
    argList = {args};
	parsedArgs = Switch[
	  Length[argList],
	  1, "{" <> parseTmaData[argList[[1]]] <> "}",
	  2, "{" <> parseTmaData[argList[[1]]] <> "}{" <> parseTmaData[argList[[2]]] <> "}",
	  3, "{" <> parseTmaData[argList[[1]]] <> "}{" <> parseTmaData[argList[[3]]] <> "}",
	  _, "" (* Unexpected number of arguments: stop parsing here *)
	];

    argList2 = {args2};
	parsedArgs2 = Switch[
	  Length[argList2],
	  1, "{" <> parseTmaData[argList2[[1]]] <> "}",
	  2, "{" <> parseTmaData[argList2[[1]]] <> "}{" <> parseTmaData[argList2[[2]]] <> "}",
	  3, "{" <> parseTmaData[argList[[1]]] <> "}{" <> parseTmaData[argList[[3]]] <> "}",
	  _, "" (* Unexpected number of arguments: stop parsing here *)
	];
    " \\" <> ToString[nextOp] <> parsedArgs <> parsedArgs2
  ]
  
(* Special Case/Alternative 4: Numbers *)
parseTmaData[i_Integer] := (* e.g. in Theorema`Language`VAR$[Theorema`Knowledge`VAR$m1$TM], 2], 
	2 eventually gets processed*)
  ToString[i]
  
(* Recursion-Stop (Alternative 5) Axiomatic Expression/No Operation *)
parseTmaData[ax_] := (* e.g. Theorema`Knowledge`VAR$x$TM, i.e. axioms/parse-tree leaves*)
  prepareSymbolName[ax]

(* -- Part 1.C.2, Auxilliary Functionality -- *)
	
isTmaLanguageSymbol[f_Symbol] := (* Context required here to distinguish from predicates like Theorema`Knowledge`P$TM  *)
    With[ {n = SymbolName[f], c = Context[f]},
        c === "Theorema`Language`"
    ]
isTmaLanguageSymbol[f_] := False

prepareSymbolName[op_Symbol] := 
    With[{n = SymbolName[op]},
        If[StringTake[n, -3] == "$TM",
        	If[StringTake[n, 4] == "VAR$", (* e.g. Theorema`Knowledge`VAR$x$TM *)
        		(*"\\" <> *)StringDrop[StringDrop[n, 4], -3](* <> "TM"*),
        	(*else*) (* e.g. Theorema`Language`Forall$TM, but: Theorema`Knowledge`Q$TM *)
        		If[isTmaLanguageSymbol[op], (* only language should be suffixed with TM for TeX-Macros *)
        			(*"\\" <> *)StringDrop[n, -3] <> "TM",
        		(*else*)
        			StringDrop[n, -3]
        		]
        	],
        (*else*)
        	If[StringTake[n, -1] == "$", (* e.g. VAR$ *)
        		StringDrop[n, -1] <> "TM",
        	(*else*)
        		n <> "TM"
        	]
        ]
    ]
    
(* -- Part 1.C.3, Final String-level Replacements -- *)
    
formatTmaData[parsedExpression_String] :=
  Module[{replacedString}, (* As needed *)
  replacedString = StringReplace[parsedExpression, ""->""];
  replacedString <> "\n" (* Take care that LaTeX outputs are on their own lines here *)
]

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
   resourceDir = $resDir, texResult, sownData, filledContent, closeFlag = False},
  If[Length[$tmaData] == 0, (* Issue message if Theorema-Formula-Data not provisioned *)
	Message[tmaDataImport::empty, "The Theorema-Formula-Datastructure is empty. 
		Did you evaluate a Theorema notebook before loading the package and calling the conversion function?"];
	(* Additional handling for empty data can be added here *)
	Return[$Failed]
  ];
  
  (*nb = NotebookOpen[notebookPath, Visible->False];*) (* WSRP 2024: ! *)
  
  (* Update: check if NB is already open, then just load 
  	-> otherwise do it in a way that is not visible. 
  	
  	Comment the following and the final line in the function to demo the problem,
  		needs a test document like FirstTour open. *)
  nb = If[isNotebookOpen[notebookPath],
  	NotebookOpen[notebookPath],
  	NotebookOpen[notebookPath, Visible->False]; closeFlag = True];
  
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
  
  If[closeFlag === True, NotebookClose[notebookPath]]; (* WSRP 2024: ! *)
]

(* Helper fn to determin if the notebook specified by the given path is open *)
isNotebookOpen[path_] := 
 Module[{c}, 
  Quiet[MemberQ[Notebooks[], 
    n_ /; (c = "FileName" /. NotebookInformation[n]; c[[2]]) === 
      FileNameTake[path, -1]]]]

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

(* Tests-Part *)

(* Helper Fn *)
cleanString[str_] := StringReplace[str, Whitespace -> ""]

(* Main Test from Overview Notebook *)
test1 = TestCreate[
  cleanString@parseTmaData[
    Theorema`Language`Iff$TM[
      Theorema`Language`And$TM[
        Theorema`Language`Forall$TM[
          Theorema`Language`RNG$[
            Theorema`Language`SIMPRNG$[
              Theorema`Language`VAR$[Theorema`Knowledge`VAR$x$TM]]], 
          True, 
          Theorema`Language`Or$TM[
            Theorema`Knowledge`P$TM[
              Theorema`Language`VAR$[Theorema`Knowledge`VAR$x$TM]], 
            Theorema`Knowledge`Q$TM[
              Theorema`Language`VAR$[Theorema`Knowledge`VAR$x$TM]]]], 
        Theorema`Language`Forall$TM[
          Theorema`Language`RNG$[
            Theorema`Language`SIMPRNG$[
              Theorema`Language`VAR$[Theorema`Knowledge`VAR$y$TM]]], 
          True, 
          Theorema`Language`Implies$TM[
            Theorema`Knowledge`P$TM[
              Theorema`Language`VAR$[Theorema`Knowledge`VAR$y$TM]], 
            Theorema`Knowledge`Q$TM[
              Theorema`Language`VAR$[Theorema`Knowledge`VAR$y$TM]]]]], 
      Theorema`Language`Forall$TM[
        Theorema`Language`RNG$[
          Theorema`Language`SIMPRNG$[
            Theorema`Language`VAR$[Theorema`Knowledge`VAR$x$TM]]], 
        True, 
        Theorema`Knowledge`Q$TM[
          Theorema`Language`VAR$[Theorema`Knowledge`VAR$x$TM]]]
    ]
  ],
  cleanString[
    "\\IffTM{\\AndTM{\\ForallTM{\\RNGTM{\\SIMPRNGTM{\\VARTM{x}}}}{\\OrTM{P[\\VARTM{x}]}{Q[\\VARTM{x}]}}}{\\ForallTM{\\RNGTM{\\SIMPRNGTM{\\VARTM{y}}}}{\\ImpliesTM{P[\\VARTM{y}]}{Q[\\VARTM{y}]}}}}{\\ForallTM{\\RNGTM{\\SIMPRNGTM{\\VARTM{x}}}}{Q[\\VARTM{x}]}}"
  ],
  TestID -> "Test1"
];

(* Example of another test *)
test2 = TestCreate[
  cleanString@parseTmaData[
    Theorema`Language`Forall$TM[
      Theorema`Language`RNG$[
        Theorema`Language`SIMPRNG$[
          Theorema`Language`VAR$[Theorema`Knowledge`VAR$a$TM]], 
        Theorema`Language`SIMPRNG$[
          Theorema`Language`VAR$[Theorema`Knowledge`VAR$b$TM]]], 
      Theorema`Language`Equal$TM[
        Theorema`Language`BracketingBar$TM[
          Theorema`Language`VAR$[Theorema`Knowledge`VAR$a$TM]], 
        Theorema`Language`BracketingBar$TM[
          Theorema`Language`VAR$[Theorema`Knowledge`VAR$b$TM]]]
    ]
  ],
  cleanString[
    "\\ForallTM{\\RNGTM{\\SIMPRNGTM{\\VARTM{a}}}{\\SIMPRNGTM{\\VARTM{b}}}}{\\EqualTM{\\BracketingBarTM{\\VARTM{a}}}{\\BracketingBarTM{\\VARTM{b}}}}"
  ],
  TestID -> "Test2"
];

testReport = TestReport[{test1, test2}];
Export["TestResults_tma2tex_3_mainFunction2.txt", testReport, "Text"];

