

(* Wolfram Language Raw Program *)

BeginPackage["Tma2tex`"];

(* ---- written by Jack Heseltine, starting in July 2023
	Updates 
		- August - December 2023: Set up Project Structure and Recursion Rules
		- January 2024: Introduce Common WL-Package Structure
		- February 2024: Add convertToLatexFromString for Cloud-testing
		- March 2024: Split Recursion into parseNotebookContent and parseTheoremaData,
			i.e. Parts 1A and 1B in the package
	
	Purpose: This program recurses over the Theorema notebook structure to produce a LaTeX representation. The patterns
		are specified in Part 1A and -B, -A for the notebook (Wolfram Language) structure, -B for the Theorema cell structure.
		(Part 1 is where the program would typically be extended in the future, if notebooks or Theorema change in some way.)
		The result is inserted into the appropriate LaTeX template (Part 2, Filehandling): the main function intended 
		for use by the client is at the end of the program, Part 3.
		
	Usage: call one of the variants of the main function, see
		a) convertToLatexDoc[notebookPath_], where notebookPath specifies the Mathematica/Wolfram Lang. notebook to be converted, 
				the output is a TeX file at the level of notebookPath.
			Dependencies: There has to be a LaTeX template in a reachable directory corresponding to the module variable resourceDir.
		b) convertToLatexAndPDFDocs[notebookPath_]: same as above, but also produces a PDF file at the level of the TeX.
			Dependencies: Template as above, but also requires a working TeX distribution installed on your system.
		c) convertToLatexFromString[nbContentString_, resourceDir_Optional]: experimental, intended be called from the Cloud, 
			simply transofrming Wolfram Language String Input to TeX Output (returned directly, not via file). 
			Also uses a template, the resource for which can be passed as the second argument.
	 ---- *)

(* -- Part 0, Imports and Global Variables as per Theorema Specification -- *)

(*-- Optional Theorema-Import with Get --*)

(* Uncomment the Tma-Get call if NOT called from inside the Tma-Package or an environment that loads Tma already *)

(* << Theorema` *)

(*-- Global Variables --*)

Tma2tex`$resDir::usage="Points to ..."

Tma2tex`$resDir = "C:\\Users\\jackh\\git\\repository\\tma2tex\\res"

convertToLatexDoc::usage="convertToLatexDoc[notebookPath] transforms a given WL notebook (by file path) to TeX output, creating a new TeX file from a specified resource template."
convertToLatexAndPDFDocs::usage="convertToLatexAndPDFDocs[notebookPath] transforms a given WL notebook (by file path) to PDF file as final output, with TeX file as intermediary step, from a specified resource template."
convertToLatexFromString::usage="convertToLatexFromString[nbContentString_, resourceDir_Optional]: Tma2tex`$resDir] is experimental and intended be called from the Cloud, simply transofrming Wolfram Language String Input to TeX Output (returned directly, not via file). Also uses a template, the resource for which can be passed as the second argument."

Begin["`Private`"]

(* -- Part 1A, Recursive Pattern Matching: parseNotebookContent[] -- *)

(* -- Part 1A.0 -- In-Flow Expressions: these are processed one after the other *)

(* -- Part 1A.0.0 -- Structural Expressions: \light{}-TeX Command available in Frontend, to demarcate structural text output from content *)

(*parseNotebookContent[Notebook[l_List, ___]] := "NB reached " <> parseNotebookContent /@ l*) 
	(* Careful with Map: Goes to parseNotebookContent[c_Cell] *)
parseNotebookContent[Notebook[l_List, ___]] := "\\light{NB reached} " <> parseNotebookContent[l] 
	(* goes to parseNotebookContent[l_List], this our entry point to parsing *)

parseNotebookContent[c_Cell] := "\\light{Cell reached} " (* matches Cells that are not further specified (as relevant WL or TMA cells) below *)

parseNotebookContent[l_List] := "\\light{List reached} "
parseNotebookContent[l_List] /; MemberQ[l, _Cell] := StringJoin["\\light{List of cells reached} ", ToString /@ parseNotebookContent /@ l] 


parseNotebookContent[Cell[CellGroupData[l_List, ___], ___]] := "\\light{CellGroupData reached} " <> parseNotebookContent[l]


(* -- Part 1A.0.1 -- Text Expressions (at the Cell Level) *)

parseNotebookContent[Cell[text_String, "Text", ___]] := "\\begingroup \\section*{} " <> text <> "\\endgroup \n\n"

parseNotebookContent[Cell[text_String, "Section", ___]] := "\\section{" <> text <> "}\n\n"


(* -- Part 1A.0.2 -- Text/Math/Symbols at the String Level *)

(* Operators *)
parseNotebookContent["<"] := "\\textless"

parseNotebookContent[">"] := "\\textgreater"

(* Greek Letters *)
parseNotebookContent["\[CapitalDelta]"] := "\\Delta"


(* -- Part 1A.0.2.0 -- Boxes *)

parseNotebookContent[Cell[BoxData[FormBox[content_, TraditionalForm]], "DisplayFormula", ___]] := 
    StringJoin["\\begin{center}", parseNotebookContent[content], "\\end{center}\n"]

(* This particular rule does a lot of the parsing through the Tma-Env. *)
parseNotebookContent[RowBox[list_List]] := 
    StringJoin[parseNotebookContent /@ list] 

(* Underscriptboxes *)
parseNotebookContent[UnderscriptBox[base_, script_]] := 
    StringJoin["\\underset{", parseNotebookContent[script], "}{", parseNotebookContent[base], "}"]
    
parseNotebookContent[UnderscriptBox["\[Exists]", cond_]] :=
    "\\underset{" <> parseNotebookContent[cond] <> "}{\\exists}"
parseNotebookContent[UnderscriptBox["\[ForAll]", cond_]] :=
    "\\underset{" <> parseNotebookContent[cond] <> "}{\\forall}"
    
(* -- Part 1A.0.2.1 -- Symbols Dependent on Boxes (TODO: Complete list needed? First Order Logic?) *)

parseNotebookContent[RowBox[{left_, "\[And]", right_}]] := 
    StringJoin[parseNotebookContent[left], " \\land ", parseNotebookContent[right]]

parseNotebookContent[RowBox[{left_, "\[Or]", right_}]] := 
    StringJoin[parseNotebookContent[left], " \\lor ", parseNotebookContent[right]]

parseNotebookContent[RowBox[{left_, "\[DoubleLeftRightArrow]", right_}]] := 
    StringJoin[parseNotebookContent[left], " \\Leftrightarrow ", parseNotebookContent[right]]

parseNotebookContent[RowBox[{left_, "\[Implies]", right_}]] := 
    StringJoin[parseNotebookContent[left], " \\Rightarrow ", parseNotebookContent[right]]
    
parseNotebookContent[RowBox[{left_, "<", right_}]] := 
    parseNotebookContent[left] <> " < " <> parseNotebookContent[right]
    
parseNotebookContent[RowBox[{left_, ">", right_}]] := 
    parseNotebookContent[left] <> " >  " <> parseNotebookContent[right]
    
parseNotebookContent[RowBox[{left_, "\[Equal]", right_}]] := 
    parseNotebookContent[left] <> " = " <> parseNotebookContent[right]

parseNotebookContent[RowBox[{left_, "\[SubsetEqual]", right_}]] := 
	parseNotebookContent[left] <> "\\subseteq" <> parseNotebookContent[right]

parseNotebookContent[RowBox[{left_, "\[Element]", right_}]] := 
	parseNotebookContent[left] <> "\\in" <> parseNotebookContent[right]

(* -- Part 1A.0.2.2 -- Brackets: this rule might be contentious *)

parseNotebookContent[RowBox[{func_, "[", arg_, "]"}]] := 
    StringJoin[parseNotebookContent[func], "(", parseNotebookContent[arg], ")"]
    
    
(* -- Part 1A.1 -- Theorema-Language-specific Expressions *)

(* separating line in the tmanotebook *)
parseNotebookContent[Cell["", "OpenEnvironment", ___]] := 
    "\\begin{openenvironment}\n\\end{openenvironment}"

(* the following cell -> cellgroup -> list of cells is the environment, with environment cells after the header-cell *)     
parseNotebookContent[Cell[CellGroupData[{Cell[headertext_, "EnvironmentHeader", options___], envcells___}, ___]]] :=
    Module[{contentStrings},
        contentStrings = StringJoin[parseNotebookContent /@ {envcells}]; (* Apply parsing to each cell *)
        StringJoin[
            "\\begin{tmaenvironment}\n", 
            "\\subsection{", parseNotebookContent[headertext], "}\n", 
            contentStrings, 
            "\\end{tmaenvironment}\n"
        ]
    ]
    
(* Similar to Tma-Envs, but no Header Text *)
parseNotebookContent[Cell[BoxData[
 RowBox[{envcells___}]], "GlobalDeclaration", ___]] :=
  Module[{contentStrings},
    contentStrings = StringJoin[parseNotebookContent /@ {envcells}];
        StringJoin[
            "\\begin{tmaenvironmentgd}\n", 
            "\\subsubsection{Global Declaration}\n", (* Maybe *)
            contentStrings, 
            "\\end{tmaenvironmentgd}\n"
        ]
  ]
  
parseNotebookContent[Cell[BoxData[content_], "GlobalDeclaration", ___]] :=
    Module[{contentStrings},
        contentStrings = parseNotebookContent[content]; (* Directly pass the content to parseNotebookContent *)
        StringJoin[
            "\\begin{tmaenvironmentgd}\n", 
            "\\subsubsection{Global Declaration}\n", (* Optional title *)
            contentStrings, 
            "\\end{tmaenvironmentgd}\n"
        ]
    ]


    
(* Parse the cells in the theorema environment list one by one: the empty string below generally marks the beginning of a Tma Cell in the TeX *)
parseNotebookContent[Cell[BoxData[rowboxes___], "FormalTextInputFormula", ___]] := "" <> StringJoin[parseNotebookContent /@ {rowboxes}]

(* ensure we handle nested RowBox instances correctly by recursively parsing their content *)
parseNotebookContent[RowBox[list_List]] := 
    StringJoin[parseNotebookContent /@ list]

(* rowbox on list in Part 1A.0.2.0 *)

(* Tma-Env elements that occur within {lists}, often inside RowBox[] *)
parseNotebookContent[TagBox["(","AutoParentheses"]] := "\\left("
parseNotebookContent[TagBox[")","AutoParentheses"]] := "\\right)"
(*parseNotebookContent[UnderscriptBox["\[ForAll]", "x"]] := "forAll "*)
parseNotebookContent[RowBox[{n_, "[", var_, "]"}]] := n <> "[" <> var <> "]"
parseNotebookContent[TagBox["\[DoubleLeftRightArrow]", ___]] := " \\Leftrightarrow "

(* Subscriptboxes *)
parseNotebookContent[SubscriptBox[base_, subscript_]] := 
    parseNotebookContent[base] <> "_{" <> parseNotebookContent[subscript] <> "}"
    
parseNotebookContent[TagBox[content_, _, SyntaxForm -> "a\[Implies]b"]] := 
    "\\rightarrow "


parseNotebookContent[Cell["\[GraySquare]", "EndEnvironmentMarker", ___]] := 
    " \\graysquare{}" 


(* -- Part 1A.2 -- Out-of-Flow Expressions: Reap and Sow mechanism to process in a different order than the expressions are encountered in *)

parseNotebookContent[Cell[t_String, "Title", ___]] := (Sow[t, "title"]; Sow["", "author"]; Sow["", "date"];) (* author and date currently not included in sample doc *)



(* -- Part 1A.3 -- Key for Testing? Highlight Unclaimed Expressions *)

parseNotebookContent[other_] := StringJoin["\\textcolor{red}{", "Pattern not found! ", ToString[other], "}"]


(* -- Part 1A.4 -- String Processing for Symbols Occuring In-text in the Notebook *)
parseNotebookContent[s_String] := StringReplace[s, "\[SubsetEqual]" -> "\\subseteq"]

(* This concludes Part 1A, concerned with parsing the Theorema notebook presenting the Theorema environemnt. 
	Its data is parsed in the following, Part 1B. *)

(* -- Part 1B, Recursive Pattern Matching: parseTheoremaData[] -- *)

(* parseTheoremaData[] := "Test Tma Data!" *)

(* -- Part 2, Filehandling -- *)

writeToLatexDoc[latexPath_, nbContent_] := 
 Module[{strm }, strm = OpenWrite[latexPath];
  WriteString[strm, parseNotebookContent[nbContent]]; 
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
  nb = NotebookOpen[notebookPath, Visible->False];
  content = NotebookGet[nb];
  NotebookEvaluate[content]; (* on content: important, 
    so that Tma env. variables are available in any case *)
  latexPath = getLatexPath[notebookPath];
  latexTemplatePath = getLatexTemplatePath[notebookPath]; 
  (*filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbName" -> FileBaseName[notebookPath]|>];*)
  {texResult, sownData} = Reap[parseNotebookContent[content], {"title", "author", "date"}];
  filledContent = fillLatexTemplate[resourceDir,
  <|
    "nbContent" -> texResult,
    "nbTitle" -> First[sownData[[1, 1]]],
    "nbAuthor" -> First[sownData[[2, 1]]],
    "nbDate" -> First[sownData[[3, 1]]]
  |>];
  Export[latexPath, filledContent, "Text"];
  Print[Theorema`Common`$tmaEnv];
]

convertToLatexAndPDFDocs[notebookPath_] :=  Module[{latexPath, pdfPath, compileCmd},
  convertToLatexDoc[notebookPath];
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
    {texResult, sownData} = Reap[parseNotebookContent[nbContent], {"title", "author", "date"}];

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
  