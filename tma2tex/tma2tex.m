

(* Wolfram Language Raw Program *)

(* ---- written by Jack Heseltine, July 2023 onwards
	Updates August 2023: Filehandling and Client Functions (Parts 2 and 3)
	Updates September 2023 onwards: Recursion Rules (Part 1)
	
	Purpose: This program recurses over the Theorema notebook structure to produce a LaTeX representation. The patterns
		are specified in Part 1 of the program and this is where the program would typically be extended in the future.
		The result is inserted into the appropriate LaTeX template (Part 2, Filehandling): the main function intended 
		for use by the client is at the end of the program, Part 3.
		
	Usage: call one of the variants of the main function, see
		a) convertToLatexDoc[notebookPath_], where notebookPath specifies the Mathematica/Wolfram Lang. notebook to be converted, 
				the output is a TeX file at the level of notebookPath.
			Dependencies: There has to be a LaTeX template in a reachable directory corresponding to the module variable resourceDir.
		b) convertToLatexAndPDFDocs[notebookPath_]: same as above, but also produces a PDF file at the level of the TeX.
			Dependencies: Template as above, but also requires a working TeX distribution installed on your system.
	 ---- *)

(* -- Part 0, Global Variables as per Theorema Specification -- *)

$resDir = "C:\\Users\\jackh\\git\\repository\\tma2tex\\res"

(* -- Part 1, Recursive Pattern Matching: parseNotebookContent[] -- *)

parseNotebookContent[Cell[t_String, "Title", ___]] := (Sow[t, "title"]; Sow["", "author"]; Sow["", "date"];) (* author and date currently not included in sample doc *)

(* -- Part 1.0, 1st Recursive Descent in Wolfram Language -- *)

(*parseNotebookContent[Notebook[l_List, ___]] := "NB reached " <> parseNotebookContent /@ l*) (* goes to parseNotebookContent[c_Cell], see Map *)
(*parseNotebookContent[Notebook[l_List, ___]] := "NB reached " <> parseNotebookContent[l] (* goes to parseNotebookContent[l_List] *)

parseNotebookContent[l_List] := "List reached1 " <> parseNotebookContent @@@ l (* specificity test + MapApply test <--- *)
parseNotebookContent[l_List] /; MemberQ[l, _Cell] := StringJoin["List reached2 ", ToString /@ parseNotebookContent /@ l] 

parseNotebookContent[Cell[CellGroupData[l_List, ___], ___]] := "CellGroupData reached " <> parseNotebookContent[l]

parseNotebookContent[c_Cell[___x]] := "Cell reached " <> parseNotebookContent @@@ x
parseNotebookContent[c_Cell["Proving", ___]] := "Proving Cell reached " *)


(* -- Part 1.1, 2nd Recursive Descent, now in Theorema Language -- *)

(* TODO *)


(* -- Part 1.2, Testing: Generic Rule for Match That Is Not Anticipated -- *)

(*parseNotebookContent[other_] := ToString[other] <> "Test"  handle other patterns, like individual elements within a Cell's content *)


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
]

convertToLatexAndPDFDocs[notebookPath_] :=  Module[{latexPath, pdfPath, compileCmd},
  convertToLatexDoc[notebookPath];
  (* Compile LaTeX to PDF using pdflatex *)
  latexPath = getLatexPath[notebookPath];
  pdfPath = StringReplace[latexPath, ".tex" -> ".pdf"];
  compileCmd = 
   "pdflatex -interaction=nonstopmode -output-directory=" <> 
    DirectoryName[latexPath] <> " " <> latexPath;
  RunProcess[{"cmd", "/c", compileCmd}]
]
  