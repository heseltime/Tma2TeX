

(* Wolfram Language Raw Program *)

(* ---- written by Jack Heseltine, July 2023
	Updates August 2023: Recursion Rules
	
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

(* -- Part 1, Recursive pattern matching -- *)

patternMatch[Notebook[l_List, ___]] := 
 StringJoin[ToString /@ patternMatch /@ l]
 (* recursive call *)
 
patternMatch[l_List] := 
 patternMatch /@ l(* mapping same level elements *)
 
patternMatch[c_Cell] := "Test" (* stand-in *)

(* -- Part 2, Filehandling -- *)

writeToLatexDoc[latexPath_, nbContent_] := 
 Module[{strm }, strm = OpenWrite[latexPath];
  WriteString[strm, patternMatch[nbContent]]; 
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
  
  
(* -- Part 3, Main functions intended for client. -- *)

convertToLatexDoc[notebookPath_] := 
 Module[{nb, content, latexPath, latexTemplatePath, 
   resourceDir = 
    "C:\\Users\\jackh\\git\\repository\\tma2tex\\res", filledContent},
  nb = NotebookOpen[notebookPath, Visible -> False]; 
  content = NotebookGet[nb];
  latexPath = getLatexPath[notebookPath];
  latexTemplatePath = getLatexTemplatePath[notebookPath]; 
  (*filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbName" -> FileBaseName[notebookPath]|>];*)
  filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbContent" -> patternMatch[content]|>];
  Export[latexPath, filledContent, "Text"];]

convertToLatexAndPDFDocs[notebookPath_] :=  Module[{nb, content, latexPath, latexTemplatePath, 
   resourceDir = 
    "C:\\Users\\jackh\\git\\repository\\tma2tex\\res", filledContent, pdfPath, compileCmd},
  nb = NotebookOpen[notebookPath, Visible -> False]; 
  content = NotebookGet[nb];
  latexPath = getLatexPath[notebookPath];
  latexTemplatePath = getLatexTemplatePath[notebookPath]; 
  (*filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbName" -> FileBaseName[notebookPath]|>];*)
  filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbContent" -> patternMatch[content]|>];
  Export[latexPath, filledContent, "Text"];
  (* Compile LaTeX to PDF using pdflatex *)
  pdfPath = StringReplace[latexPath, ".tex" -> ".pdf"];
  compileCmd = 
   "pdflatex -interaction=nonstopmode -output-directory=" <> 
    DirectoryName[latexPath] <> " " <> latexPath;
  RunProcess[{"cmd", "/c", compileCmd}];
  ]