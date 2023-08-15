

(* Wolfram Language Raw Program *)

patternMatch[Notebook[l_List, ___]] := 
 StringJoin[ToString /@ patternMatch /@ l]
 (* recursive call *)
 
patternMatch[l_List] := 
 patternMatch /@ l(* mapping same level elements *)
 
patternMatch[c_Cell] := "Test" (* stand-in *)

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
   Import[FileNameJoin[{resDir, "template_prototype1.tex"}], "Text"];
  (*no template object needed here*)template = texContent;
  (*Apply the data to the template*)
  filledContent = TemplateApply[template, data];
  (*Return the filled content*)filledContent]

convertToLatexDoc[notebookPath_] := 
 Module[{nb, content, latexPath, latexTemplatePath, 
   resourceDir = 
    "C:\\Users\\jackh\\OneDrive\\Documents\\RISC2023\\prototype-\
wolfram-lang\\res", filledContent},
  nb = NotebookOpen[notebookPath, Visible -> False]; 
  content = NotebookGet[nb];
  latexPath = getLatexPath[notebookPath];
  latexTemplatePath = getLatexTemplatePath[notebookPath]; 
  filledContent = 
   fillLatexTemplate[
    resourceDir, <|"nbName" -> FileBaseName[notebookPath]|>]; 
  Export[latexPath, filledContent, "Text"];]

