# Tma2TeX

## Intro

This Software Engineering thesis project at Hagenberg Campus (FHOÖ, AUT) is developed from a [notebook-based prototype](https://github.com/heseltime/prototype-wolfram-lang-theorema-to-latex) detailing some of the thought and ideas going into the design. The framework for the Mathematica/Wolfram Language native approach taken here is [Theorema](https://risc.jku.at/sw/theorema/) ([RISC](https://risc.jku.at/) at Johannes Kepler University, AUT). Thesis forthcoming.

## Design

The project is following a specification (the [overall goal is formulated on the RISC homepage too](https://risc.jku.at/th/theorema-project-document-processing/)) requiring a single file that will be placed inside another package (Theorema). Therefore modularization is in a sense limited, but unit testing will be implemented along with further development, mainly of 

* tma2tex.m

Which is the main file. Other relevant parts are

* /res

The resource directory containing the TeX-template that will be applied to as a template object in Wolfram Language.

* FirstTour.nb

Is the file being transformed.

* tma2tex.nb

Acts as the interface to the main program, transforming the test document.

## Resources

[Wolfram Workbench in the Wolfram Screencast and Video Gallery](https://www.wolfram.com/broadcast/video.php?c=93&v=900)

[MUnit Testing](https://reference.wolfram.com/workbench/index.jsp?topic=/com.wolfram.eclipse.help/html/tasks/tester/tester.html)