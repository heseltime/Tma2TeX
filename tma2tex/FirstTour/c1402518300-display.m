TabView[{"knowledge" -> Pane[Row[{}, ", "], 500], 
  "built-in" -> Column[{"\[Checkmark] [Sets]", "\[Checkmark] [Tuples]", 
     "\[Checkmark] [Arithmetic]", "\[Checkmark] [Logic]", 
     "\[Checkmark] [Domains]", "\[Checkmark] [Programming]"}], 
  "Computation" -> TabView[
    {"Input" -> DisplayForm[RowBox[{RowBox[{"\[LeftAngleBracket]", 
           RowBox[{"3", ",", RowBox[{"\[LeftAngleBracket]", RowBox[
                {"1", ",", "2"}], "\[RightAngleBracket]"}]}], 
           "\[RightAngleBracket]"}], UnderscriptBox["<", 
          RowBox[{"Mon", "[", "\[DoubleStruckCapitalQ]", "]"}]], 
         RowBox[{"\[LeftAngleBracket]", RowBox[{RowBox[{"3", "/", "4"}], ",", 
             RowBox[{"\[LeftAngleBracket]", RowBox[{"3", ",", "7"}], 
               "\[RightAngleBracket]"}]}], "\[RightAngleBracket]"}]}]], 
     "Result" -> DisplayForm[FormBox[RowBox[
         {RowBox[{"\[LeftAngleBracket]", RowBox[{"3", ",", 
              RowBox[{"\[LeftAngleBracket]", RowBox[{"1", ",", "2"}], 
                "\[RightAngleBracket]"}]}], "\[RightAngleBracket]"}], 
          UnderscriptBox["<", RowBox[{"Mon", "[", "\[DoubleStruckCapitalQ]", 
             "]"}]], RowBox[{"\[LeftAngleBracket]", 
            RowBox[{FractionBox["3", "4"], ",", RowBox[{
                "\[LeftAngleBracket]", RowBox[{"3", ",", "7"}], 
                "\[RightAngleBracket]"}]}], "\[RightAngleBracket]"}]}], 
        TheoremaForm]], "statistics" -> 
      Column[{Labeled[0., "Computation time:", Left]}]}, AutoAction -> True, 
    ControlPlacement -> Left], "Restore settings" -> 
   Row[{"Really restore all relevant settings to the values they had when \
this action was performed last time?", Button["OK", 
      Theorema`Common`setComputationEnvironment[
       "C:\\Users\\jackh\\git\\repository\\tma2tex\\FirstTour\\c1402518300"]]}\
, Spacer[5]]}, ImageSize -> Automatic]
