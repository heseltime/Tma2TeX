TabView[{"knowledge" -> Pane[Row[{}, ", "], 500], 
  "built-in" -> Column[{"\[Checkmark] [Sets]", "\[Checkmark] [Tuples]", 
     "\[Checkmark] [Arithmetic]", "\[Checkmark] [Logic]", 
     "\[Checkmark] [Domains]", "\[Checkmark] [Programming]"}], 
  "Computation" -> TabView[
    {"Input" -> DisplayForm[RowBox[{RowBox[{"\[LeftAngleBracket]", 
           RowBox[{"1", ",", "1", ",", "1"}], "\[RightAngleBracket]"}], 
         SubscriptBox["<", "lex"], RowBox[{"\[LeftAngleBracket]", 
           RowBox[{"1", ",", "2", ",", "0"}], "\[RightAngleBracket]"}]}]], 
     "Result" -> DisplayForm[FormBox[RowBox[
         {RowBox[{"\[LeftAngleBracket]", RowBox[{"1", ",", "1", ",", "1"}], 
            "\[RightAngleBracket]"}], SubscriptBox["<", "lex"], 
          RowBox[{"\[LeftAngleBracket]", RowBox[{"1", ",", "2", ",", "0"}], 
            "\[RightAngleBracket]"}]}], TheoremaForm]], 
     "statistics" -> Column[{Labeled[0.015625, "Computation time:", Left]}]}, 
    AutoAction -> True, ControlPlacement -> Left], 
  "Restore settings" -> Row[{"Really restore all relevant settings to the \
values they had when this action was performed last time?", 
     Button["OK", Theorema`Common`setComputationEnvironment[
       "C:\\Users\\jackh\\git\\repository\\tma2tex\\FirstTour\\c1142086229"]]}\
, Spacer[5]]}, ImageSize -> Automatic]
