(* ::Package:: *)
(* ::Section::Closed:: *)
(*Begin package*)
BeginPackage["Utilities`CellReuse"]


(* ::Section:: *)
(*Description*)

(*Functions for reuse of the cell content*)

CellReuseSequencePrint::usage="CellReuseSequencePrint[] prints a sequence of commands for re evaluation of cells with tag replacement rules"

CreateCellIDs::usage="CreateCellIDs[]"

ReEvaluateCells::usage="ReEvaluateCells[cellIDList,textReplacementRules,InputOnly\[Rule]False,PrintExpressions\[Rule]True,EvaluateExpressions\[Rule]False]"

GetCellIDButton::usage="GetCellIDButton[cellIDList]"


(* ::Section:: *)
(*Body*)
Begin["`Private`"]


CellReuseSequencePrint[]:=CellPrint[
	Cell[
		CellGroupData[{
			Cell["New", "Section"],
			Cell[
				CellGroupData[{
					Cell[BoxData[
					 RowBox[{"GetCellIDButton", "[", "cIDs", "]"}]], "Input"],
					Cell[BoxData[
					 ButtonBox["\<\"Get ID's of selected cells\"\>",
					  Appearance->Automatic,
					  ButtonFunction:>
					   Module[{$CellContext`cellList$, $CellContext`nb$}, \
					$CellContext`nb$ = InputNotebook[]; $CellContext`cellList$ = 
					     SelectedCells[$CellContext`nb$]; $CellContext`cIDs = 
					     CurrentValue[$CellContext`cellList$, "CellID"]],
					  Evaluator->Automatic,
					  Method->"Preemptive"]], "Output"]
					}, Open  
				]
			],
			Cell[BoxData[
			 RowBox[{"ReEvaluateCells", "[", 
			  RowBox[{"cIDs", ",", 
			   RowBox[{"\"\<M1\>\"", "\[Rule]", "\"\<M2\>\""}]}], "]"}]], "Input"]
			}, Open  
		]
	]
]


(*Assign ID's to the existing cells in the notebook and enable their creation for the new cells*)
CreateCellIDs[]:=Module[{nb},
	nb=InputNotebook[];
	SetOptions[nb,CreateCellID->True];
	SelectionMove[nb,All,Notebook];
	FrontEndTokenExecute[nb,"Cut"];
	FrontEndTokenExecute[nb,"Paste"];
	NotebookFind[nb,"CreateCellIDs[",All,CellContents];
]


(*CellIDButton creates a button, on a push of which the ID's of the currently selected cells a retrieved and assigned to its argument*)
ClearAll[GetCellIDButton];
SetAttributes[GetCellIDButton,HoldFirst];
GetCellIDButton[cellIDList_]:=Button["Get ID's of selected cells",
	Module[{cellList,nb},
		nb=InputNotebook[];
		cellList=SelectedCells[nb];
		cellIDList=CurrentValue[cellList,"CellID"]
	]
]


(*function takes a list of cell ID's (i.g., argument from CellIDButton), applies he replacement rules from textReplacementRules to the textual representation of their content and re-evaluates the new cells*)
ReEvaluateCells[cellIDList_,textReplacementRules_,OptionsPattern[{InputOnly->False,PrintExpressions->True,EvaluateExpressions-> False}]]:=
(*
Option InputOnly if set to be true, select for execution only input cells among those with IDs in cellIDList; 
PrintCommands\[Rule]True prints the commands before execution;
*)
Module[{cellList,cellListTransf,nb},
	nb=InputNotebook[];
	(*extract the cells using their IDs*)
	cellList=NotebookRead[
		If[OptionValue[InputOnly],
		Cells[nb,CellID-> cellIDList,CellStyle->"Input"],
		Cells[nb,CellID-> cellIDList]]
	];
	(*omit output cells anyway*)
	cellList=Select[cellList,#[[2]]=!="Output"&];
	(*convert Cell[]'s list to a sting, then apply replacement rules and then transform back to Cell[] expressions*)
	cellListTransf=ToExpression[StringReplace[ToString[cellList,InputForm],textReplacementRules]];
	(*print outputs*)
	Do[
		If[OptionValue[PrintExpressions],
			(*If PrintExpressions is enabled, print the content of the new transformed cells.*)
			NotebookWrite[nb,cellListTransf[[i]]]
		];
		If[OptionValue[EvaluateExpressions],
		(*If EvaluateExpressions is enabled, evaluate and print the BoxData[] from the new cell*)
			Print[ToExpression[cellListTransf[[i,1]]]]
		];,
		{i,Length[cellListTransf]}
	]
]

(* ::Section::Closed:: *)
(*End*)
End[]
EndPackage[]