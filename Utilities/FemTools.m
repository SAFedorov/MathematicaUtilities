(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(*A fix for usage messages, from http://mathematica.stackexchange.com/questions/3943/usage-displays-properly-only-after-second-call/27671#27671*)
System`Dump`fixmessagestring[System`Dump`s_]:=ToString@InputForm@System`Dump`s


ComsolImport::usage = 
	"ComsolImport[file] imports the .txt file with table results exported by COMSOL.
	 ComsolImport[file, \"Headers\"] imports column headers of the table.
	 ComsolImport[file, \"Info\"] gives the additional information stored in the file."
	 
Begin["`Private`"]

(*Helper methods*)
(* count of comment lines in exported file *)
CountCommentLines[file_String] := Module[{st, line, i},
	st = OpenRead[file];
	line = Read[st, String];
	i = 1;
	While[ StringMatchQ[line, StartOfString ~~ "%" ~~ __] && line =!= EndOfFile, 
		line = Read[st, String];
		i = i + 1;
	];
	Close[st];
	i
]

ComsolImport[file_String?FileExistsQ] := ComsolImport[file, "Table"] ;

(* import file, but skip unnecessary information *)
ComsolImport[file_String?FileExistsQ, "Table"] := Module[{impStr},
	(*read the input as text and change the number format to Matemathica-readable*)
	impStr=StringReplace[Import[file, "Plaintext"],{"i"->"*I","E"->"*10^"}];
	ToExpression[ImportString[impStr, "Table"][[CountCommentLines[file] ;; ]]] 
]

(* import only column headers *)
ComsolImport[file_String?FileExistsQ, "Headers"] := 
	ImportString[ StringDrop[ 
		Import[file, {"Text", "Lines", CountCommentLines[file] -1}],
	1], "Table" , "FieldSeparators"-> "  "];

(* get files information *)
ComsolImport[file_String?FileExistsQ, "Info"|"Information"] := 
	StringDrop[# , 1] &/@ Import[file, {"Text", "Lines", Range[CountCommentLines[file] -2]}] ;
	
End[]


SplitParametricSweepTable::usage = "SplitParametricSweepTable[list_,nPars_]
	Transform table {{\!\(\*SubscriptBox[\(s\), \(1\)]\), \!\(\*SubscriptBox[\(s\), \(2\)]\), ..., \!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(x\), \(2\)]\),...},...} to a new form {\!\(\*SubscriptBox[\(s\), \(1\)]\), {..{\!\(\*SubscriptBox[\(s\), \(n\)]\),{\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(x\), \(2\)]\),...},...}...}} 
	where each of the parameters \!\(\*SubscriptBox[\(s\), \(1\)]\) runs over its own list dimension. nPar_ is the number of parameters \!\(\*SubscriptBox[\(s\), \(i\)]\)"

SplitParametricSweepTable[list_?MatrixQ, 1]:=Module[{par1ValList},
	par1ValList=DeleteDuplicates[list[[;;,1]]];
	Table[
		{par1,Select[list,(#[[1]]==par1)&][[;;,2;;]]},
		{par1,par1ValList}
	]
]
SplitParametricSweepTable[list_?MatrixQ,nPars_?((#>1)&)]:=Module[{par1ValList,subList},
	par1ValList=DeleteDuplicates[list[[;;,1]]];
	Table[
		subList=Select[list,(#[[1]]==par1)&][[;;,2;;]];
		{par1,SplitParametricSweepTable[subList, nPars-1]},
		{par1,par1ValList}
	]
]


SplitByModes::usage = "SplitByModes[list_]"
SplitByModes[list_]:=Module[{par1, nModes},
	par1=list[[1,1]];
	nModes=Count[list,{par1,__},1];
	Table[Take[list,{i,-1,nModes}],{i,1,nModes}]
]