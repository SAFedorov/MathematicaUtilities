(* ::Package:: *)

(* ::Section:: *)
(*Description*)


(* ::Section:: *)
(*Begin*)


(* ::Subsection:: *)
(*BeginPackage[], set directory and list data directories, import other packages*)


BeginPackage["Utilities`",{"Utilities`DataAnalysis`","Utilities`Fourier`"}]

(* adds current notebook path to mathematica $PATH, if not already present *)
(* in general, this should be executed once from a notebook, from inside the directory that
contains this file, so that He3Analysis can be loaded *)
(* 
If[
	Nor@@Table[NotebookDirectory[]==$Path[[i]],{i,1,Length[$Path]}],
	AppendTo[$Path,NotebookDirectory[]];
]
*)

(*A fix for usage messages, from http://mathematica.stackexchange.com/questions/3943/usage-displays-properly-only-after-second-call/27671#27671*)
System`Dump`fixmessagestring[System`Dump`s_]:=ToString@InputForm@System`Dump`s


(* ::Subsection:: *)
(*List of all functions in the package*)


(* ::Subsubsection:: *)
(*General*)


(* an xylist is a list of the form {{x1,y1},...,{xn,yn}} *)
(* an xyzlist is a list of the form {{x1,y1,z1},...,{xn,yn,zn}} *)
(* a spectra is an xylist, where the xlist is frequency [Hz] *)

SetRAMRestriction::usage="SetRAMRestriction[maxMemAllowedGB:2,intervalBetweenTests:5]
	Set a restriction on RAM usage by Mathematica to prevent hanging. maxMemAllowedGB is the RAM space in gigabytes and intervalBetweenTests is the period in seconds"

RemoveRAMRestriction::usage="RemoveRAMRestriction[]
	Switch the RAM restriction off by removing all the scheduled tasks. To remove only specific task use RemoveScheduledTask with return argument from SetRAMRestriction."

DefaultHeaderPrint::usage="DefaultHeaderPrint[] prints a cell with commands, frequently used on startup"

PhaseUnwrap::usage="PhaseUnwrap[list_, jumpThreshold\[Rule]1.8\[Pi]]"


(* ::Subsubsection:: *)
(*Manipulation with multi-dimensional lists *)


ReflectX::usage=""

AverageXYLists::usage="AverageXYLists[xylists_]
	Computes average of multiple lists"


(* ::Subsubsection::Closed:: *)
(*Experiment-specific*)


TFromAreaList::usage="TFromAreaList[AList_,pList_,T0_]
	Converts the signal area vs power dependence to T vs power by assuming that the T=T0 at zero power and that T\[Proportional] signal area.

	Output:
	{{{p,T},...},{c1,c2}}, where the fitted dependence is c1+c2\[Times]T

	Options:
	\[CapitalGamma]List\[Rule]False,
	calToneAList->False,
	BootstrapUsingFit\[Rule]False
"
GetAsymmetrySpectrum::usage="GetAsymmetrySpectrum[spectrum_,peakRange_,nAvg_:1]
Returns the the list {{\[CapitalDelta]\[Nu],\!\(\*FractionBox[\(S[\*SubscriptBox[\(\[Nu]\), \(m\)] + \[CapitalDelta]\[Nu]] - S[\*SubscriptBox[\(\[Nu]\), \(m\)] - \[CapitalDelta]\[Nu]]\), \(S[\*SubscriptBox[\(\[Nu]\), \(m\)] + \[CapitalDelta]\[Nu]] + S[\*SubscriptBox[\(\[Nu]\), \(m\)] - \[CapitalDelta]\[Nu]]\)]\)},..}, characterizing asymmetry of the homodyne thermomechanical spectrum and thus the magnitude of optomechanical correlations 
Peak-to-peak variation of the ratio defined above is 2\!\(\*SqrtBox[\(\[Eta]\\\ C/\*SubscriptBox[\(n\), \(phon\)]\)]\), 
where \[Eta] is detection efficiency, is the optomechanical multi-photon cooperativity and \!\(\*SubscriptBox[\(n\), \(phon\)]\) is the phonon number \!\(\*SubscriptBox[\(n\), \(phon\)]\)=\!\(\*SubscriptBox[\(n\), \(th\)]\)+C+\!\(\*SubscriptBox[\(n\), \(classical\\\ heating\)]\)
"


(* ::Section:: *)
(*Body*)


Begin["`Private`"]


(* ::Subsection:: *)
(*RAM usage restriction*)


SetRAMRestriction[maxMemAllowedGB_:2,intervalBetweenTests_:5]:=(
	iAmAliveSignal=0;
	Print[Dynamic[iAmAliveSignal]];
	RunScheduledTask[If[MemoryInUse[]>(maxMemAllowedGB 1024^3),Quit[],iAmAliveSignal++],intervalBetweenTests]
)

RemoveRAMRestriction[]:=RemoveScheduledTask[ScheduledTasks[]];


(* ::Subsection:: *)
(*Frequently used cells*)


DefaultHeaderPrint[]:=CellPrint[
	Cell[CellGroupData[{Cell["Header","Section",GeneratedCell->True,CellAutoOverwrite->True],
	Cell[BoxData[RowBox[{"SetDirectory","[",RowBox[{"NotebookDirectory","[","]"}],"]"}]],"Input",GeneratedCell->True,CellAutoOverwrite->True],
	Cell[BoxData[RowBox[{RowBox[{RowBox[{"Get","[","\"\<processing.wdx\>\"","]"}],";"}],RowBox[{"(*",RowBox[{"load"," ","notebook"," ","content"," ",RowBox[{"(",RowBox[{"variable"," ","devinitions"}],")"}]," ","from"," ","file"}],"*)"}]}]],"Input",GeneratedCell->True,CellAutoOverwrite->True],
	Cell[BoxData[RowBox[{RowBox[{RowBox[{"DumpSave","[",RowBox[{"\"\<processing.wdx\>\"",",","\"\<Global`\>\""}],"]"}],";"}]," ",RowBox[{"(*",RowBox[{"save"," ","notebook"," ","content"," ","to"," ","file"}],"*)"}]}]],"Input",GeneratedCell->True,CellAutoOverwrite->True],
	Cell[BoxData[RowBox[{RowBox[{"CreateCellIDs","[","]"}],";"}]],"Input",GeneratedCell->True,CellAutoOverwrite->True],
	Cell[BoxData[RowBox[{RowBox[{"SetRAMRestriction","[","]"}],";"}]],"Input",GeneratedCell->True]},Open]]
];


(* ::Subsection:: *)
(*Manipulations with multi-dimensional lists*)


(*reflects XYData about x0 in x-direction*)
ReflectX[xylist_,x0_]:=Transpose[ {2x0-xylist[[;;,1]],xylist[[;;,2]]}]


AverageXYLists[xylists_]:=Module[{nMin,nLists,ylist},
	nMin=Min[Map[Length,xylists]](*find minimum trace length*);
	nLists=Length[xylists];
	ylist=Total[xylists[[;;,;;nMin,2]],{1}]/nLists;
	Transpose[{xylists[[1,;;nMin,1]],ylist}]
]


(* ::Section::Closed:: *)
(*End*)


End[]
EndPackage[]
