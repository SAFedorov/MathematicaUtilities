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

AreaXY::usage=""

ListIntegrate::usage="ListIntegrate[xylist_, intRange_]"

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


ListIntegrate[xylist_,intRange_]:=Module[{interpData,intRangeExt,x0,\[CapitalDelta]x,\[CapitalDelta]xMin,x},
	(*Function integrates data over the int Range using interpolation*)
	\[CapitalDelta]xMin=xylist[[2,1]]-xylist[[1,1]];
	{x0,\[CapitalDelta]x}={Mean[intRange],intRange[[2]]-intRange[[1]]};
	intRangeExt={x0-Max[2\[CapitalDelta]x,\[CapitalDelta]xMin],x0+Max[2\[CapitalDelta]x,\[CapitalDelta]xMin]};
	interpData=Interpolation[Select[xylist,InRangeQ[#[[1]],intRangeExt]&]];
	Integrate[interpData[x],{x,intRange[[1]],intRange[[2]]}]
]


AreaXY[xylist_,intRange_]:=Module[{tmpData},
	(*Function integrates data over the int Range using sum over elements*)
	tmpData=Select[xylist,InRangeQ[#[[1]],intRange]&];
	tmpData[[;;-2,2]].Differences[tmpData[[;;,1]]]+tmpData[[-1,2]]*(intRange[[2]])
]


(*reflects XYData about x0 in x-direction*)
ReflectX[xylist_,x0_]:=Transpose[ {2x0-xylist[[;;,1]],xylist[[;;,2]]}]


AverageXYLists[xylists_]:=Module[{nMin,nLists,ylist},
	nMin=Min[Map[Length,xylists]](*find minimum trace length*);
	nLists=Length[xylists];
	ylist=Total[xylists[[;;,;;nMin,2]],{1}]/nLists;
	Transpose[{xylists[[1,;;nMin,1]],ylist}]
]


(* ::Subsection:: *)
(*Batch data processing*)


(* ::Subsubsection::Closed:: *)
(*Fitting*)


(* ::Input::Initialization:: *)
(**;
Function for fitting the homodyne fringes ("fringeTraceList") with sine function within "fitRanges" intervals;
"fitRanges" is a list of intervals for each trace.
**)
FringesFit[fringeTraceList_,fitRanges_,OptionsPattern[{DCBlock->10}]]:=Module[{B0,A0,x00,\[Nu]0,tmpData,ftTmp},
Table[
tmpData=Select[fringeTraceList[[i]],InRangeQ[#[[1]],fitRanges[[i]]]&];
A0=(Max[tmpData[[;;,2]]]-Min[tmpData[[;;,1]]])/2;
B0=(Max[tmpData[[;;,2]]]+Min[tmpData[[;;,1]]])/2;
x00=tmpData[[Ordering[tmpData[[;;,2]],-1][[1]],1]];
ftTmp=Select[Abs[\[Omega]FourierD[tmpData]],#[[1]]>OptionValue[DCBlock]&];(*exclude DC offset*)
\[Nu]0=ftTmp[[Ordering[ftTmp[[1;;,2]],-1][[1]],1]];
FindFit[tmpData,A Cos[\[Nu] (x-x0)]+B,{{A,A0},{B,B0},{\[Nu],\[Nu]0},{x0,x00}},x],{i,Length[fringeTraceList]}]
]


(* ::Subsection:: *)
(*Misc*)


PhaseUnwrap[list_,OptionsPattern[{jumpThreshold-> 1.8\[Pi]}]]:=
Module[{tmpPhase,dim},
	dim=Depth[list];
	Which[
		dim==3,
		tmpPhase=0;
		Table[{x[[1]],tmpPhase=Which[
			x[[2]]-tmpPhase>OptionValue[jumpThreshold],x[[2]]-2\[Pi],
			x[[2]]-tmpPhase<-OptionValue[jumpThreshold],x[[2]]+2\[Pi],
			True,x[[2]]
			]},
		{x,list}]
		,
		dim==4,
		Table[
			tmpPhase=0;
			Table[
				{x[[1]],tmpPhase=Which[
					x[[2]]-tmpPhase>OptionValue[jumpThreshold],x[[2]]-2\[Pi],
					x[[2]]-tmpPhase<-OptionValue[jumpThreshold],x[[2]]+2\[Pi],
					True,x[[2]]
					]},
			{x,y}],
		{y,list}]
	]
];


(* ::Subsection:: *)
(*Experiment-specific functions*)


(* ::Subsubsection::Closed:: *)
(*Heating processing*)


Options[TFromAreaList]={
	\[CapitalGamma]List->False,
	calToneAList->False,
	BootstrapUsingFit->False
}
TFromAreaList[AList_,pList_,T0_,OptionsPattern[{TFromAreaList}]]:=Module[{ARef,pAFit,pAList,AScale,calToneCorrList,BACorrList},
	(*
	Area(P) linearly extrapolated to 0 power where it is set to T0.;
	Calibration tone option is sed for better normalization of the area under peaks, to exclude the effects of locking point.
	*)
	ARef=AList[[1]];
	
	If[OptionValue[calToneAList]=!=False,
		calToneCorrList=OptionValue[calToneA][[1]]/OptionValue[calToneA],
		calToneCorrList=Table[1,Length[AList]]
	];
	If[OptionValue[\[CapitalGamma]List]=!=False,
		BACorrList=Abs[OptionValue[\[CapitalGamma]List]/OptionValue[\[CapitalGamma]List][[1]]]^2,
		BACorrList=Table[1,Length[AList]]
	];
	pAList=Table[{pList[[i]],calToneCorrList[[i]]BACorrList[[i]] AList[[i]]/ARef},{i,Length[AList]}];
	pAFit=FindFit[pAList,a T+b,{a,b},T];
	AScale=If[OptionValue[BootstrapUsingFit],(T0/b/.pAFit),T0];
	{Transpose[{pAList[[;;,1]],pAList[[;;,2]]AScale}],{b/AScale,a/AScale}/.pAFit}
];


(* ::Subsubsection::Closed:: *)
(*Homodyne signal asymmetry-related*)


GetAsymmetrySpectrum[spectrum_,peakRange_,nAvg_:1]:=Module[{\[Nu]Peak,spectrumSel,spectrumInterp,spectrumRefl,spectrumSelAvg,spectrumReflAvg},
	\[Nu]Peak=FindPeakCenter[Select[spectrum,InRangeQ[#[[1]],peakRange]&]];

	(*select only thouse frequency points for which both original and reflected about \[Nu]Peak spectra have specified values*)
	spectrumSel=Select[spectrum,InRangeQ[#[[1]],{Max[spectrum[[1,1]],2 \[Nu]Peak-spectrum[[-1,1]]],Min[spectrum[[-1,1]],2 \[Nu]Peak-spectrum[[1,1]]]}]&];
	
	(*reflect about \[Nu]Peak*)
	spectrumRefl=Transpose[{spectrumSel[[;;,1]],Map[Interpolation[spectrum],2 \[Nu]Peak-spectrumSel[[;;,1]]]}];

	If[nAvg!=1,
		spectrumSelAvg=Average[spectrumSel,nAvg];
		spectrumReflAvg=Average[spectrumRefl,nAvg];
		Transpose[{spectrumSelAvg[[;;,1]]-\[Nu]Peak,(spectrumSelAvg[[;;,2]]-spectrumReflAvg[[;;,2]])/(spectrumSelAvg[[;;,2]]+spectrumReflAvg[[;;,2]])}],
		
		Transpose[{spectrumSel[[;;,1]]-\[Nu]Peak,(spectrumSel[[;;,2]]-spectrumRefl[[;;,2]])/(spectrumSel[[;;,2]]+spectrumRefl[[;;,2]])}]
	]
]


(* ::Section::Closed:: *)
(*End*)


End[]
EndPackage[]
