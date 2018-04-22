(* ::Package:: *)

(* ::Section::Closed:: *)
(*Description*)


(* ::Section:: *)
(*Begin*)


(* ::Subsection::Closed:: *)
(*BeginPackage[], set directory and list data directories, import other packages*)


BeginPackage["Utilities`"]

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
(*Global definition*)


(* ::Subsubsection::Closed:: *)
(*Sets of settings*)


compilationOptionsC::usage=""


(* ::Subsubsection::Closed:: *)
(*Patterns*)


(*Tests if object is a function, taken from http://stackoverflow.com/questions/3736942/test-if-an-expression-is-a-function*)
FunctionQ[_Function|_InterpolatingFunction|_CompiledFunction]=True;
FunctionQ[f_Symbol]:=Or[DownValues[f]=!={},MemberQ[Attributes[f],NumericFunction]]
FunctionQ[_]=False;


(*Tests if the argument is a vector or array of numbers*)
NumericVectorQ[expr_]:=VectorQ[expr,NumericQ];
NumericArrayQ[expr_]:=ArrayQ[expr,_,NumericQ];


notOptPatt=Except[_?OptionQ]


(*differentiate between 2D and 3D datasets*)
XYListQ[list_]:=ArrayQ[list,2]&&(Dimensions[list][[2]]==2);

ListOfXYListsQ[list_]:=ListQ[list]&& AllTrue[list,XYListQ];

XYZListQ[list_]:=ArrayQ[list,2]&&(Dimensions[list][[2]]==3);


(* ::Subsection:: *)
(*List of all functions in the package*)


(* ::Subsubsection::Closed:: *)
(*General*)


(* an xylist is a list of the form {{x1,y1},...,{xn,yn}} *)
(* an xyzlist is a list of the form {{x1,y1,z1},...,{xn,yn,zn}} *)
(* a spectra is an xylist, where the xlist is frequency [Hz] *)

SetRAMRestriction::usage="SetRAMRestriction[maxMemAllowedGB:2,intervalBetweenTests:5]
	Set a restriction on RAM usage by Mathematica to prevent hanging. maxMemAllowedGB is the RAM space in gigabytes and intervalBetweenTests is the period in seconds"

RemoveRAMRestriction::usage="RemoveRAMRestriction[]
	Switch the RAM restriction off by removing all the scheduled tasks. To remove only specific task use RemoveScheduledTask with return argument from SetRAMRestriction."

DefaultHeaderPrint::usage="DefaultHeaderPrint[] prints a cell with commands, frequently used on startup"

InRangeQ::usage="InRangeQ[x_, range_, IncludeBoundary\[Rule]True]"

PhaseUnwrap::usage="PhaseUnwrap[list_, jumpThreshold\[Rule]1.8\[Pi]]"


(* ::Subsubsection:: *)
(*Manipulation with multi-dimensional lists *)


(*Useful functions adopted from the V.Sudhir's He3Analysis package;
The function names are self-explanatory, the two instances are different in the argument type.*)
MapX::usage="MapX[f_, list_] applies f to the x coordinate of the list. list_ can be either 2D of 3D, {{x,y},...} or {{x,y,z},...}"
MapY::usage="MapY[f_, list_] applies f to the y coordinate of the list. list_ can be either 2D of 3D, {{x,y},...} or {{x,y,z},...}"
MapXY::usage="MapXY[fx_,fy_,list_] applies fx to the x coordinate and fy to the y coordinate of the list. 
	list_ can be either 2D of 3D, {{x,y},...} or {{x,y,z},...}"
MapZ::usage="MapZ[f_, list_] applies f to the z coordinate of the list"

ScaleY::usage="Function rescales data along the y coordinate by the factor of a_
	Also, a shift in y-dimension, or scaling and shift in x dimension can be specified by options ShiftY, ScaleX and ShiftX
	Output is calculated as: {x,y}\[Rule]{ScaleX\[Times](x+ShiftX),a\[Times](y+ShiftY)}

	Input list_ can be either xylist of a set of xylist's 

	a_ is a constant for single-list input, or may be a list of constants in the case of multiple list input.
	In the latter case Subscript[y, i]^j is scaled by Subscript[a, j]

	ScaleX and ShiftX can be constant only

	ShiftY can be a constant, a list or a xylist.
	In the case of list Subscript[ShiftY, i] is added to the Subscript[y, i] of the data
	In the case of a xylist, the y-values of this list are added to the y-values of the data.
"
	
NormalizeY::usage="NormalizeY[xylist_]
	Function accepts 2D array of data (xylist) and rescales it along Y coordinate to be within [0,1]"

Average::usage="Average[list_, nAvg_]
	Function calculates moving average of the list and does a corresponding decimation of the list.
	The output thus consists of averages over successive intervals of nAvg elements. 
	The X-values for each of the averaging intervals are taken close to the centers of the interval\[IndentingNewLine]
	list_ can be
	1. 1D list
	2. xylist
	3. list of xylists"
	
GaussianAverage::usage="GaussianAverage[list_, nAvg_]"
	
FindPeaksXY::usage="FindPeaksXY[xylist_, \[Sigma]_:0, s_:0, t_:-\[Infinity]]"

FindPeakCenter::usage="FindPeakCenter[xylist_]"

ReflectX::usage=""

AreaXY::usage=""

ListIntegrate::usage="ListIntegrate[xylist_, intRange_]"

AverageXYLists::usage="AverageXYLists[xylists_]
	Computes average of multiple lists"


(* ::Subsubsection::Closed:: *)
(*Interval manipulations*)


IntervalComplement::usage="IntervalComplement[a_,b_,c_,..]  computes a\[Backslash](b\:222ac\:222a\[Ellipsis])"

IntervalInverse::usage="IntervalInverse[a_] computes the complement (\[Minus]\[Infinity],\[Infinity])/a"


(* ::Subsubsection::Closed:: *)
(*Series data processing*)


LoadDataSeries::usage="LoadDataSeries[namePattern_,parNamesList_:{x}]
	Input: namePattern, containing varibles which values are being swept and parNamesList that lists the variables.
	In the case of single variable named x parNamesList may be omitted.  
	Example: LoadDataSeries[x__~~\" nW pickoff.txt\"]

	Output: {parList, dataList}, where dataList[[i]] corresponds to the content of the file with name including parList[[i]]
	parList={\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(x\), \(2\)]\), ... } in the case of single parameter and {{\!\(\*SubscriptBox[\(x\), \(1\)]\),\!\(\*SubscriptBox[\(y\), \(1\)]\),\!\(\*SubscriptBox[\(z\), \(1\)]\)},..} in the case of multiple
	
	Options:
	By default the measurement files are loaded as Import[fileName,\"Table\"], custom loading function can be specified using FileLoadingFunction option, 
	which should return data if applied to file name
	By default the parameters are interpreted as numbers, if they should remain in text format, the InterpretParameter option should be set to False. "

LoadSpeParameters::usage="LoadSpeParameters[namePattern_, keyword_]
	Function loads parameters given by keyword from .spe files with names matching namePattern"

SelectSeries::usage="[xylist_,xRange_]"



(* ::Subsubsection::Closed:: *)
(*Plots*)


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


(* ::Subsection::Closed:: *)
(*List of options to functions*)


ScaleX::usage=""
ShiftX::usage=""
ShiftY::usage=""


(* ::Section:: *)
(*Body*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*Sets of settings*)


compilationOptionsC=Sequence[
	CompilationTarget->"C",
	CompilationOptions->{"InlineExternalDefinitions"->True},
	RuntimeOptions->{"EvaluateSymbolically"->False}
];


(* ::Subsection::Closed:: *)
(*RAM usage restriction*)


SetRAMRestriction[maxMemAllowedGB_:2,intervalBetweenTests_:5]:=(
	iAmAliveSignal=0;
	Print[Dynamic[iAmAliveSignal]];
	RunScheduledTask[If[MemoryInUse[]>(maxMemAllowedGB 1024^3),Quit[],iAmAliveSignal++],intervalBetweenTests]
)

RemoveRAMRestriction[]:=RemoveScheduledTask[ScheduledTasks[]];


(* ::Subsection::Closed:: *)
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


(*Useful functions adopted from the V.Sudhir's He3Analysis package;
The function names are self-explanatory, the two instances are different in the argument type.*)
MapX[f_,list_?XYListQ]:=Map[{f[#[[1]]],#[[2]]}&,list];
MapX[f_,list_?XYZListQ]:=Map[{f[#[[1]]],#[[2]],#[[3]]}&,list];

MapY[f_,list_?XYListQ]:=Map[{#[[1]],f[#[[2]]]}&,list];
MapY[f_,list_?XYZListQ]:=Map[{#[[1]],f[#[[2]]],#[[2]]}&,list];

MapXY[fx_,fy_,list_?XYListQ]:=Map[{fx[#[[1]]],fy[#[[2]]]}&,list];
MapXY[fx_,fy_,list_?XYZListQ]:=Map[{fx[#[[1]]],fy[#[[2]]],#[[3]]}&,list];

MapZ[f_,xyzlist_]:=Map[{#[[1]],#[[2]],f[#[[3]]]}&,xyzlist];


(**;
Function rescales data along the y coordinate by the factor of a_;
Also, a shift in y-dimension, or scaling and shift in x dimension can be specified by options ShiftY, ScaleX and ShiftX;
Output is calculated as: {x,y}\[Rule]{ScaleX\[Times](x+ShiftX),a\[Times](y+ShiftY)};

Input list_ can be either xylist of a set of xylist's 

a_ is a constant for single-list input, or may be a list of constants in the case of multiple list input.;
In the latter case Subscript[y, i]^j is scaled by Subscript[a, j];

ScaleX and ShiftX can be constant only;

ShiftY can be a constant, a list or a xylist.;
In the case of list Subscript[ShiftY, i] is added to the Subscript[y, i] of the data;
In the case of a xylist, the y-values of this list are added to the y-values of the data.;
**)
ScaleY[xylist_?XYListQ, a:Except[_?OptionQ]:1, OptionsPattern[{ScaleX->1,ShiftX->0,ShiftY->0}]]:=Module[{xSc,ySc,xSh,ySh},
	xSc=OptionValue[ScaleX];
	ySc=a;
	xSh=OptionValue[ShiftX];
	ySh=OptionValue[ShiftY];
	
	Which[
		ArrayQ[ySh,2]&&(Length[ySh]==Length[xylist]),
			Transpose[{xSc (xylist[[;;,1]]+xSh),ySc (xylist[[;;,2]]+ySh[[;;,2]])}],
		True,
			Transpose[{xSc (xylist[[;;,1]]+xSh),ySc (xylist[[;;,2]]+ySh)}]
	]
]

ScaleY[list_?ListOfXYListsQ,a:Except[_?OptionQ]:1,opts:OptionsPattern[{ScaleX->1,ShiftX->0,ShiftY->0}]]:=Module[{},
	If[ListQ[a],
		Table[ScaleY[list[[i]],a[[i]],opts],{i,Length[list]}],
		Table[ScaleY[xylist,a,opts],{xylist,list}]
	]
]


(**
Function takes 2D array of data (list_) 
and rescales it along Y coordinate to be within [0,1]
**)
NormalizeY[list_]:=
	Module[{ymin,ymax},
	ymin=Min[list[[;;,2]]];
	ymax=Max[list[[;;,2]]];
	Transpose[{list[[;;,1]],(list[[;;,2]]-(ymax+ymin)/2) 2/(ymax-ymin)}]
];


Average[list_,nAvg_]:=
(**
Function calculates moving average of the list and does a corresponding decimation of the list_.;
The output thus consists of averages over successive intervals of nAvg elements.; 

list_ can be;
1. a 1D list;
2. a XY trace;
3. a list of XY traces;
In the latter two cases an appropriate the X-values for each of the averaging intervals are taken close to the centers of the interval;
**)
Which[
	Depth[list]==2, 
	(*single 1D list input*)
	Take[MovingAverage[list,nAvg],{1,-1,nAvg}]
	,
	Depth[list]==3, 
	(*single XY trace input*)
	Take[Transpose[{list[[Ceiling[nAvg/2];;-Floor[nAvg/2]-1,1]],MovingAverage[list[[;;,2]],nAvg]}],{1,-1,nAvg}]
	,
	Depth[list]==4, 
	(*list of XY traces as input*)
	Table[Take[Transpose[{x[[Ceiling[nAvg/2];;-Floor[nAvg/2]-1,1]],MovingAverage[x[[;;,2]],nAvg]}],{1,-1,nAvg}],{x,list}]
]


GaussianAverage[list_,nAvg_]:=
(**
Function calculates the Gaussian filtered signal;
The output thus consists of averages over successive intervals of nAvg elements.; 

list_ can be;
1. a 1D list;
2. a XY trace;
3. a list of XY traces;
In the latter two cases an appropriate the X-values for each of the averaging intervals are taken close to the centers of the interval;
**)
Which[
	Depth[list]==2, 
	(*single 1D list input*)
	GaussianFilter[list,nAvg]
	,
	Depth[list]==3, 
	(*single XY trace input*)
	Transpose[{list[[;;,1]],GaussianFilter[list[[;;,2]],nAvg]}]
	,
	Depth[list]==4, 
	(*list of XY traces as input*)
	Table[Transpose[{x[[;;,1]],GaussianFilter[x[[;;,2]],nAvg]}],{x,list}]
]


FindPeaksXY[XYData_,\[Sigma]:Except[_?OptionQ]:0,s:Except[_?OptionQ]:0,t:Except[_?OptionQ]:-\[Infinity],opts:OptionsPattern[{FindPeaks,sign-> 1}]]:=
Module[{peakIndexList,peakValList,xInterp},
	{peakIndexList,peakValList}=Transpose[FindPeaks[Sign[OptionValue[sign]]*XYData[[;;,2]],\[Sigma],s,t]];
	xInterp=Interpolation[XYData[[;;,1]],InterpolationOrder->OptionValue[InterpolationOrder]];

	Transpose[{Thread[xInterp[peakIndexList]],peakValList}]
];


FindPeakCenter[XYData_,OptionsPattern[sign-> 1]]:=Module[{YIntegralList,YIntegralInterpol,iCOM,offset,dim},
(*Function accepts an interval of XY data and returns x-position of its center of mass, using linear data interpolation over x*)

dim=Depth[XYData];
Which[
dim==3 (*single XY trace*),
offset=If[Sign[OptionValue[sign]]>=0,Min[XYData[[;;,2]]],Max[XYData[[;;,2]]]];
YIntegralList=Accumulate[XYData[[;;,2]]-offset];
YIntegralInterpol=Interpolation[YIntegralList/YIntegralList[[-1]],InterpolationOrder->1];

(*fractional index of the Center of Mass*)
(iCOM=0.5+(x/.FindRoot[YIntegralInterpol[x]==0.5,{x,Length[YIntegralList]/2}]))//Quiet;
1/(Ceiling[iCOM]-Floor[iCOM]) (XYData[[Floor[iCOM],1]](Ceiling[iCOM]-iCOM)+XYData[[Ceiling[iCOM],1]](iCOM-Floor[iCOM]))

,
dim==4 (*list of XY traces*),
Table[
offset=If[Sign[OptionValue[sign]]>=0,Min[y[[;;,2]]],Max[y[[;;,2]]]];
YIntegralList=Accumulate[y[[;;,2]]-offset];
YIntegralInterpol=Interpolation[YIntegralList/YIntegralList[[-1]],InterpolationOrder->1];

(*fractional index of the Center of Mass*)
(iCOM=0.5+(x/.FindRoot[YIntegralInterpol[x]==0.5,{x,Length[YIntegralList]/2}]))//Quiet;
1/(Ceiling[iCOM]-Floor[iCOM]) (y[[Floor[iCOM],1]](Ceiling[iCOM]-iCOM)+y[[Ceiling[iCOM],1]](iCOM-Floor[iCOM]))
,{y,XYData}]
]
]


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


(* ::Subsection::Closed:: *)
(*Plotting*)


(* ::Subsection::Closed:: *)
(*Interval manipulations*)


(* ::Input:: *)
(*(*taken from http://mathematica.stackexchange.com/questions/11345/can-mathematica-handle-open-intervals-interval-complements*)*)


(*computes the complement (\[Minus]\[Infinity],\[Infinity])/a.*)
IntervalInverse[Interval[int___]]:=Interval@@Partition[
	Flatten@{int}/.{{-\[Infinity],mid___,\[Infinity]}:>{mid},{-\[Infinity],mid__}:>{mid,\[Infinity]},{mid__,\[Infinity]}:>{-\[Infinity],mid},{mid___}:>{-\[Infinity],mid,\[Infinity]}},2]


(*IntervalComplement[a,b,c,..] computes a\[Backslash](b\:222ac\:222a\[Ellipsis])*)
IntervalComplement[a_Interval,b__Interval]:=IntervalIntersection[a,IntervalInverse@IntervalUnion[b]]



(* ::Subsection:: *)
(*Batch data processing*)


(* ::Subsubsection::Closed:: *)
(*Loading data*)


LoadDataSeries[namePattern_,parNamesList:Except[_?OptionQ]:{Global`x},OptionsPattern[{InterpretParameter->True,FileLoadingFunction->Automatic,BaseDirectory->Directory[]}]]:=Module[{fileNamesList,dir,FLF,parList,dataList,ret},
	dir=OptionValue[BaseDirectory];
	fileNamesList=FileNames[namePattern,dir,Infinity](*read file names and extract parameters from them*);
	(*parList=Flatten[ToExpression@StringCases[fileNamesList,namePattern->parNamesList],1];*)
	parList=Table[
		If[OptionValue[InterpretParameter],
			Interpreter["Number"][StringCases[fileName,namePattern->parNamesList][[1]]],
			StringCases[fileName,namePattern->parNamesList][[1]]
		],
		{fileName,fileNamesList}];
	
	(*read data from the files*)
	If[FunctionQ[OptionValue[FileLoadingFunction]],
		FLF=OptionValue[FileLoadingFunction],
		FLF=(Import[#,"Table"]&)];
	dataList=Table[FLF[fileName],{fileName,fileNamesList}];
	
	(*sort parameters and data sets according to increase in the first parameter in parList*)
	ret=Transpose@Sort[Transpose[{parList,dataList}],#1[[1,1]]<#2[[1,1]]&];
	
	(*If only one parameter, flatten the parList*)
	If[Length[parNamesList]>1,ret,{Flatten[ret[[1]]],ret[[2]]}]
]


LoadSpeParameters[namePattern_, keyword_,fileParNamesList:Except[_?OptionQ]:{Global`x},OptionsPattern[{InterpretParameter->True}]]:=Module[{fileNamesList,tmpData,fileParList,speParList,ret},
	fileNamesList=FileNames[namePattern,Directory[],Infinity];
	fileParList=Table[
		If[OptionValue[InterpretParameter],
			Interpreter["Number"][StringCases[fileName,namePattern->fileParNamesList][[1]]],
			StringCases[fileName,namePattern->fileParNamesList][[1]]
		],
	{fileName,fileNamesList}];
	speParList={};
	Do[
		tmpData=Import[x,"Table"];
		AppendTo[speParList,FirstCase[tmpData,{keyword,_,_}][[-1]]],
	{x,fileNamesList}];
	ret=Sort[Transpose[{fileParList,speParList}],#1[[1,1]]<#2[[1,1]]&]; (*sorting according to increase in the first file name parameter*)
	ret[[;;,2]]
]


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


(* ::Subsubsection::Closed:: *)
(*Integration and selection*)


SelectSeries[xylist_,xRange_]:=Table[Select[x,((#[[1]]>=xRange[[1]])&&(#[[1]]<=xRange[[2]]))&],{x,xylist}];


(* ::Subsection::Closed:: *)
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


InRangeQ[x_,range_,OptionsPattern[{IncludeBoundary->True}]]:=
Module[{upCompFunc,lowCompFunc,ibOpt},
(**;
Function returns True or False depending on whever x_ belongs to the interval range_={Subscript[x, min],Subscript[x, max]}.;
Usin the option IncludeBoundary one can regulate if the boundary points are included into the interval. Option can be a single logical value or a list of 2 logical values (e.g. {True,False}), related to the low and high boundary correspondingly.
**)
	ibOpt=OptionValue[IncludeBoundary];
	Which[
		Head[ibOpt]===Symbol,
		lowCompFunc=If[ibOpt===True,GreaterEqual,Greater];
		upCompFunc=If[ibOpt===True,LessEqual,Less];
	,
		Head[ibOpt]===List,
		lowCompFunc=If[ibOpt[[1]]===True,GreaterEqual,Greater];
		upCompFunc=If[ibOpt[[2]]===True,LessEqual,Less];
	];
	lowCompFunc[x,range[[1]]]&&upCompFunc[x,range[[2]]]
];


(* ::Subsection::Closed:: *)
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
