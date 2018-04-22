(* ::Package:: *)

(* ::Section::Closed:: *)
(*Description*)


(* ::Subsection::Closed:: *)
(*Changes log*)


(*The package contains some useful functions for experimental data processing. 
last modified: 2017, Sergey Fedorov*)


(* ::Text:: *)
(*1.3: *)
(*	1. added function YNormalize[]*)
(*	2. added options XOffset and YOffsed added to  YScale[]*)
(*	3. added finction InRangeQ[]*)
(*	*)
(*1.4: *)
(*	1. Fourier Transform section is included*)
(*	2. FitRangeSelector[] function is added*)
(*	3. FitQualityCheck[] function added*)
(*	4. FringesFit[] function added*)
(*1.5:*)
(*	1. FindFitSeries[] added*)
(*	2. LogPlot[] option is added to FitRangeSelector and FitQualityCheck*)
(*	3. YScale[] option YSHift is modified to be able to work with lists of traces (intended application: individual non-constant backgrounds)*)
(*	4. In ReEvaluateCells[] function an option to print the command text is added (PrintCommands->True).*)
(*	5. Average[] function is added*)
(*	6. _PlotJ functions are modified to properly treat user-defined PlotStyle*)
(*	7. OneFromTwoSidedSpectrum[] function is added to Fourier section*)
(*1.6:*)
(*	1. Fourier transform section is modified. *)
(*		1.1 \[Nu],\[Omega]FourierD2 now return complex fourier transform, not modulus squared.*)
(*		1.2 Corresponding changes in \[Nu],\[Omega]FourierD and \[Nu],\[Omega]Fourier*)
(*	2. AreaTable[] is modified to use InRangeQ[] *)
(*1.7:*)
(*	1. ReEvaluateCells[] is changed to only print text by default without evaluating it*)
(*	2. In ReEvaluateCells[] the option PrintCommands is renamed to Print Expression.*)
(*	3. In ReEvaluateCells[] the option EvaluateExpressions is added*)
(*	4. XYPeakDetectp[] function is added*)
(*	5. In the Fourier transform section SymmetrizeSpectrum[] function is added*)
(*	6. Interval Operations section is created (copied from http://mathematica.stackexchange.com/questions/11345/can-mathematica-handle-	open-intervals-interval-complements).*)
(*		6.1 IntervalInverse[] function added*)
(*		6.2 IntervalComplement[] function added*)
(*	7. Function FindPeakCenter[] is added*)
(*	8. Function DefaultHeaderPrint[] is added*)
(*	9. SweepPlot function family is added*)
(*	10. FindLogFit[] function is added*)
(*	11. ListNIntegrate[] function is added*)
(*	12. FunctionQ[] is added*)
(*	13. NumericVecorQ[] and NumericArrayQ functions are added*)
(*1.8:*)
(*	1. XYPeakDetect[] is replaced with XYFIndPeak[]*)
(*	2. XReflect[] function is added*)
(*	3. Added color lists*)
(*	4. Color definitions are excluded from the DefaultHeaderPring[] *)
(*	5. In YScale[] the argument a is made optional*)
(*	6. AreaTable[] is replaced with XYArea*)
(*	7. GaussianAverage[] function is added*)
(*1.9:*)
(*	1. Function PlotGrid[] is copypasted*)
(*2.0:*)
(*	1. Transition to package form*)
(*	2. Naming convention change: xylist introduced*)
(*	3. CellReuseSequencePrint[] is introduced*)
(*	4. LoadDataSeries[] is updated to be able to handle names name patters with several variables *)
(*	5. GenerateGridLines[] is introduced*)
(*2.1:*)
(*	1. A  number of functions is adopted from the Vivishek Sudhir's He3Analysis package, including*)
(*	MapX,XY,Z*)
(*	2. XYListQ[] and XYZListQ[] function introduced to differentiate between 2D and 3D datasets*)
(*3.0: *)
(*	1. ListOfXYListsQ[] introduced*)
(*	2. ScaleY is introduced as a replacement to the YScale, slightly different in behavior and implementation*)
(*	3. \[Nu]FourierXY introduced to replace \[Nu]FourierD*)
(*	4. "Old" subsection is introduce to store the replaced functions for compatibility*)
(*	5. Naming for an umber of functions changed. Old definitions are kept for compatibility.*)
(*	6. LoadSpeParameters[] function is introduced*)
(*	7. FitRangeSelector[] is cleaned up and re-introduced*)
(*	8. TFromAreaList[] is introduced in the Heating processing section*)
(*	9. Section Experiment-specific functions is introduced*)
(*	10. GetAsymmetrySpectrum[] function is introduced*)


(* ::Subsection::Closed:: *)
(*To do*)


(* ::Text:: *)
(*- FitRangeSelector add proper boundaries for cursors*)
(*- add series moving averages*)
(*- add default plotting header for data processing*)
(*-  SweepListPlot[] function?*)


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

defaultPlotFrameOptions::usage=""

defaultPlotGridOptions::usage=""


(* ::Subsubsection::Closed:: *)
(*Colors*)


(*97-standard colors list*)
stdColors::usage="Mathematica >9.0 standard plotting colors list (ColorData[97,\"ColorList\"])"

lightColors::usage="ColorData[55,\"ColorList\"]"

redColors::usage="Table[
	Blend[{ColorData[2,\"ColorList\"][[3]],ColorData[2,\"ColorList\"][[1]],ColorData[5,\"ColorList\"][[1]]},x],
	{x,0,1,0.2}]"

blueColors::usage="Table[Blend[{CMYKColor[1,0,0,.25],CMYKColor[1,0.7,0,.25]},x],{x,0,1,0.2}]"

greenColors::usage="Table[Blend[{ColorData[38,\"ColorList\"][[3]],CMYKColor[1,0,1,.5]},x],{x,0,1,0.2}]"

yellowColors::usage="Table[
	Blend[{ColorData[50,\"ColorList\"][[6]],ColorData[50,\"ColorList\"][[2]],ColorData[50,\"ColorList\"][[8]]},x],
	{x,0,1,0.2}]"



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

CellReuseSequencePrint::usage="CellReuseSequencePrint[] prints a sequence of commands for re evaluation of cells with tag replacement rules"

CreateCellIDs::usage="CreateCellIDs[]"

ReEvaluateCells::usage="ReEvaluateCells[cellIDList,textReplacementRules,InputOnly\[Rule]False,PrintExpressions\[Rule]True,EvaluateExpressions\[Rule]False]"

GetCellIDButton::usage="GetCellIDButton[cellIDList]"

InRangeQ::usage="InRangeQ[x_, range_, IncludeBoundary\[Rule]True]"

PhaseUnwrap::usage="PhaseUnwrap[list_, jumpThreshold\[Rule]1.8\[Pi]]"

FindLogFit::usage="FindLogFit[data_, expr_, rest__]. The function is equivalent to FindFit, but works in log Y scale."

PlotExplorer::usage="PlotExplorer[plot_]"

PlotGrid::usage="PlotGrid[l_List, w_, h_]"

SweepListLogPlot::usage="SweepListLogPlot[plotData_,opts]"

GenerateGridLines::"usage"="GenerateGridLines[xMin_,xMax_,step_,OptionsPattern[]] or GenerateGridLines[xMin_,xMax_,step_,subStep_,OptionsPattern[]]
	The function is used to generate custom grid lines, possibly with different style for main and sub grid lines.
	In the case of exponential scaling, subStep is a multiplier for the first sub grid line with respect to the previous main one.
	Options: {MainGridLinesStyle\[Rule]Null, SubGridLinesStyle\[Rule]Null, LogScale\[Rule]False}"


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

FitRangeSelector::usage="FitRangeSelector[fitRanges_,traceList_,OptionsPattern[{logYPlot->False,logXPlot-> False}]]
	Function allows to select ranges for each element of the list of traces (presumably for future use in fitting)
	Left click - set lower x interval boundary at current mouse position; right click - set upper x interval boundary at current mouse position.
	fitRanges is an empty variable to which the list of ranges will be assigned."
	
FindFitSeries::usage="FindFitSeries[traceList_,fitModel_,pars_,vars_,fitRanges_:Full]
	Function applies FindFit[] to each element in the list of 2D data traces traceList.

	Input:
	fitModel, pars and vars are the corresponding elements for FindFit[]. fitModel and pars can be either a single element, or a list of corresponding elements for each trace in traceList.
	fitRanges is an optional argument, alowing pre-selection of the data to be fitted. It can be
	1. single range {Subscript[x, min],Subscript[x, max]}
	2. a list of ranges {\!\(\*SubsuperscriptBox[\(x\), \(i\), \(min\)]\),\!\(\*SubsuperscriptBox[\(x\), \(i\), \(max\)]\)} for every traceList[[i]]
	3. a single Interval
	4. a list of Interval's for every traceList[[i]]

	Output:
	list of the fit replacement rules 

	Options: FitFunction can be either FindFit (by default) of FindLogFit
"

FitQualityCheck::usage="FitQualityCheck[traceList_,fitRanges_,fitModel_,fitParameters_]
	Plots fits on top of the orifinal data.

	Input:
	fitModel needs to have x as free variable.;
	fitRanges should be either a single fitting interval for all the traces in traceList, or the list of individual fitting intervals for traceList[[i]]

	Options:
	logYPlot->False,
	logXPlot->False"


(* ::Subsubsection::Closed:: *)
(*Fourier Transform*)


\[Nu]FourierXY::usage="\[Nu]FourierXY[xylist_] or \[Nu]FourierXY[f_,xmin_,xmax_,\[Nu]max_] 
	computes in numerical approximation of the Fourier transform in linear units y[\[Nu]]=\!\(\*FractionBox[\(1\), SqrtBox[\(L\)]]\)\!\(\*SubsuperscriptBox[\(\[Integral]\), SubscriptBox[\(x\), \(0\)], \(\*SubscriptBox[\(x\), \(0\)] + L\)]\)y(x)Exp[\[ImaginaryI](2\[Pi]\[Nu])x]\[DifferentialD]x=\!\(\*FractionBox[\(dx\), SqrtBox[\(L\)]]\)\!\(\*UnderoverscriptBox[\(\[Sum]\), \(r = 1\), \(n - 1\)]\)y(\!\(\*SubscriptBox[\(x\), \(r\)]\))Exp[\[ImaginaryI](2\[Pi]\[Nu])\!\(\*SubscriptBox[\(x\), \(r\)]\)];
	Frequency is defined in linear units on the two-sided interval \[Nu]=s/dx, s\[Element][-(n-1)/2, (n-1)/2];
	In the case of odd n the spectrum contains equal number of positive and negative frequency components, in the case of even n there is n/2 positive frequency components and n/2-1 negative frequency components"

\[Omega]FourierXY::usage="\[Nu]FourierXY[xylist_]"

OneFromTwoSidedSpectrum::usage="OneFromTwoSidedSpectrum[sp_,OptionsPattern[\[Epsilon]Zero->10^-5]]"
SymmetrizeSpectrum::usage="SymmetrizeSpectrum[sp_,OptionsPattern[\[Epsilon]Zero->10^-5]]:"


(* ::Subsubsection::Closed:: *)
(*Plots*)


ListPlotJ::usage=""
ListLogPlotJ::usage=""
ListLogLogPlotJ::usage=""
ListLogLinearPlotJ::usage=""


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


(* ::Subsection::Closed:: *)
(*Old*)


YScale::usage="YScale[list_, a_]
	Function accepts an array of data list_ and rescales it along the Y coordinate by the factor of a_.

	list is either an array of XY data of the format {{\!\(\*SubscriptBox[\(x\), \(i\)]\),\!\(\*SubscriptBox[\(y\), \(i\)]\)}, ...} (single list input) or a list of XY data
	arrays {{{\!\(\*SubsuperscriptBox[\(x\), \(i\), \(1\)]\),\!\(\*SubsuperscriptBox[\(y\), \(i\), \(1\)]\)}, ...},{{\!\(\*SubsuperscriptBox[\(x\), \(i\), \(2\)]\),\!\(\*SubsuperscriptBox[\(y\), \(i\), \(2\)]\)}, ...},...} (multiple list input). In the latter case Y-scaling is applied to every element in the array.

	Output: {X,Y}\[Rule]{X\[Times]XScale+XShift,Y\[Times]A+YShift}.\[IndentingNewLine]
	a \[Dash] constant for single-list input, or may be a list of constants in the case of multiple list input.
	In the latter case \!\(\*SuperscriptBox[SubscriptBox[\(y\), \(i\)], \(j\)]\) is scaled by \!\(\*SubscriptBox[\(a\), \(j\)]\)\[IndentingNewLine]
	XScale and XShift can be constant only\[IndentingNewLine]
	YShift can be a constant, a list of constants, a single XY trace or a list of XY traces.
	In the case of list of constants \!\(\*SubscriptBox[\(YShift\), \(i\)]\) is applied to the i-th trace.
	In the case of a single trace, Y-values of this trace get subtracted from Y-values of the data traces, generalization to multiple offset traces is regular."


XScale::usage=""
XShift::usage=""
YShift::usage=""


\[Nu]FourierD::usage="\[Nu]FourierD[f_] computes the discrete Fourier transform of the function f, given as a list values {{Subscript[t, 0],Subscript[f, 0]},{Subscript[t, 0]+dt, Subscript[f, 1]},{Subscript[t, 0]+2dt,Subscript[f, 2]},...,{Subscript[t, 0]+n dt, Subscript[f, n]}}
	The returned value is a list {-n\[CapitalDelta]\[Nu],{Overscript[f, ~](-n\[CapitalDelta]\[Nu])},...,{0, Overscript[f, ~](0)},...,{n \[CapitalDelta]\[Nu], Overscript[f, ~](n\[CapitalDelta]\[Nu])}}, where \[CapitalDelta]\[Nu]=\!\(\*FractionBox[\(1\), \(dt \((n - 1)\)/2\)]\) and n is rounded to the nearest smaller odd number.
"


XReflect::usage=""

XYArea::usage=""
XYFindPeaks::usage="XYFindPeaks[xylist_, \[Sigma]_:0, s_:0, t_:-\[Infinity]]"
YNormalize::usage="YNormalize[xylist_]
	Function accepts 2D array of data (xylist) and rescales it along Y coordinate to be within [0,1]"


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

defaultPlotFrameOptions=Sequence[
	Frame->True,
	LabelStyle->Directive["Arial",10],
	FrameStyle->Directive[Black,AbsoluteThickness[1]]
];

defaultPlotGridOptions=Sequence[GridLines->Automatic,
	GridLinesStyle->Directive[LightGray,AbsoluteThickness[1]]
];


(* ::Subsection::Closed:: *)
(*Color sets*)


(* ::Subsubsection::Closed:: *)
(*Colors*)


(*97-standard colors list*)
stdColors=ColorData[97,"ColorList"];

lightColors=ColorData[55,"ColorList"];

redColors=Table[
	Blend[{ColorData[2,"ColorList"][[3]],ColorData[2,"ColorList"][[1]],ColorData[5,"ColorList"][[1]]},x],
{x,0,1,0.2}];

blueColors=Table[
	Blend[{CMYKColor[1,0,0,.25],CMYKColor[1,0.7,0,.25]},x],
{x,0,1,0.2}];

greenColors=Table[
	Blend[{ColorData[38,"ColorList"][[3]],CMYKColor[1,0,1,.5]},x],
{x,0,1,0.2}];

yellowColors=Table[
	Blend[{ColorData[50,"ColorList"][[6]],ColorData[50,"ColorList"][[2]],ColorData[50,"ColorList"][[8]]},x],
{x,0,1,0.2}];



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


CellReuseSequencePrint[]:=CellPrint[
Cell[CellGroupData[{Cell["New", "Section"],
Cell[CellGroupData[{
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
}, Open  ]],
Cell[BoxData[
 RowBox[{"ReEvaluateCells", "[", 
  RowBox[{"cIDs", ",", 
   RowBox[{"\"\<M1\>\"", "\[Rule]", "\"\<M2\>\""}]}], "]"}]], "Input"]
}, Open  ]]];


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
ScaleY[xylist_?XYListQ,a:Except[_?OptionQ]:1,OptionsPattern[{ScaleX->1,ShiftX->0,ShiftY->0}]]:=Module[{xSc,ySc,xSh,ySh},
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


(* ::Subsubsection::Closed:: *)
(*Joined List Plots*)


(*Acts as ListPlot but allways adds data points, even if Joined\[Rule]True*)
ListPlotJ[Data__]:=Module[{plotStyleOption,plotStyleOptionList},
	plotStyleOptionList=Cases[{Data},HoldPattern[PlotStyle-> __]];
	plotStyleOption=If[plotStyleOptionList=={},PlotStyle->Automatic,plotStyleOptionList];
	Show[ListPlot[Data],ListPlot[{Data}[[1]],Joined->False,plotStyleOption,PlotRange->Full]]
];
ListLogPlotJ[Data__]:=Module[{plotStyleOption,plotStyleOptionList},
	plotStyleOptionList=Cases[{Data},HoldPattern[PlotStyle-> __]];
	plotStyleOption=If[plotStyleOptionList=={},PlotStyle->Automatic,plotStyleOptionList];Show[ListLogPlot[Data],ListLogPlot[{Data}[[1]],Joined->False,plotStyleOption,PlotRange->Full]]
];
ListLogLogPlotJ[Data__]:=Module[{plotStyleOption,plotStyleOptionList},
	plotStyleOptionList=Cases[{Data},HoldPattern[PlotStyle-> __]];
	plotStyleOption=If[plotStyleOptionList=={},PlotStyle->Automatic,plotStyleOptionList];Show[ListLogLogPlot[Data],ListLogLogPlot[{Data}[[1]],Joined->False,plotStyleOption,PlotRange->Full]]
];
ListLogLinearPlotJ[Data__]:=Module[{plotStyleOption,plotStyleOptionList},
	plotStyleOptionList=Cases[{Data},HoldPattern[PlotStyle-> __]];
	plotStyleOption=If[plotStyleOptionList=={},PlotStyle->Automatic,plotStyleOptionList];Show[ListLogLinearPlot[Data],ListLogLinearPlot[{Data}[[1]],Joined->False,plotStyleOption,PlotRange->Full]]
];



(* ::Subsubsection::Closed:: *)
(*Sweep List Plots*)


(* ::Input::Initialization:: *)
SweepListLogPlot[plotData_,opts:OptionsPattern[{Plot}]]:=Module[{plotStyleList,colList},
colList={Red,Black};
plotStyleList=Table[Directive[Blend[colList,x],Thin],{x,0,1,1/(Length[plotData]-1)}];

ListLogPlot[plotData,opts,PlotStyle-> plotStyleList]
];


(* ::Subsubsection::Closed:: *)
(*Plot grid*)


Options[PlotGrid]={ImagePadding->40};
PlotGrid[l_List,w_,h_,opts:OptionsPattern[]]:=Module[{nx,ny,sidePadding=OptionValue[PlotGrid,ImagePadding],topPadding=0,widths,heights,dimensions,positions,frameOptions=FilterRules[{opts},FilterRules[Options[Graphics],Except[{ImagePadding,Frame,FrameTicks}]]]},
	{ny,nx}=Dimensions[l];
	widths= Table[(w-2 sidePadding)/nx,{nx}];
	widths[[1]]=widths[[1]]+sidePadding;
	widths[[-1]]=widths[[-1]]+sidePadding;
	heights= Table[(h-2 sidePadding)/ny,{ny}];
	heights[[1]]=heights[[1]]+sidePadding;
	heights[[-1]]=heights[[-1]]+sidePadding;
	positions=Transpose@Partition[Tuples[Prepend[Accumulate[Most[#]],0]&/@{widths,heights}],ny];
	Graphics[
		Table[Inset[
			Show[l[[ny-j+1,i]],
				ImagePadding->{
					{If[i==1,sidePadding,0],If[i==nx,sidePadding,0]},
					{If[j==1,sidePadding,0],If[j==ny,sidePadding,topPadding]}
				},
				AspectRatio->Full],
			positions[[j,i]],{Left,Bottom},{widths[[i]],heights[[j]]}],
		{i,1,nx},{j,1,ny}],
	PlotRange->{{0,w},{0,h}},ImageSize->{w,h},Evaluate@Apply[Sequence,frameOptions]
	]
]


(* ::Subsubsection::Closed:: *)
(*Grid lines*)


ClearAll[GenerateGridLines];
Options[GenerateGridLines]={MainGridLinesStyle->Null,SubGridLinesStyle->Null,LogScale->False};

GenerateGridLines[xMin_,xMax_,step_,OptionsPattern[]]:=Which[
SameQ[OptionValue[MainGridLinesStyle],Null]&&(!OptionValue[LogScale]),
	Table[x,{x,xMin,xMax,step}],
SameQ[OptionValue[MainGridLinesStyle],Null]&&OptionValue[LogScale],
	Table[Exp[x],{x,Log[xMin],Log[xMax],Log[step]}],
(!SameQ[OptionValue[MainGridLinesStyle],Null])&&(!OptionValue[LogScale]),
	Table[{x,OptionValue[MainGridLinesStyle]},{x,xMin,xMax,step}],
True,
	Table[{Exp[x],OptionValue[MainGridLinesStyle]},{x,Log[xMin],Log[xMax],Log[step]}]
]

GenerateGridLines[xMin_,xMax_,step_,subStep_,OptionsPattern[]]:=Module[{mainGridLines,subGridLines,tmpXMin,tmpXMax,nSubMax,xRef},
mainGridLines=Which[
SameQ[OptionValue[MainGridLinesStyle],Null]&&(!OptionValue[LogScale]),
	Table[x,{x,xMin,xMax,step}],
SameQ[OptionValue[MainGridLinesStyle],Null]&&OptionValue[LogScale],
	Table[Exp[x],{x,Log[xMin],Log[xMax],Log[step]}],
(!SameQ[OptionValue[MainGridLinesStyle],Null])&&(!OptionValue[LogScale]),
	Table[{{x,OptionValue[MainGridLinesStyle]}},{x,xMin,xMax,step}],
True,
	Table[{{Exp[x],OptionValue[MainGridLinesStyle]}},{x,Log[xMin],Log[xMax],Log[step]}]
];

If[SameQ[OptionValue[MainGridLinesStyle],Null],
xRef=mainGridLines[[1]],
xRef=mainGridLines[[1,1,1]]
];

Which[(*In the case of logarithmic scale, the substep is relative to the lower boundary of the grid interval*)
SameQ[OptionValue[MainGridLinesStyle],Null]&&(!OptionValue[LogScale]),
	nSubMax=Floor[(mainGridLines[[2]]-mainGridLines[[1]])/subStep],
SameQ[OptionValue[MainGridLinesStyle],Null]&&OptionValue[LogScale],
	nSubMax=Floor[(mainGridLines[[2]]-mainGridLines[[1]])/(subStep xRef)],
(!SameQ[OptionValue[MainGridLinesStyle],Null])&&(!OptionValue[LogScale]),
	nSubMax=Floor[(mainGridLines[[2,1,1]]-mainGridLines[[1,1,1]])/subStep],
True,
	nSubMax=Floor[(mainGridLines[[2,1,1]]-mainGridLines[[1,1,1]])/(subStep xRef)]
];
subGridLines=Table[
If[SameQ[OptionValue[MainGridLinesStyle],Null],
tmpXMin=mainGridLines[[i]];
tmpXMax=mainGridLines[[i+1]];
,
tmpXMin=mainGridLines[[i,1,1]];
tmpXMax=mainGridLines[[i+1,1,1]];
];
Which[
SameQ[OptionValue[SubGridLinesStyle],Null]&&(!OptionValue[LogScale]),
	Table[x,{x,tmpXMin,tmpXMax,subStep}],
SameQ[OptionValue[SubGridLinesStyle],Null]&&OptionValue[LogScale],
	Table[x,{x,tmpXMin,tmpXMax,subStep*tmpXMin}],
(!SameQ[OptionValue[SubGridLinesStyle],Null])&&(!OptionValue[LogScale]),
	Table[{x,OptionValue[SubGridLinesStyle]},{x,tmpXMin,tmpXMax,subStep}],
True,
	Table[{x,OptionValue[SubGridLinesStyle]},{x,tmpXMin,tmpXMax,subStep*tmpXMin}]
][[2;;nSubMax]],
{i,Length[mainGridLines]-1}
];
(*return ordered*)
Flatten[Riffle[mainGridLines,subGridLines],1]
]


(* ::Subsection::Closed:: *)
(*Interactive plot manipulation*)


(* ::Subsubsection::Closed:: *)
(*Plot Explorer Original*)


Attributes[PlotExplorerOr]={HoldFirst};
PlotExplorerOr[defPlot_]:=DynamicModule[
{held,head,plot,range,zoomRange,defRange,defSize,defEpilog,size,pt1,pt2,spt1,dist,rect={},dragged=False,overButton=False,pushedButton=False,overHor=False,overVer=False,activeHor=False,activeVer=False,xPos=.5,yPos=.5,minDistance=0.1,sliderRatio=0.05,panFactor=10,reset,distance,slider,replot,coord=None},
held=Hold@defPlot;
head=Part[held,1,0];
plot=defPlot;
{defEpilog,defRange}={Epilog,PlotRange}/.Quiet@AbsoluteOptions@plot;
defEpilog=defEpilog/.Epilog->{};
size=defSize=Rasterize[plot,"RasterSize"];
range=zoomRange=defRange;

Deploy@EventHandler[
Dynamic[
MouseAppearance[
Show[plot,PlotRange->Dynamic@range,Epilog->{defEpilog,(*Original epilog*)(*Replot button*)EventHandler[Inset[Graphics[{Black,Dynamic@Opacity@If[overButton,If[pushedButton,.2,.1],0],Rectangle[{0,0},{1,1},RoundingRadius->Offset@5],Black,Dynamic@Opacity@If[overButton,If[pushedButton,.9,.7],0],Text["Replot",Center]},ImageSize->50,AspectRatio->1/2,BaseStyle->{FontFamily->"Helvetica"}],Scaled@{1,1},Scaled@{1,1}],{"MouseEntered":>(overButton=True),"MouseExited":>(overButton=False),"MouseDown":>(pushedButton=True),"MouseUp":>(pushedButton=False;replot@plot)}],(*Zoom rectangle*)FaceForm@{Blue,Opacity@.1},EdgeForm@{Thin,Dotted,Opacity@.5},Dynamic@rect,(*Range sliders*)Inset[slider[Dynamic@xPos,Dynamic@overHor,Dynamic@activeHor,size,True],Scaled@{.5,0},Scaled@{.5,0}],Inset[slider[Dynamic@yPos,Dynamic@overVer,Dynamic@activeVer,size,False],Scaled@{0,.5},Scaled@{0,.5}]}
],
(*cursor*)
Which[
dragged,"FrameRisingResize",

CurrentValue@"AltKey",
Graphics[{Antialiasing->False,Line@{{-1,0},{1,0}},Line@{{0,-1},{0,1}},Dynamic@Text[Style[coord,FontFamily->"Helvetica",8],{0,0},{-1.1,-1}]},ImageSize->160,ImagePadding->0,AspectRatio->1,PlotRange->{{-1,1},{-1,1}}*8],

CurrentValue@"ControlKey"&&!overHor&&!overVer,"ZoomView",

CurrentValue@"ShiftKey"&&!overHor&&!overVer,"DragGraphics",

CurrentValue@"ShiftKey"&&(overHor||overVer),"LinkHand",True,Automatic]
],

TrackedSymbols:>{dragged,plot}
],
{"MouseMoved":>(coord=MousePosition@"Graphics"),"MouseClicked":>If[!overButton&&CurrentValue@"MouseClickCount"==2,reset[]],"MouseDown":>If[(!overHor||!overVer),pt1=MousePosition@"Graphics";
spt1=MousePosition@"GraphicsScaled"],
"MouseDragged":>
If[
(!overHor&&!overVer||dragged)&&!overButton&&!pushedButton&&!activeVer&&!activeHor,
dragged=True;
Which[
CurrentValue@"ControlKey",
dist=Last@MousePosition@"GraphicsScaled"-Last@spt1;
range=zoomRange*10^dist,

(*"GraphicsScaled" is required below as "Graphics" would give jumpy results as the plot range is constantly modified.*)
CurrentValue@"ShiftKey",pt2=MapThread[Rescale,{MousePosition@"GraphicsScaled",{{0,1},{0,1}},zoomRange}]-pt1;
range=zoomRange-pt2,

True,
pt2=MousePosition@"Graphics";
rect=If[pt2===None,{},Rectangle[pt1,pt2]]
]
],
"MouseUp":>(If[!CurrentValue@"ShiftKey"&&!CurrentValue@"ControlKey"&&dragged&&distance[pt1,pt2,zoomRange]>minDistance,rect={};
range=Transpose@Sort@{pt1,pt2};];zoomRange=range;
dragged=False)},PassEventsDown->True,PassEventsUp->True]
,Initialization:>(slider[Dynamic[pos_],Dynamic[over_],Dynamic[active_],size_,hor_]:=EventHandler[Dynamic[LocatorPane[If[hor,Dynamic[{pos,.5},(pos=Clip[First@#,{0,1}];
range[[1]]=If[CurrentValue@"ShiftKey",First@zoomRange+(pos-.5)*Abs[Subtract@@First@zoomRange]*panFactor,First@zoomRange*10^((pos-.5)*2)])&],Dynamic[{.5,pos},(pos=Clip[Last@#,{0,1}];
range[[2]]=If[CurrentValue@"ShiftKey",Last@zoomRange+(pos-.5)*Abs[Subtract@@Last@zoomRange]*panFactor,Last@zoomRange*10^((pos-.5)*2)])&]],(*Locator below is required inside Graphics as LocatorPane inside Inset looses the crosshair image.*)Graphics[{Black,Opacity@If[(over||active)&&!dragged,.1,0],Rectangle[{0,0},{1,1},RoundingRadius->Offset@5],Opacity@1,Locator[Dynamic@If[hor,{pos,.5},{.5,pos}],Appearance->If[(over||active)&&!dragged,Automatic,None]]},ImagePadding->0,PlotRangePadding->0,ImageSize->If[hor,#*{1,0}+{0,20}&,#*{0,1}+{20,0}&]@size,AspectRatio->If[hor,1/sliderRatio/First@size,sliderRatio*Last@size]],Enabled->(over||active),Appearance->None,FrameMargins->0,ContentPadding->False],TrackedSymbols:>{pos,over,active,dragged}],{"MouseDown":>(active=True),"MouseUp":>(active=False),"MouseEntered":>(over=!If[hor,activeVer,activeHor]),(*The value of `active` must be public so that one slider can check whether the other is dragged,not to pop up if the other locator is moved over this slider's area.*)"MouseExited":>(over=False)},PassEventsDown->True
];


replot[p_]:=Module[{temp},Switch[head,Plot,temp=ReplacePart[held,{{1,2}->{held[[1,2,1]],range[[1,1]],range[[1,2]]}}];
(*Replace plot range or insert if nonexistent*)plot=ReleaseHold@If[MemberQ[temp,PlotRange,Infinity],temp/.{_[PlotRange,_]->(PlotRange->range)},Insert[temp,PlotRange->range,{1,-1}]],DensityPlot|ContourPlot|RegionPlot|StreamPlot|StreamDensityPlot|VectorPlot|VectorDensityPlot|LineIntegralConvolutionPlot,temp=ReplacePart[held,{{1,2}->{held[[1,2,1]],range[[1,1]],range[[1,2]]},{1,3}->{held[[1,3,1]],range[[2,1]],range[[2,2]]}}];
plot=ReleaseHold@If[MemberQ[temp,PlotRange,Infinity],temp/.{_[PlotRange,_]->(PlotRange->range)},Insert[temp,PlotRange->range,{1,-1}]];,_,Null]];

reset[]:=(range=zoomRange=defRange;xPos=yPos=.5);
(*Quiet suppresses error message when Rescale[y,{x,x}] where y\[NotEqual]x.EuclideanDistance then defaults to Infinity which is fine.*)

distance[p1_,p2_,{r1_,r2_}]:=Quiet[N@EuclideanDistance[Rescale[p1,r1],Rescale[p2,r2]]];)
];


(* ::Subsubsection::Closed:: *)
(*Plot Explorer New*)


PlotExplorer[plot_]:=
(*
Function allows to interactively manipulate the display range in plots. It should work with any kind of 2D Plots, ListPlots, etc.;

Functionality:              ;
CTRL+mouse drag \[Dash] select the area to be zoomed in;
Shift+mouse drag \[Dash] shift the area of view;
CTRL+double mouse click \[Dash] restore graph to its default
*)
DynamicModule[
(*Local variables*)
{currentPlotRange (*dynamic PlotRange*),
defPlotRange (*default PlotRange as defined in the initial plot*),
defEpilog (*epilog in the initial plot*),
currentAxesOrigin (*dynamic AxesOrigin*),
defAxesOrigin (*AxesOrigin in the initial plot*),
currentShowAxes (*current value of Axes option in the graph*),
defShowAxes(*default value of Axes option in the graph*),
dragged=False (*flag, indicating if the mouse cursor is being dragged now*),
zoomRect (*Rectangle, indicating the area to b zoomed in*),
pt1,pt2 (*two points in the coordinates of the graph, denote the corners of zoomRect*),
spt1,spt2(*two points in relative coordinates*),
minDist=10^-8(*minimal relative zoom area, needed for technical reasons*),
Reset(*functions resers the plot to its default*)
},

Reset[]:=(
zoomRect={};(*means no rectangle to be displayed*)
currentPlotRange=defPlotRange;
currentAxesOrigin=defAxesOrigin;
currentShowAxes=defShowAxes
);


{defEpilog,defPlotRange,defAxesOrigin,defShowAxes}={Epilog,PlotRange,AxesOrigin,Axes}/.Quiet@AbsoluteOptions@plot;
Reset[];

EventHandler[
Dynamic[
MouseAppearance[
Show[plot,PlotRange->Dynamic@currentPlotRange,AxesOrigin->Dynamic@currentAxesOrigin,Axes->Dynamic@currentShowAxes,
Epilog->{defEpilog,
(*Zoom rectangle*)FaceForm@{Blue,Opacity@.1},EdgeForm@{Thin,Dotted,Opacity@.5},Dynamic@zoomRect}],
Which[
CurrentValue@"ControlKey","ZoomView",
CurrentValue@"ShiftKey","DragGraphics",
True,Automatic
]
]
]
,
{"MouseClicked":>If[CurrentValue["ControlKey"]&&(CurrentValue["MouseClickCount"]==2),Reset[]],
"MouseDown":>(
pt1=MousePosition@"Graphics";
spt1=MousePosition@"GraphicsScaled"),
"MouseDragged":>(
pt2=MousePosition@"Graphics";
spt2=MousePosition@"GraphicsScaled";
If[(spt1-spt2).(spt1-spt2)>minDist,dragged=True];
Which[
CurrentValue@"ControlKey",
zoomRect=If[pt2===None,{},Rectangle[pt1,pt2]],

CurrentValue@"ShiftKey",
currentPlotRange=currentPlotRange-(pt2-pt1);
currentShowAxes=False (*Empirically, its better to hide the axes while dragging the field of view, otherwise the picture may be jumpy as a result of axes redrawing. *),

True,zoomRect={};
]),
"MouseUp":>(
zoomRect={};
Which[
CurrentValue@"ControlKey"&&dragged,
currentPlotRange={Sort@{pt1[[1]],pt2[[1]]},Sort@{pt1[[2]],pt2[[2]]}};
(*If the new field of view does not include axes, set AxesOrigin to left bottom corner*)
If[(currentPlotRange[[1,1]]>currentAxesOrigin[[1]])|| ( currentPlotRange[[1,2]]<currentAxesOrigin[[1]]),currentAxesOrigin[[1]]=currentPlotRange[[1,1]]];(* X axes*)
If[(currentPlotRange[[2,1]]>currentAxesOrigin[[2]])|| ( currentPlotRange[[2,2]]<currentAxesOrigin[[2]]),currentAxesOrigin[[2]]=currentPlotRange[[2,1]]]
(* Y axes*),

CurrentValue@"ShiftKey"&&dragged,
(*If the new field of view does not include axes, set AxesOrigin to left bottom corner*)
If[(currentPlotRange[[1,1]]>currentAxesOrigin[[1]])|| ( currentPlotRange[[1,2]]<currentAxesOrigin[[1]]),currentAxesOrigin[[1]]=currentPlotRange[[1,1]]];(* X axes*)
If[(currentPlotRange[[2,1]]>currentAxesOrigin[[2]])|| ( currentPlotRange[[2,2]]<currentAxesOrigin[[2]]),currentAxesOrigin[[2]]=currentPlotRange[[2,1]]]
(* Y axes*)
];
currentShowAxes=defShowAxes;
dragged=False)
},
PassEventsDown->True
]
];


(* ::Subsection::Closed:: *)
(*Interval manipulations*)


(* ::Input:: *)
(*(*taken from http://mathematica.stackexchange.com/questions/11345/can-mathematica-handle-open-intervals-interval-complements*)*)


(*computes the complement (\[Minus]\[Infinity],\[Infinity])/a.*)
IntervalInverse[Interval[int___]]:=Interval@@Partition[
	Flatten@{int}/.{{-\[Infinity],mid___,\[Infinity]}:>{mid},{-\[Infinity],mid__}:>{mid,\[Infinity]},{mid__,\[Infinity]}:>{-\[Infinity],mid},{mid___}:>{-\[Infinity],mid,\[Infinity]}},2]


(*IntervalComplement[a,b,c,..] computes a\[Backslash](b\:222ac\:222a\[Ellipsis])*)
IntervalComplement[a_Interval,b__Interval]:=IntervalIntersection[a,IntervalInverse@IntervalUnion[b]]


(* ::Subsection::Closed:: *)
(*Reuse of the previously typed expressions*)


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
Module[{cellList,cellListTransf,selCriterion,nb},
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
			];
		,{i,Length[cellListTransf]}]
]


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


ClearAll[FitRangeSelector];
SetAttributes[FitRangeSelector,HoldFirst];(*Hold attribute for the "fitRanges" variable is needed for being able to clear the variable if it was already defined*)
FitRangeSelector[fitRanges_,traceList_,OptionsPattern[{logYPlot->False,logXPlot-> False}]]:=DynamicModule[{epList, LowerLine,UpperLine,minList,maxList,plotF},
	(*right and left cursor lines*)
	minList=Table[Min[tr[[;;,2]]],{tr,traceList}];
	maxList=Table[Max[tr[[;;,2]]],{tr,traceList}];
	LowerLine[i_,x_]:={CMYKColor[1.,0,0,.4],Line[{{x,2minList[[i]]},{x,2maxList[[i]]}}]};
	UpperLine[i_,x_]:={CMYKColor[0.,0.2,0.5,.1],Line[{{x,2minList[[i]]},{x,2maxList[[i]]}}]};
	plotF=Which[
	OptionValue[logYPlot]&&OptionValue[logXPlot],ListLogLogPlot,
	OptionValue[logYPlot]&&(!OptionValue[logXPlot]),ListLogPlot,
	(!OptionValue[logYPlot])&&OptionValue[logXPlot],ListLogLinearPlot,
	(!OptionValue[logYPlot])&&(!OptionValue[logXPlot]),ListPlot
	];

	(* epList is the list of cursors to display the chosen boundaries*)
	epList=Table[
	{LowerLine[i,traceList[[i,1,1]]],UpperLine[i,traceList[[i,-1,1]]]}
	,{i,Length[traceList]}];
	Clear[fitRanges];
	fitRanges=Table[{0,0},{i,Length[traceList]}];

	Manipulate[
	EventHandler[plotF[traceList[[i]],Joined-> True,ImageSize->Large,PlotRange->Full,Epilog-> epList[[i]]],
	{{"MouseClicked",1}:>(
		fitRanges[[i,1]]=MousePosition["Graphics"][[1]];
		epList[[i]]={LowerLine[i,fitRanges[[i,1]]], epList[[i,2]]};
		)},
	{{"MouseClicked",2}:>(
		fitRanges[[i,2]]=MousePosition["Graphics"][[1]];
		epList[[i]]={ epList[[i,1]],UpperLine[i,fitRanges[[i,2]]]};
		)}
	],
	{i,1,Length[traceList],1}],
SaveDefinitions->True]


FindFitSeries[traceList_,fitModel_,pars_,vars_,fitRanges:Except[_?OptionQ]:Full,OptionsPattern[{FitFunction->FindFit}]]:=Module[{dataList,fitModelsList,parsList,frIntervalsList},
	(*reduce the input parameters from all the various acceptable forms to a single one \[Dash] lists*)
	If[fitRanges===Full,
		(*if the entire traces are to be fitted*)
		dataList=traceList,
		
		(*if some range of x-values is to be selected first*)
		frIntervalsList=Which[
		(*case of single fit interval for all the traces*)
		(Head[fitRanges]===Interval),
			Table[fitRanges,{i,Length[traceList]}],
		(*case of individual fit intervals*)
		(Head[fitRanges]===List)&&(Head[fitRanges[[1]]]===Interval),
			fitRanges,
		(*case of single fit range for all the traces*)
		(Head[fitRanges]===List)&&(Head[fitRanges[[1]]]=!=Interval)&&(Depth[fitRanges]== 2), 
			Table[Interval[fitRanges],{i,Length[traceList]}],
		(*case of individual fit ranges*)
		(Head[fitRanges]===List)&&(Head[fitRanges[[1]]]===List)&&(Depth[fitRanges]== 3), 
			Table[Interval[fitRanges[[i]]],{i,Length[traceList]}]
		];
		dataList=Table[Select[traceList[[i]],IntervalMemberQ[frIntervalsList[[i]],#[[1]]]&],{i,Length[traceList]}];
	];
	fitModelsList=If[Head[fitModel]=!= List,
		Table[fitModel,{i,Length[traceList]}],
		fitModel
	];
	parsList=If[
		Depth[pars]<=2 ||(Depth[pars]==3 && DeleteDuplicates[Head/@Flatten[pars]]=!= {Symbol}) , Table[pars,{i,Length[traceList]}],
		Table[pars[[i]],{i,Length[traceList]}]
	];

	(*perform fitting*)
	Table[OptionValue[FitFunction][dataList[[i]],fitModelsList[[i]],parsList[[i]],vars],{i,Length[traceList]}]
];



FitQualityCheck[traceList_,fitRanges_,fitModel_,fitParameters_,OptionsPattern[{logYPlot->False,logXPlot-> False}]]:=
Module[{xPlotRanges,cPlotF,dPlotF},
If[Head[fitRanges]=!=List, Print["Fit ranges is not a list"];Return[]];
xPlotRanges=Which[
Depth[fitRanges]== 2, (*case of single fit range for all the traces*)
Table[fitRanges,{i,Length[traceList]}],
Depth[fitRanges]== 3, (*case of individual fit ranges*)
Table[fitRanges[[i]],{i,Length[traceList]}]
];

cPlotF=Which[ (*chose continuous plotting function*)
OptionValue[logYPlot]&&OptionValue[logXPlot],LogLogPlot,
OptionValue[logYPlot]&&(!OptionValue[logXPlot]),LogPlot,
(!OptionValue[logYPlot])&&OptionValue[logXPlot],LogLinearPlot,
(!OptionValue[logYPlot])&&(!OptionValue[logXPlot]),Plot
];
dPlotF=Which[ (*chose discrete plotting function*)
OptionValue[logYPlot]&&OptionValue[logXPlot],ListLogLogPlot,
OptionValue[logYPlot]&&(!OptionValue[logXPlot]),ListLogPlot,
(!OptionValue[logYPlot])&&OptionValue[logXPlot],ListLogLinearPlot,
(!OptionValue[logYPlot])&&(!OptionValue[logXPlot]),ListPlot
];

Do[
Print[i];
Print[
Show[dPlotF[traceList[[i]],PlotRange->{xPlotRanges[[i]],Full}],cPlotF[(fitModel)/.fitParameters[[i]],{Global`x,xPlotRanges[[i,1]],xPlotRanges[[i,2]]},PlotStyle->Directive[Red,Thin],PlotRange->Full]]
]
,{i,Length[traceList]}]
];


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


FindLogFit[data_,expr_,rest__]:=Module[{logData},
(*The function is equivalent to FindFit, but works in log Y scale*)
	logData=Which[
		VectorQ[data],Log[Abs[data]],(*data list*)
		XYListQ[data],Transpose[{data[[;;,1]],Log[Abs[data[[;;,2]]]]}], (*XY data*)
		True,{}
	];
	FindFit[logData,Log[expr],rest]
];


(* ::Subsection::Closed:: *)
(*Fourier Transform*)


(*computes in numerical approximation of the Fourier transform in linear units y[\[Nu]]=1/Sqrt[L]\!\(
\*SubsuperscriptBox[\(\[Integral]\), 
SubscriptBox[\(x\), \(0\)], \(
\*SubscriptBox[\(x\), \(0\)] + L\)]\(y\((x)\)Exp[\[ImaginaryI]\((2 \[Pi]\[Nu])\)x]\[DifferentialD]x\)\)=dx/Sqrt[L]\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(r = 1\), \(n - 1\)]\(y\((
\*SubscriptBox[\(x\), \(r\)])\)Exp[\[ImaginaryI]\((2 \[Pi]\[Nu])\)
\*SubscriptBox[\(x\), \(r\)]]\)\);
Frequency is defined in linear units on the two-sided interval \[Nu]=s/dx, s\[Element][-(n-1)/2, (n-1)/2];
In the case of odd n the spectrum contains equal number of positive and negative frequency components, in the case of even n there is n/2 positive frequency components and n/2-1 negative frequency components
*)
\[Nu]FourierXY[xylist_?XYListQ]:=Module[{L,dx,n,nOS,\[Nu]List,ftyList},
	L=xylist[[-1,1]]-xylist[[1,1]](*interval length in x dimension*);
	n=Length[xylist];
	nOS=Floor[n/2](*define maximum positive frequency index*);
	dx=L/(n-1)(*step in x dimension*);
	\[Nu]List=1/L Range[-(n-1)+nOS,nOS];
	ftyList=Sqrt[dx]Fourier[xylist[[;;,2]]];
	ftyList=Join[ftyList[[nOS+2;;]],ftyList[[;;nOS+1]]](*by default the frequencies in Fourier[] span from 0 to (n-1)/L, here they are rearranged to form two-sided spectrum [(n/2-1)/L,(n/2-1)/L]*);
	Transpose[{\[Nu]List,ftyList}]
];

(* the version of \[Nu]FourierXY that acts on functions*)
\[Nu]FourierXY[f_?FunctionQ,xmin_,xmax_,\[Nu]max_]:=Module[{L,dx,n,xylist},
	L=(xmax-xmin);
	n=Ceiling[(\[Nu]max*2L)](*find the number of samples needed to calculate the spectrum up to the frequency \[Nu]max*);
	If[OddQ[n],n=n+1];
	dx=L/(n-1)(*step in x dimension*);
	xylist=Table[{xmin+k*dx,f[xmin+k*dx]},{k,0,n-1}];
	\[Nu]FourierXY[xylist]
];

\[Omega]FourierXY[args__]:=ScaleY[\[Nu]FourierXY[args],ScaleX->2\[Pi]]


OneFromTwoSidedSpectrum[sp_,OptionsPattern[\[Epsilon]Zero->10^-5]]:=Module[{d\[Nu],\[Nu]0,\[Epsilon],tmpSp,zero\[Nu]Element},
(*Function converts a two-sided spectrum at the input {{-Subscript[\[Nu], n],Subsuperscript[f, n, *]},{-Subscript[\[Nu], n]+d\[Nu],Subsuperscript[f, n, *]},...,{Subscript[\[Nu], n],Subscript[f, n]}};
to one-sided spectrum {{0,Subscript[f, 0]'},...,{Subscript[\[Nu], n],Subscript[f, n]'}}, where Subscript[f, 0]'=Subscript[f, 0], Subscript[f, n]'=2Subscript[f, n];
Works only with uniformly-spaced frequency components Subscript[\[Nu], n].
*)
	\[Epsilon]=OptionValue[\[Epsilon]Zero];(*small number, characterizing the acceptable deviation from zero frequency*)
	d\[Nu]=sp[[2,1]]-sp[[1,1]];(*frequency step*)
	zero\[Nu]Element=Select[sp,(Abs[#[[1]]]<\[Epsilon] d\[Nu])&];
	If[Length[zero\[Nu]Element]!=1,
		Print["Multiple or no zero-frequency elements are found"];Return[{}],
		zero\[Nu]Element=zero\[Nu]Element[[1]]
	];
	tmpSp=Select[sp,(#[[1]]>0)&];
	Join[{zero\[Nu]Element},Transpose[{tmpSp[[;;,1]],2tmpSp[[;;,2]]}]]
]


SymmetrizeSpectrum[sp_,OptionsPattern[\[Epsilon]Zero->10^-5]]:=Module[{d\[Nu],\[Epsilon],zero\[Nu]ElementI,l,ret,\[Nu]List},
(*Function converts a two-sided spectrum at the input {{-Subscript[\[Nu], n],Subscript[f, -n]},{-Subscript[\[Nu], n]+d\[Nu],Subscript[f, -n]},...,{Subscript[\[Nu], n],Subscript[f, n]}};
to two-sided symmetrized spectrum {{-Subscript[\[Nu], n],Subsuperscript[f, -n, ']},{-Subscript[\[Nu], n]+d\[Nu],Subsuperscript[f, -n, ']},...,{Subscript[\[Nu], n],Subscript[f^', n]}}, where Subscript[f, 0]'=Subscript[f, 0], Subscript[f, n]'=(Subscript[f, n]+Subscript[f, -n])/2;
Works only with uniformly-spaced frequency components Subscript[\[Nu], n].
*)
	\[Epsilon]=OptionValue[\[Epsilon]Zero];(*small number, characterizing the acceptable deviation from zero frequency*)
	d\[Nu]=sp[[2,1]]-sp[[1,1]];(*frequency step*)

	zero\[Nu]ElementI=Position[sp,{_?(Abs[#]<\[Epsilon]&),_}];
	If[Length[zero\[Nu]ElementI]!=1,Print[zero\[Nu]ElementI,"Multiple or no zero-frequency elements are found"];Return[{}],zero\[Nu]ElementI=zero\[Nu]ElementI[[1,1]]];
	l=Min[Length[sp]-zero\[Nu]ElementI,zero\[Nu]ElementI-1];
	\[Nu]List=Table[k d\[Nu],{k,-l,l,1}];
	(*number of frequency components, for which an opposite-frequency countepart exists*)
	ret=Table[(sp[[zero\[Nu]ElementI+i,2]]+sp[[zero\[Nu]ElementI-i,2]])/2,{i,0,l}];
	Transpose[{\[Nu]List,Join[Reverse[ret[[2;;]]],ret]}]
]


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


(* ::Subsection::Closed:: *)
(*Old*)


(**;
Function takes array of data (list_)
and rescales it along Y coordinate by the factor of a_;
list_ can be either a 2D array (interpreted as single {{Subscript[x, i],Subscript[y, i]},..} trace) or a list of such arrays. 

**)
YScale[list_,a:Except[_?OptionQ]:1,OptionsPattern[{XScale->1,XShift->0,YShift->0}]]:=Module[{dimL, dimA,xSc,xSh,ySh},
(* 
list is either an array of XY data of the format "{{Subscript[x, i],Subscript[y, i]}, ...}" (single list input);
or a list of XY data arrays "{{{Subscript[x, i]^1,Subscript[y, i]^1}, ...},{{Subscript[x, i]^2,Subscript[y, i]^2}, ...},...}" (multiple list input).;
In the latter case Y-scaling is applied to every element in the array;

Output: {X,Y}\[Rule]{X\[Times]XScale+XShift,Y\[Times]A+YShift};

a is a constant for single-list input, or may be a list of constants in the case of multiple list input.;
In the latter case Subscript[y, i]^j is scaled by Subscript[a, j];

XScale and XShift can be constant only;

YShift can be a constant, a list of constants, a single XY trace or a list of XY traces.;
In the case of list of constants Subscript[YShift, i] is applied to the i-th trace;
In the case of a single trace, Y-values of this trace get subtracted from Y-values of the data traces, generalization to multiple offset traces is regular.;

*)
dimL=Depth[list];
dimA=Depth[a];
xSc=OptionValue[XScale];
xSh=OptionValue[XShift];
ySh=OptionValue[YShift];

Which[
dimL==3,
(*single list input*)
Which[
Head[ySh]=!=List,
(*single number Y offset*)
Transpose[{xSc list[[;;,1]]+xSh,a list[[;;,2]]+ySh}]
,
Head[ySh]===List && Length[ySh]==Length[list]&&Depth[ySh]==3,
(*an YOffset trace*)
Transpose[{xSc list[[;;,1]]+xSh,a list[[;;,2]]+ySh[[;;,2]]}]
]
,
dimL==4 && dimA==1,
(*multiple list input, single constant scaling*)
Which[
Head[ySh]=!=List,
(*single number Y offset*)
Table[Transpose[{xSc list[[i,;;,1]]+xSh,a list[[i,;;,2]]+ySh}],{i,Length[list]}]
,
Head[ySh]===List &&  Dimensions[ySh]==Dimensions[list][[{2,3}]]&&Depth[ySh]==3,
(*a single YOffset trace*)
Table[Transpose[{xSc list[[i,;;,1]]+xSh,a list[[i,;;,2]]+ySh[[;;,2]]}],{i,Length[list]}]
,
Head[ySh]===List && Dimensions[ySh]==Dimensions[list]&&Depth[ySh]==4,
(*multiple YOffset traces*)
Table[Transpose[{xSc list[[i,;;,1]]+xSh,a list[[i,;;,2]]+ySh[[i,;;,2]]}],{i,Length[list]}]
]
,
dimL==4 && dimA==2,
(*multiple list input, multiple constant scaling*)
Which[
Head[ySh]=!=List,
(*single number Y offset*)
Table[Transpose[{xSc list[[i,;;,1]]+xSh,a[[i]] list[[i,;;,2]]+ySh}],{i,Length[list]}]
,
Head[ySh]===List && Length[ySh]==Length[list]&&Depth[ySh]==3,
(*a single YOffset trace*)
Table[Transpose[{xSc list[[i,;;,1]]+xSh,a[[i]] list[[i,;;,2]]+ySh[[;;,2]]}],{i,Length[list]}]
,
Head[ySh]===List && Length[ySh]==Length[list]&&Depth[ySh]==4,
(*multiple YOffset traces*)
Table[Transpose[{xSc list[[i,;;,1]]+xSh,a [[i]]list[[i,;;,2]]+ySh[[i,;;,2]]}],{i,Length[list]}]
]
,
True,
Print["Error: no case found. dimL=",dimL,", dimA=",dimA];
]
]


\[Nu]FourierD2[f_]:=Module[{\[Nu]Tmp, yTmp,l,dt},
	l=Length[f];
	dt=(f[[2,1]]-f[[1,1]]);
	\[Nu]Tmp=Table[1/(l dt) (i-(Floor[(l+1)/2])),{i,0,l-1}];
	yTmp=Sqrt[dt]Fourier[Transpose[f][[2]]];
	yTmp=Join[yTmp[[Floor[l/2]+1;;]],yTmp[[;;Floor[l/2]]]];
	Transpose[{\[Nu]Tmp,yTmp}]
];
(*\:0444\:0443\:043d\:043a\:0446\:0438\:044f \:043f\:0440\:0438\:043d\:0438\:043c\:0430\:0435\:0442 \:0441\:043f\:0438\:0441\:043e\:043a \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 f \:043d\:0430 \:0441\:0438\:043c\:043c\:0435\:0442\:0440\:0438\:0447\:043d\:043e\:043c \:0438\:043d\:0442\:0435\:0440\:0432\:0430\:043b\:0435 \:043f\:043e t \:0441 \:0448\:0430\:0433\:043e\:043c dt{{Subscript[f, -n],-ndt},...,{Subscript[f, -1], -dt},{Subscript[f, 0],0},{Subscript[f, 1], dt},{Subscript[f, 2], 2dt},...,{Subscript[f, n], ndt}}, \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0435\:0433\:043e \:0434\:0438\:0441\:043a\:0440\:0435\:0442\:043d\:043e\:0435 \:043f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:043e\:0432\:0430\:043d\:0438\:0435 \:0424\:0443\:0440\:044c\:0435 {{Overscript[f, ~](-n\[CapitalDelta]\[Omega]), -n\[CapitalDelta]\[Omega]},...,{Overscript[f, ~](-\[CapitalDelta]\[Omega]), -n\[CapitalDelta]\[Omega]},{Overscript[f, ~](0),0},{Overscript[f, ~](\[CapitalDelta]\[Omega]), n\[CapitalDelta]\[Omega]},...,{Overscript[f, ~](n\[CapitalDelta]\[Omega]), n\[CapitalDelta]\[Omega]}}*)

\[Nu]FourierD[f_]:=Module[{t0,len,len2},
	len=Length[f];
	len2=If[EvenQ[len],len-1,len];
	t0=f[[Ceiling[len2/2],1]];
	\[Nu]FourierD2[Transpose[{f[[;;,1]]-t0,f[[;;,2]]}][[;;len2]]]
];
(*\:0444\:0443\:043d\:043a\:0446\:0438\:044f \:043f\:0440\:0438\:043d\:0438\:043c\:0430\:0435\:0442 \:0441\:043f\:0438\:0441\:043e\:043a \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 f \:043d\:0430 \:0438\:043d\:0442\:0435\:0440\:0432\:0430\:043b\:0435 \:043f\:043e t \:0441 \:0448\:0430\:0433\:043e\:043c dt{{Subscript[f, 0],0},{Subscript[f, 1], dt},{Subscript[f, 2], 2dt},...,{Subscript[f, n], ndt}}, \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:0435\:0433\:043e \:0434\:0438\:0441\:043a\:0440\:0435\:0442\:043d\:043e\:0435 \:043f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:043e\:0432\:0430\:043d\:0438\:0435 \:0424\:0443\:0440\:044c\:0435 {{Overscript[f, ~](-n\[CapitalDelta]\[Omega]), -n\[CapitalDelta]\[Omega]},...,{Overscript[f, ~](-\[CapitalDelta]\[Omega]), -n\[CapitalDelta]\[Omega]},{Overscript[f, ~](0),0},{Overscript[f, ~](\[CapitalDelta]\[Omega]), n\[CapitalDelta]\[Omega]},...,{Overscript[f, ~](n\[CapitalDelta]\[Omega]), n\[CapitalDelta]\[Omega]}}*)

\[Nu]Fourier[f_,tmin_,tmax_,\[Omega]max_]:=Module[{dt,n,fTmpList},
	n=Ceiling[(\[Omega]max (tmax-tmin))/\[Pi]];
	If[OddQ[n],n=n+1];
	dt=(tmax-tmin)/n;
	fTmpList=Table[{\[CapitalDelta]t-(tmax-tmin)/2,f[tmin+\[CapitalDelta]t]},{\[CapitalDelta]t,0,tmax-tmin,dt}];
	\[Nu]FourierD2[fTmpList]
];(*\:043f\:0440\:0438\:043d\:0438\:043c\:0430\:0435\:0442 \:0444\:0443\:043d\:043a\:0446\:0438\:044e f[t] \:0438 \:0432\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:043a\:0432\:0430\:0434\:0440\:0430\:0442 \:043c\:043e\:0434\:0443\:043b\:044f \:0435\:0435 \:0444\:0443\:0440\:044c\:0435-\:043f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:043e\:0432\:0430\:043d\:0438\:044f \:043d\:0430 \:043e\:0442\:0440\:0435\:0437\:043a\:0435 [tmin,tmax] \:0441 \:043c\:0430\:043a\:0441\:0438\:043c\:0430\:043b\:044c\:043d\:043e\:0439 \:0447\:0430\:0441\:0442\:043e\:0442\:043e\:0439 \[GreaterEqual] \[Omega]max \:0432 \:0432\:0438\:0434\:0435 \:0441\:043f\:0438\:0441\:043a\:0430 {{Overscript[f, ~](-n\[CapitalDelta]\[Omega]), -n\[CapitalDelta]\[Omega]},...,{Overscript[f, ~](-\[CapitalDelta]\[Omega]), -n\[CapitalDelta]\[Omega]},{Overscript[f, ~](0),0},{Overscript[f, ~](\[CapitalDelta]\[Omega]), n\[CapitalDelta]\[Omega]},...,{Overscript[f, ~](n\[CapitalDelta]\[Omega]), n\[CapitalDelta]\[Omega]}}*)



XYArea[XYData_,intRange_]:=Module[{tmpData},
	(*Function integrates data over the int Range using sum over elements*)
	tmpData=Select[XYData,InRangeQ[#[[1]],intRange]&];
	tmpData[[;;-2,2]].Differences[tmpData[[;;,1]]]
]


(*reflects XYData about x0 in x-direction*)
XReflect[XYData_,x0_]:=Transpose[ {2x0-XYData[[;;,1]],XYData[[;;,2]]}]


(**
Function takes 2D array of data (list_) 
and rescales it along Y coordinate to be within [0,1]
**)
YNormalize[list_]:=
	Module[{ymin,ymax},
	ymin=Min[list[[;;,2]]];
	ymax=Max[list[[;;,2]]];
	Transpose[{list[[;;,1]],(list[[;;,2]]-(ymax+ymin)/2) 2/(ymax-ymin)}]
];


XYFindPeaks[XYData_,\[Sigma]:Except[_?OptionQ]:0,s:Except[_?OptionQ]:0,t:Except[_?OptionQ]:-\[Infinity],opts:OptionsPattern[{FindPeaks,sign-> 1}]]:=
Module[{peakIndexList,peakValList,xInterp},
	{peakIndexList,peakValList}=Transpose[FindPeaks[Sign[OptionValue[sign]]*XYData[[;;,2]],\[Sigma],s,t]];
	xInterp=Interpolation[XYData[[;;,1]],InterpolationOrder->OptionValue[InterpolationOrder]];

	Transpose[{Thread[xInterp[peakIndexList]],peakValList}]
];


(* ::Section::Closed:: *)
(*End*)


End[]


EndPackage[]
