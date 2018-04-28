(* ::Package:: *)

(* ::Section::Closed:: *)
(*Old versions log*)


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
(**)
(*log last modified: 2017, Sergey Fedorov*)


(* ::Section::Closed:: *)
(*Description*)


(*This file contains outdated functions or function names to ensure compatibility with previous package versions*)

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


End[]
