(* ::Package:: *)
(* ::Section::Closed:: *)
(*Begin package*)
BeginPackage["Utilities`DataAnalysis`"]


(* ::Section:: *)
(*Description*)

(*Data analysis: manipulations with xylists, fitting, batch data processing*)


(* ::Subsubsection::Closed:: *)
(*Fitting*)
FindLogFit::usage="FindLogFit[data_, expr_, rest__]. The function is equivalent to FindFit, but works in log Y scale."

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
	


(* ::Section:: *)
(*Body*)
Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Fitting*)


FindLogFit[data_,expr_,rest__]:=Module[{logData},
(*The function is equivalent to FindFit, but works in log Y scale*)
	logData=Which[
		VectorQ[data],Log[Abs[data]],(*data list*)
		XYListQ[data],Transpose[{data[[;;,1]],Log[Abs[data[[;;,2]]]]}], (*XY data*)
		True,{}
	];
	FindFit[logData,Log[expr],rest]
];


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


(* ::Section::Closed:: *)
(*End*)
End[]
EndPackage[]