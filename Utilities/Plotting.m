(* ::Package:: *)

(* ::Section:: *)
(*Description*)


(*Functions and definitions for data plotting*)


(* ::Subsection::Closed:: *)
(*Functions*)


ListPlotJ::usage=""
ListLogPlotJ::usage=""
ListLogLogPlotJ::usage=""
ListLogLinearPlotJ::usage=""

PlotGrid::usage="PlotGrid[l_List, w_, h_]"

GenerateGridLines::"usage"="GenerateGridLines[xMin_,xMax_,step_,OptionsPattern[]] or GenerateGridLines[xMin_,xMax_,step_,subStep_,OptionsPattern[]]
	The function is used to generate custom grid lines, possibly with different style for main and sub grid lines.
	In the case of exponential scaling, subStep is a multiplier for the first sub grid line with respect to the previous main one.
	Options: {MainGridLinesStyle\[Rule]Null, SubGridLinesStyle\[Rule]Null, LogScale\[Rule]False}"
	
SweepListLogPlot::usage="SweepListLogPlot[plotData_,opts]"


(* ::Subsection:: *)
(*Definitions*)


(*Plotting options*)

defaultPlotFrameOptions::usage=""

defaultPlotGridOptions::usage=""


(*Color lists*)

(*97 is the standard colors list in Mathematica 10-11*)
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
	
BlueColors::usage="BlueColors[n_] gives a list of blue color shades of the length n_"

RedColors::usage="RedColors[n_] gives a list of red color shades of the length n_"

GreenColors::usage="GreenColors[n_] gives a list of green color shades of the length n_"


(* ::Section:: *)
(*Body*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Definitions*)


defaultPlotFrameOptions=Sequence[
	Frame->True,
	LabelStyle->Directive["Arial",10],
	FrameStyle->Directive[Black,AbsoluteThickness[1]]
];


defaultPlotGridOptions=Sequence[GridLines->Automatic,
	GridLinesStyle->Directive[LightGray,AbsoluteThickness[1]]
];


(* ::Subsection:: *)
(*Sets of colors*)


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


(*Lists of colors with variable numbers of elements*)
RedColors[n_]:=Table[
	Blend[{ColorData[2,"ColorList"][[3]],ColorData[2,"ColorList"][[1]],ColorData[5,"ColorList"][[1]]},x],
{x,0,1,1/(n-1)}];

BlueColors[n_]:=Table[
	Blend[{CMYKColor[1,0,0,.25],CMYKColor[1,0.7,0,.25]},x],
{x,0,1,1/(n-1)}];

GreenColors[n_]:=Table[
	Blend[{ColorData[38,"ColorList"][[3]],CMYKColor[1,0,1,.5]},x],
{x,0,1,1/(n-1)}];


(* ::Subsection:: *)
(*Functions*)


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


SweepListLogPlot[plotData_,opts:OptionsPattern[{Plot}]]:=Module[{plotStyleList,colList},
	colList={Red,Black};
	plotStyleList=Table[Directive[Blend[colList,x],Thin],{x,0,1,1/(Length[plotData]-1)}];
	
	ListLogPlot[plotData,opts,PlotStyle-> plotStyleList]
];


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*End*)


End[]
