(* ::Package:: *)

(* ::Section:: *)
(*Description*)


(*This file useful functions and definitions for plotting*)


(* ::Subsection::Closed:: *)
(*Functions*)


ListPlotJ::usage=""
ListLogPlotJ::usage=""
ListLogLogPlotJ::usage=""
ListLogLinearPlotJ::usage=""

PlotExplorer::usage="PlotExplorer[plot_]"

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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection:: *)
(*End*)


End[]
