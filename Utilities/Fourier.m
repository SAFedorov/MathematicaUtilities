(* ::Package:: *)
(* ::Section::Closed:: *)
(*Begin package*)
BeginPackage["Utilities`Fourier"]


(* ::Section:: *)
(*Description*)

(*Fourier transforms of functions on a uniform grid*)

\[Nu]FourierXY::usage="\[Nu]FourierXY[xylist_] or \[Nu]FourierXY[f_,xmin_,xmax_,\[Nu]max_] 
	computes in numerical approximation of the Fourier transform in linear units y[\[Nu]]=\!\(\*FractionBox[\(1\), SqrtBox[\(L\)]]\)\!\(\*SubsuperscriptBox[\(\[Integral]\), SubscriptBox[\(x\), \(0\)], \(\*SubscriptBox[\(x\), \(0\)] + L\)]\)y(x)Exp[\[ImaginaryI](2\[Pi]\[Nu])x]\[DifferentialD]x=\!\(\*FractionBox[\(dx\), SqrtBox[\(L\)]]\)\!\(\*UnderoverscriptBox[\(\[Sum]\), \(r = 1\), \(n - 1\)]\)y(\!\(\*SubscriptBox[\(x\), \(r\)]\))Exp[\[ImaginaryI](2\[Pi]\[Nu])\!\(\*SubscriptBox[\(x\), \(r\)]\)];
	Frequency is defined in linear units on the two-sided interval \[Nu]=s/dx, s\[Element][-(n-1)/2, (n-1)/2];
	In the case of odd n the spectrum contains equal number of positive and negative frequency components, in the case of even n there is n/2 positive frequency components and n/2-1 negative frequency components"

\[Omega]FourierXY::usage="\[Nu]FourierXY[xylist_]"

OneFromTwoSidedSpectrum::usage="OneFromTwoSidedSpectrum[sp_,OptionsPattern[\[Epsilon]Zero->10^-5]]"

SymmetrizeSpectrum::usage="SymmetrizeSpectrum[sp_,OptionsPattern[\[Epsilon]Zero->10^-5]]:"


(* ::Section:: *)
(*Body*)
Begin["`Private`"]


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


(* ::Section::Closed:: *)
(*End*)
End[]
EndPackage[]