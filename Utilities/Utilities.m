(* ::Package:: *)

(*Main file that loads all the package components, which can be also loaded separately*)
BeginPackage["Utilities`"]

(*The base folder named "Utilities" should be on the Mathematica $Path*)
Get[FileNameJoin[{"Utilities","DataAnalysis.m"}]]
Get[FileNameJoin[{"Utilities","Plotting.m"}]]
Get[FileNameJoin[{"Utilities","Fourier.m"}]]
Get[FileNameJoin[{"Utilities","Compatibility.m"}]]

EndPackage[]
