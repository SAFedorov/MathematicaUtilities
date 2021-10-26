(* ::Package:: *)

(*Author: Sergey Fedorov (SAFedorov)*)

(*Main file that loads all the package components, which can be also loaded separately*)
BeginPackage["Utilities`"]

(*The base folder named "Utilities" should be on the Mathematica $Path*)
Get[FileNameJoin[{"Utilities","DataAnalysis.m"}]]
Get[FileNameJoin[{"Utilities","Plotting.m"}]]
Get[FileNameJoin[{"Utilities","Fourier.m"}]]
Get[FileNameJoin[{"Utilities","Compatibility.m"}]]
Get[FileNameJoin[{"Utilities","FemTools.m"}]]
Get[FileNameJoin[{"Utilities","Optimization.m"}]]
Get[FileNameJoin[{"Utilities","CellReuse.m"}]]

utilitiesStyleGuide="
Variables are named camelCase 
Functions and options are names CamelCase 
Long abbreviations like FEM, GUI in names are written as regular words starting from a single capital letter,
e.g. FemTools, ComsolImport etc.


Usage messages for functions (called by ?FunctionName) follow the template:

FunctionName[arg1_, arg2_]
	General description

Input:
	arg1_ is ...
	arg2_ is ...

Interactive operation:
	If the function has any interative elements, they are described here

Output:
	{x1,x2,x3} - list of values ...

Options:
	Opt1 -> Default - the options sets ...

Example:
	Usage examples
"

EndPackage[]
