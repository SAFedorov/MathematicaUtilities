# MathematicaUtilities

This package contains useful function for common tasks arising in data processing and calculations using Mathematica. 
The content is split into a few sub-packages, each of which can be loaded independently and only relies on the functions 
from the DataAnalysis package.   
	Components:
	
	DataAnalysis - Functions for manipulations with lists of {x, y} and {x, y, z} data, generally useful patterns, 
		Fitting routines, batch data analysis
	CellReuse - A special set of functions to rerun Mathematica code with difrent inputs in the same notebook.
	Plotting - Definitions of custom color lists, plotting options and a few plotting functions beyond Mathematica
		standard set. Also an interactive plot browser. 
	Fourier - High-level routines for performing DFT on sets of {x, y} data 
	Compatibility - Storage of all the outdated and superseded functions for backward compatibility

Most of the .m packages are not to be edited directly, but via the companion .nb file with
the same name, from which they were generated. Each time the .nb file is saved, .m file is automatically replaced with a 
new one, thus discarding any changes made in the .m file directly.

Tested with Mathematica 11.0

The family of functions MapX.. is adopted with revision from the V. Sudhir's He3Analysis package.
Also a few functions are based on or completely borrowed from the discussions at mathematica.stackexchange.com and
stackoverflow.com, with appropriate references in such case. 