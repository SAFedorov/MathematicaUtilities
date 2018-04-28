# MathematicaUtilities

This package contains useful function for common tasks arising in data processing and calculations using Mathematica. 
The content is split into a few sub-packages, each of which can be loaded independently and only relies on the functions 
from the General.m package.   
	Components:
	
	General -
	DataAnalysis - 
	Fourier - 
	CellReuse - 
	Compatibility - file to store for backward compatibility all the outdated and superseded functions

Most of the .m packages are not to be edited directly, but via the companion .nb file with
the same name, from which they were generated. Each time the .nb file is saved, .m file is automatically replaced with a 
new one, thus discarding any changes made in the .m file directly.

Tested with Mathematica 11.0

The family of functions MapX.. is adopted with revision and extension from the V Sudhir's He3Analysis package.
Also a few functions are heavily based on those from the discussions at mathematica.stackexchange.com and 
referenced in such case. 