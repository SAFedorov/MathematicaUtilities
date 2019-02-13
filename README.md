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

## Acknowledgements

The family of functions MapX.. is adopted with revision from the V. Sudhir's He3Analysis package.
Also a few functions are based on or completely borrowed from the discussions at mathematica.stackexchange.com and
stackoverflow.com, with appropriate references in such case. 

## Getting started

The most commonly useful functions are those contained in `DataAnalysis`, especially the ones that work with lists of `{x,y}` values (the predominant type of experimental data). These functions are typically automatically threaded over lists of such xy traces.

- `ScaleY` is the most commonly used general purpose function that can rescale a list of xy data in X and Y directions and shift the origin in a single command. 
 
Other frequently used function include:
- `SelectRange` – pick data points which x values belong to a certain range. Also the lower-level function `InRangeQ` that it uses is useful per se.
- `IntegrateXY` – routine that efficiently integrates y(x) represented as a xy list while nicely handling arbitrary x boundaries and disconnected intervals (say, one can define a complex interval with gaps and ingrate over it)
- `Average` – routine that performs a running average together with subsequent downsampling, while taking care so that the averaging region is always centered around the corresponding downsampled x values

- `FindLogFit` – I used it for robust fitting of spectrum analyzer data
- `ThreadFindFit` – threads a fitting function over a list of xylists, possibly restricting to the x values within a certain interval
- `LoadDataSeries` – loads files that have parameters in their name (say "spectrum_power_x_mW.txt") and returns data vs parameter
Test patterns can be also useful, for example FunctionQ or `XYListQ`
 
The plotting package includes the definitions of a few gradient color sets (e.g. `redColors` and `blueColors`). These gradients are handy to use for quickly beautifying plots. Also there are:
- `ListPlotJ` – a family of functions that plot discrete data together with a line that joins the data points – a surprisingly annoying task in Mathematica to my taste. 
- `PlotExplorer`  - a function that makes Mathematica plots interactive and adds a capability of zooming into the regions of plot. But this fuction can be not very responsive with large datasets so I did not use it much in practice. 
 
Other parts are slightly more specialized. `CellReuse` package implements copy-pasting of scripts with replacing certain labels in the variable names, which is convenient at the stage when one has a script that analyzes one set of data and then needs to apply it to a different dataset while keeping the previous data still in the Mathematica workspace. The usage of this package is illustrated in the manual.  
 
-`FemTools` include a couple of function for importing and processing data saved from COMSOL (like parametric sweeps), and also efficient integration routines on 2D and 3D triangular meshes.
 

## Implementation remarks

For those .m files which have associated .nb files with the same name, the package is automatically generated from .nb. This system is convenient in that it allows to have complex formatting of comment text. The package is loaded as usual when you add in on Mathematica path and execute `<< "Utilities.m"`. By default it loads all the sub-packages, which you can also load separately.
