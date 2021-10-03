# MathematicaUtilities

An experimental physicist's aid in common data analysis routines.
This package contains a set of functions useful in processing data acquired from laboratory instruments and in numerical calculations using Mathematica. Since the majority of experimental data comes in the form of xy lists (`{{x1, y1}, {x2, y2}, ...}`), the core of the package is dedicated to handling and analysis of data in this format. 

The content is split into a few sub-packages, each of which only relies on functions within itself and DataAnalysis sub-package. 

The components:
	
	DataAnalysis - Functions for manipulations with lists of {x, y} and {x, y, z} data, useful patterns, 
		fitting routines, batch data analysis etc.
	CellReuse - A set of functions for copying Mathematica cells with replacing name tags. Doing so allows 
		to rerun once-written code for diffrent input data in one notebook while keeping all variables 
		global.
	Plotting - Definitions of custom color lists, plotting options and a few plotting functions 
		beyond the Mathematica standard catalog. Also has an interactive plot browser. 
	Fourier - High-level routines for performing Discrete Fourier Transform on sets of {x, y} data. 
	Compatibility - A repository of outdated and superseded functions kept for backward compatibility.

Among others, the packages is used in [Euler-Bernoulli beam](https://zenodo.org/record/1296925#.YVon7ZqxVPY).

Tested with Mathematica 11.0

## Getting started

The package is loaded as usual by adding its root directory on the Mathematica path and executing `<< "Utilities.m"`. Doing this loads all sub-packages.

To see help for a function, use `??` command, such as
```
?? ScaleX
```

The most commonly useful functions in my experience are those from `DataAnalysis`, especially those that work with lists of `{x,y}` values - the predominant format of instrument traces. They include

- `ScaleY`, `ScaleX`, `ShiftY` and `ShiftX` can rescale a list of xy data in X and Y directions and shift the origin in a single command. 
 
- `SelectRange` – pick data points which x values belong to a certain range. Also the lower-level function `InRangeQ` that it uses is useful per se.
- `IntegrateXY` – a routine that efficiently integrates y(x) represented as a xy list and nicely handles arbitrary x boundaries and disjoined intervals (say, x ranging from 1 to 1.5 *and*  2 to 2.5)
- `Average` – performs a running average of an xy list with subsequent downsampling. It takes care so that the averaging region is always centered around the corresponding downsampled x values.

- `FindLogFit` – Fitting on log scale. Can be used to make fitting more robust when the magnitude of data in one trace spans several different scales.
- `ThreadFindFit` – threads a fitting function over a list of xylists, possibly restricting to the x values within a certain interval.
- `LoadDataSeries` – loads files that have parameters in their name (say "spectrum_power_x_mW.txt") and returns data vs parameter
- Test patterns can be also useful, for example see `FunctionQ` or `XYListQ`.

Note that these functions are typically automatically threaded over lists of xy traces.
 
The plotting package includes the definitions of a few gradient color sets (e.g. `redColors` and `blueColors`). These gradients are handy for quickly beautifying plots. Also there are:
- `ListPlotJ` – a family of functions that plot discrete data points and lines that join them at the same time. This is a surprisingly annoying thing to implement in Mathematica. 
- `PlotExplorer`  - a function that makes Mathematica plots interactive and adds interactive zoom capabilities. Beware that it can be slow with large datasets. 
 
Other sub-packages are slightly more specialized. `CellReuse` package implements copy-pasting of scripts with replacing certain labels in the variable names, which is convenient at the stage when one has a script that analyzes one set of data and then needs to apply it to a different dataset while keeping the previous data still in the Mathematica workspace.   
 
`FemTools` includes a couple of function for importing and processing data saved from COMSOL (such as parametric sweeps), and also efficient integration routines on 2D and 3D triangular (tetrahedral) meshes.
 

## Implementation remarks

Most of the .m packages are not to be edited directly, but using the companion .nb files with
the same name, from which they were generated. Each time the .nb file is saved, .m file is automatically replaced with a 
new one, thus discarding any changes made in the .m file directly. This system is followed because notebooks allow 
reacher formatting and embedding images, which is very useful for illustrating complex mathematical formulas.

Those .m files that do not have associated .nb files with the same name can be edited directly. 


## Acknowledgements

The family of functions MapX.. is adopted with revision from the V. Sudhir's He3Analysis package.
Also a few functions are based on or completely borrowed from the discussions at mathematica.stackexchange.com and
stackoverflow.com, with appropriate references in such case. 
