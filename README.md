# Segmented-Regression-for-Wells
A set of R scripts and data to estimate the timing of breakpoints in water table trajectories over time (i.e. changes in hydrograph slope). Generally, these scripts take .csv inputs, and write .csv outputs that are fed into subsequent scripts. In some cases, the scripts are split between analyses in the Northern, Central, and Southern High Plains (NHP, CHP, SHP). The 'segmented' package in R was written by Vito Muggeo. The results of this project for the High Plains Aquifer were submitted to the journal *Agricultural Water Management* 8/25/2018. This repository is still under development. Inputs and intermediary files will be added to the repository once the article is accepted; in the meantime they are available on request from Erin Haacker (ehaacker2@unl.edu). 

Run order:
Munge1 (optional - skips first well record; this is useful if trying to find segmented regression for *change* in water table from previous year)

Munge2_FillMissingData - linear interpolation between data points
  ***Note: This is both dangerous and necessary. The problem is that it smooths the data series and possibly elides the actual "breakpoint" location. It is necessary because otherwise a gap in data can create the illusion of a breakpoint where there is none - imagine a well that is dropping 0.5 m/year that is measured every year for a decade, then every other year for two decades; without filling in the missing years, it looks like there's a breakpoint at the end of the first decade, where the slope of the hydrograph goes from -0.5 to -1.***

SegReg_loop_Wells_MonteCarlo_WTelev - this is the workhorse script, containing:
  -a segmented regression function, which loops through input well hydrographs
  -functionality to run each well 100 times (or any arbitrary number) because the initial breakpoint estimate is random, and the output can be sensitive to this
  -a "TryCatch" statement so that the script will keep going if it doesn't find a breakpoint

SegReg_ggPlotter - this script is set up to graph one well hydrograph at a time, for data exploration; takes the same primary input as the segmented regression loop script above

MonteCarloCruncher_R_mode - this script summarizes the outputs from the SegReg_loop_Wells_MonteCarlo_WTelev script

...There may have been some steps in Microsoft Access here...

ResultsExplorer - takes summarized segmented regression results and input results, and creates several plots

PyramidsOfGMAs - "GMA" stands for Groundwater Management Area. This script creates "pyramid" plots of water table trajectory change (breakpoints) with respect to the establishment of GMAs on the High Plains. And calculates mean precipitation by region. hmmm

...Possibly some more steps in Access and Excel...

Multiple_Linear_Regression and ARIMA scripts - perform additional analyses to correlate breakpoints with precipitation/drought and crop prices. MLR seems to be an earlier version of analyses included in ARIMA scripts. 
