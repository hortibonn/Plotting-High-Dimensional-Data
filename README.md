# Plotting-High-Dimensional-Data

See the html for ideas about how to general plots of high dimensionality.

See the folder `Radial` for R script examples of how to plot some interesting things (mostly covered in the vignette).

The `mcSimulationResults.csv` file comes out of each analysis and can be mined for visualizations. See the `MC_Examples` folder for how to extract and compare model outputs from `decisionSupport`. 

To use the files in the `MC_Examples` folder you must download the decisonSupport output files from Dropbox <https://www.dropbox.com/sh/u20heealspgtw86/AADgFCI4XTzEqNm0v0s4q932a?dl=0> they are too heavy for GitHub. 

However I found `saveRDS` which may be a work around.

MC_Results_HG<-read.csv("mcSimulationResults_HG.csv")
saveRDS(MC_Results_HG,"mcSimulationResults_HG.rds")

MC_Results_UA<-read.csv("mcSimulationResults_AD.csv")
saveRDS(MC_Results_UA,"mcSimulationResults_AD.rds")

