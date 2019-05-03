# Plotting-High-Dimensional-Data

See the html for ideas about how to general plots of high dimensionality. <http://htmlpreview.github.io/?https://github.com/CWWhitney/Plotting-High-Dimensional-Data/blob/master/HighDimensionalData.html>

See the folder `Radial` for R script examples of how to plot some interesting things (these are mostly covered in the vignette).

### Dealing with large files in GitHub

To solve the problem of files too heavy for GitHub I pushed with the command 'git push origin master' which allows pushing up to 100MB files. So now the 'AD' file is online. I wil keep trying for options to get files over 100MB in the repository (in repositories in general). 

I also tried to use `saveRDS`, which may be a work around but is not yet helpful.

MC_Results_HG<-read.csv("mcSimulationResults_HG.csv")

saveRDS(MC_Results_HG,"mcSimulationResults_HG.rds")

MC_Results_UA<-read.csv("mcSimulationResults_AD.csv")

saveRDS(MC_Results_UA,"mcSimulationResults_AD.rds")

