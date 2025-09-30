
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Plotting-High-Dimensional-Data

Good visualization tools can allow for an overview of complex data and
interactions. This repository hosts a small collection of solutions to
generate plots of high
dimensionality.

<!-- Links: start -->

| Quick Links                                                                                                                                                             |                                                              |
| [**Visualizing High Dimensional Data**](http://htmlpreview.github.io/?https://github.com/hortibonn/Plotting-High-Dimensional-Data/blob/master/HighDimensionalData.html) |
| [**High Dimensional Data Wiki**](https://github.com/hortibonn/High%20Dimensional%20Data/wiki)                                                                           |

<!-- Links: end -->

See the folder `Radial` for R script examples of how to plot some
interesting things (these are mostly covered in the vignette).

### Large files in GitHub

To solve the problem of files too heavy for GitHub we pushed with the
command ‘git push origin master’ which allows pushing up to 100MB files.
This is not quite big enough to add results of Monte Carlo model
examples yet. We will need to keep looking for options to get files over
100MB in the repository (in repositories in general).

We also tried to use `saveRDS`, which may be a work around but is not
yet helpful.

`MC_Results_HG<-read.csv("mcSimulationResults_HG.csv")`

`saveRDS(MC_Results_HG,"mcSimulationResults_HG.rds")`

`MC_Results_UA<-read.csv("mcSimulationResults_AD.csv")`

`saveRDS(MC_Results_UA,"mcSimulationResults_AD.rds")`
