#Create a radial plot with identifying data for diff parts####

#Data####

data <- structure(list(Indicator = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 
3L, 3L, 3L, 6L, 6L, 6L, 6L, 4L, 4L, 5L, 5L, 1L, 1L, 1L, 2L, 2L, 
2L, 3L, 3L, 3L, 6L, 6L, 6L, 6L, 4L, 4L, 5L, 5L, 1L, 1L, 1L, 2L, 
2L, 2L, 3L, 3L, 3L, 6L, 6L, 6L, 6L, 4L, 4L, 5L, 5L, 1L, 1L, 1L, 
2L, 2L, 2L, 3L, 3L, 3L, 6L, 6L, 6L, 6L, 4L, 4L, 5L, 5L, 1L, 1L, 
1L, 2L, 2L, 2L, 3L, 3L, 3L, 6L, 6L, 6L, 6L, 4L, 4L, 5L, 5L), .Label = c(
  "Causality",
  "Climatechangeriskperceptions",
  "Currentadaptationoptions",
  "Fishingasalivelihoodactivity",
  "Governance",
  "Roleofshadowstateactors"
), class = "factor"),
                       Village = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                             1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                             2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
                                             3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 
                                             4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 
                                             5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
                                             5L), 
              .Label = c("Andra", "lahapau", "Pelipowai", "Ponam", 
              "Tulu"), class = "factor"), Variables = structure(c(13L, 
                   3L, 10L, 11L, 12L, 16L, 5L, 8L, 1L, 2L, 15L, 17L, 6L, 14L, 
                   9L, 4L, 7L, 13L, 3L, 10L, 11L, 12L, 16L, 5L, 8L, 1L, 2L, 
                   15L, 17L, 6L, 14L, 9L, 4L, 7L, 13L, 3L, 10L, 11L, 12L, 16L, 
                   5L, 8L, 1L, 2L, 15L, 17L, 6L, 14L, 9L, 4L, 7L, 13L, 3L, 10L, 
                   11L, 12L, 16L, 5L, 8L, 1L, 2L, 15L, 17L, 6L, 14L, 9L, 4L, 
                   7L, 13L, 3L, 10L, 11L, 12L, 16L, 5L, 8L, 1L, 2L, 15L, 17L, 
                   6L, 14L, 9L, 4L, 7L), .Label = c(
                     "alternativelivelihood",
                     "anyactorsinvolvedinsustainability",
                     "Attributionfactors",
                     "discusswithelectedleaders",
                     "Effortsdirectedtoreducerisks",
                     "fishercommunityinfluence",
                     "Infrastructureeffectiveness",
                     "multiplicityofactors",
                     "Occupationforchildren",
                     "Reversibility",
                     "Riskasamajorconsideration",
                     "Riskbeingaddressed",
                     "Statusoffisheries",
                     "Timefishing",
                     "Whatwasdone",
                     "Whoisatrisk",
                     "whowasinvolved?"
                   ), class = "factor"), legend.var = structure(c(1L, 2L, 3L, 
                   4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 
                   17L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 
                   14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                   11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 
                   7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 
                   3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 
                   16L, 17L), 
.Label = c(letters[1:17]), class = "factor"), 
                       score = c(1, 0.97, 1, 0.76, 0.794, 1, 0.71, 0.9, 0.5, 1, 
                                 1, 1, 1, 0.49, 0.72, 1, 0.7, 1, 1, 0, 0.67, 0.5, 1, 0.2, 
                                 1, 1, 0.7, 0.4, 0.5, 0.3, 0.67, 0.5, 0.7, 0.8, 1, 0, 0.46, 
                                 0.56, 0.375, 1, 0.13, 0.3, 0.5, 0.3, 0.3, 0.4, 0.6, 1, 1, 
                                 0.7, 0.8, 1, 0.86, 0.69, 0.51, 0.429, 1, 0.44, 0.3, 0.5, 
                                 0.6, 0.6, 0.7, 0.8, 0.4, 0.79, 0.8, 1, 1, 0.82, 0.85, 0.25, 
                                 0.226, 1, 0.18, 0.1, 1, 0.7, 0.3, 0.6, 0.3, 0.48, 0.16, 0.4, 
                                 0.8)), .Names = c("Indicator", "Village", "Variables", "legend.var", 
    "score"), class = "data.frame", row.names = c(NA, -85L))

#Plot####

library(ggplot2)
ggplot(data, aes(x = legend.var, y = score, fill = Indicator), color='black') + 
  geom_bar(width = 1, alpha=0.5, stat="identity") + 
  scale_y_continuous() + 
  coord_polar()  + 
  theme( axis.ticks = element_blank()) + 
  facet_wrap(~Village, nrow=2, ncol=3) + 
  guides(colour = guide_legend(title.hjust = 0.5))

