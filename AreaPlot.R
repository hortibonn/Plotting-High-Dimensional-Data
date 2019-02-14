#Plotting plant use area plant types RStudio .pdf custom export 18*17 inch

MC_Results_UA<-read.csv("MC_Examples/mcSimulationResults_AD.csv")
MC_Results_HG<-read.csv("MC_Examples/mcSimulationResults_HG.csv")

library(plyr)
MC_Results<-rbind.fill(MC_Results_UA,MC_Results_HG)
MC_Results$ID<-seq.int(nrow(MC_Results)) #create unique rownames
#write.csv(MC_Results, "MC_Results.csv", sep="", row.names=T) 

#Coverage UA==============
mccoverage<-MC_Results[,grep("coverage_", colnames(MC_Results)), ]
names(mccoverage)
#Pulse<-mccoverage[,grep("PULSE", colnames(mccoverage)), ]
#Staple<-mccoverage[,grep("STAPLE", colnames(mccoverage)), ]
#Fruit<-mccoverage[,grep("FRUIT", colnames(mccoverage)), ]
#Vegetable<-mccoverage[,grep("VEG", colnames(mccoverage)), ]
#Spice<-mccoverage[,grep("SPICE", colnames(mccoverage)), ]

mccoverage$UA_Pulse<-rowSums(mccoverage[,c(2:19)])
mccoverage$HG_Pulse<-rowSums(mccoverage[,c(348:449)])
mccoverage$UA_Staple<-rowSums(mccoverage[,c(20:41)])
mccoverage$HG_Staple<-rowSums(mccoverage[,c(246:347)])
mccoverage$Fruit<-rowSums(mccoverage[,c(42:143)])
mccoverage$Vegetable<-rowSums(mccoverage[,c(144:245)])
mccoverage$Spice<-rowSums(mccoverage[,c(450:551)])
mccoverage$Staple<-rowSums(mccoverage[,c(553:554)])
mccoverage$Pulse<-rowSums(mccoverage[,c(551:552)])
#mccoverageUA<-mccoverage[,c(558,551:552)]
#library(splitstackshape)
#mccoverageHG<-mccoverage[,c(558,553:557)]
#Split in two farming scenarios and then run the plot for both
#plot everything together and then send to Eike with code for ideas
#mccoverage$ha_Pulse<-rowSums(mccoverage[,c(2:19,348:449)])
#mccoverage$ha_Staple<-rowSums(mccoverage[,c(20:41,246:347)])
#mccoverage$ha_Fruit<-rowSums(mccoverage[,c(42:143)])
#mccoverage$ha_Vegetable<-rowSums(mccoverage[,c(144:245)])
#mccoverage$ha_Spice<-rowSums(mccoverage[,c(450:551)])
mccoverage$ID<-MC_Results$ID
mccoverage$scenario<-MC_Results$farmer_scenario

names(mccoverage)
mccoverage1<-mccoverage[,c(560,561,555:559)]
names(mccoverage1)
library(reshape2)
mccoverage2<-melt(mccoverage1, id.vars = c('ID','scenario'))
names(mccoverage2) #"ID"       "scenario" "Ha"       "value"  
mccoverage2[is.na(mccoverage2)] <- 0.1
#mccoverage2<-na.omit(mccoverage2)
mccoverage2$value<-round(mccoverage2$value*1)
mccoverage3 <- mccoverage2[ rep( seq(dim(mccoverage2)[1]), mccoverage2$value),] #repeat rows for each stem
rawug$PrimaryUseName <- mapvalues(rawug$PrimaryUse, 
                                  from = c("AF", "Fence", "FRUIT", "MED", 
                                           "PULSE", "Sale", "SPICE", "STAPLE", "TECH", "VEG","Wood"), 
                                  to = c("Animal feed", "Fence", "Fruit", "Medicine", 
                                         "Pulses","Sale","Spice","Staple","Technical", "Vegetable", "Wood"))
#mccoverage2<-as.data.frame(melt(mccoverage1, id.vars = 'ID',  na.rm = TRUE)[, c('ID','value')])
#names(mccoverage2)
#mccoverageHG1<-na.omit(mccoverageHG)
#mccoverage2 <- reshape(mccoverage1, direction = "long", idvar="ID", 
#                        varying=2:ncol(mccoverage1), sep = "ha_", timevar="Type")
#names(mccoverageUA1)#"ID"      "Type"    "annual_"
#mccoverage1[mccoverage1 == NA] <- "NA"
#mccoverage2 <- na.omit(reshape(mccoverage1, direction = "long", idvar="ID", 
#                        varying=2:ncol(mccoverage1), sep = "a_", timevar="Type"))
#names(mccoverageHG1)#ID"   "Type" "HG" 
#library(reshape)
#melt(mccoverageHG1, id.vars=c('ID', 'Type'),var='ha')

library(ggplot2)
PlotOne<-ggplot() + 
  geom_bar(data = mccoverage3,
           aes(x = factor(scenario),fill = factor(variable)),
           position = "fill")+  #scale_fill_manual(values = getPalette(colourCount)) +      # Set legend title
  xlab("") + ylab("") + # Set axis labels
  scale_y_continuous(labels=percent) +
  theme_bw()+
  theme(plot.margin = unit(c(.5,.001,.5,.01), "cm"))+ #top, right, bottom and left
  theme(legend.position="none",
        axis.text.x = element_text(angle=60, face="bold", hjust = 1, family='serif', size=30),
        axis.text.y = element_text(family='serif', size=30))+
  ggtitle("Homegarden crops") +   
  theme(plot.title=element_text(family='serif', size=45, face="bold"))     # Set title
print(PlotOne)



