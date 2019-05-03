#test means from 10,000 model runs (normal dist)
#Nutrient contents per ton of yield for different crops (closely followed the Harvest Plus)

MC_Results_UA<-read.csv("MC_Examples/mcSimulationResults_AD.csv")
MC_Results_HG<-read.csv("MC_Examples/mcSimulationResults_HG.csv")

library(plyr)
MC_Results<-rbind.fill(MC_Results_UA,MC_Results_HG)
MC_Results$ID<-seq.int(nrow(MC_Results)) #create unique rownames

library(ggplot2)
library(multcompView)

mcnut<-MC_Results[,grep("nut_cont_", colnames(MC_Results)), ]
names(mcnut)

#Tukey HSD function========
generate_label_df <- function(HSD, flev){
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(d, flev, function (x) max(fivenum(x$y)) + ((max(fivenum(y))-mean(fivenum(y)))*.1))
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}

#zinc############

mczinc<-mcnut[,grep("zinc", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mczinc$ID<-MC_Results$ID
names(mczinc)
mczinc<-mczinc[,c(6,1:5)]
names(mczinc) 
library(reshape)
mczinc1 <- reshape(mczinc, direction = "long", idvar="ID", 
                   varying=2:ncol(mczinc), sep = "mg_", timevar="Type")
names(mczinc1)#"ID"           "Type"           "nut_cont_"
mczinc1$Type <- mapvalues(mczinc1$Type, 
                                  from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                                  to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
library(ggplot2)
#c <- ggplot(mczinc1, aes(Month, nut_cont_,colour=factor(scenario)))
lev <- as.factor(mczinc1$Type) #gl(3, 10)
#To convert from ton to kg divide by 1000
y <-  mczinc1$nut_cont_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
#ggplot(mczinc1, aes(x=y)) + #illustration: density plot option
#geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
#Print
library(scales)
zinc <- ggplot(mczinc1, aes(x=Type, y=mczinc1$nut_cont_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ 
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+   #four values for the top, right, bottom, and left
  labs(title = expression(bold(Zinc^bold("§"))), x=element_blank(), y = "Zinc (mg/100 g)") #(mg)

print(zinc)


#energy############

names(mcnut)
mcenergy<-mcnut[,grep("energy", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcenergy$ID<-MC_Results$ID
names(mcenergy)
mcenergy<-mcenergy[,c(6,1:5)]
names(mcenergy) 
mcenergy1 <- reshape(mcenergy, direction = "long", idvar="ID", 
                   varying=2:ncol(mcenergy), sep = "kcal_", timevar="Type")
names(mcenergy1)#"scenario"       "ID"           "Type"           "nut_cont_energy_"
mcenergy1$Type <- mapvalues(mcenergy1$Type, 
                          from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                          to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcenergy1$Type) #gl(3, 10)
y <-  mcenergy1$nut_cont_energy_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = F, conf.level = 0.95)
energy <- ggplot(mcenergy1, aes(x=Type, y=nut_cont_energy_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  theme(legend.text=element_text(size=15))+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ 
  #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+
  labs(title = expression(bold(Energy)), x=element_blank(), y = "Energy (kcal/100 g)")#+ ggtitle(expression(atop(bold("Calories"), atop("Energy", ""))))
   
print(energy)

#lipid_total############

  names(mcnut)
mclipid_total<-mcnut[,grep("lipid_total", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mclipid_total$ID<-MC_Results$ID
names(mclipid_total)
mclipid_total<-mclipid_total[,c(6,1:5)]
names(mclipid_total) 
mclipid_total1 <- reshape(mclipid_total, direction = "long", idvar="ID", 
                     varying=2:ncol(mclipid_total), sep = "g_", timevar="Type")
names(mclipid_total1)#"scenario"       "ID"           "Type"           "nut_cont_lipid_total_"
mclipid_total1$Type <- mapvalues(mclipid_total1$Type, 
                            from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                            to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mclipid_total1$Type) #gl(3, 10)
y <-  mclipid_total1$nut_cont_lipid_total_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
lipid_total <- ggplot(mclipid_total1, aes(x=Type, y=nut_cont_lipid_total_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold("Fat" ^bold("†"))), x=element_blank(), y = "Total lipid/Fat (g/100 g)")#(g)
   
print(lipid_total)

#carbohydrate############

  names(mcnut)
mccarbohydrate<-mcnut[,grep("carbohydrate", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mccarbohydrate$ID<-MC_Results$ID
names(mccarbohydrate)
mccarbohydrate<-mccarbohydrate[,c(6,1:5)]
names(mccarbohydrate) 
mccarbohydrate1 <- reshape(mccarbohydrate, direction = "long", idvar="ID", 
                          varying=2:ncol(mccarbohydrate), sep = "g_", timevar="Type")
names(mccarbohydrate1)#"scenario"       "ID"           "Type"           "nut_cont_carbohydrate_"
mccarbohydrate1$Type <- mapvalues(mccarbohydrate1$Type, 
                                 from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                                 to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mccarbohydrate1$Type) #gl(3, 10)
#To convert from grams to kg divide by 1000
y <-  mccarbohydrate1$nut_cont_carbohydrate_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
carbohydrate <- ggplot(mccarbohydrate1, aes(x=Type, y=nut_cont_carbohydrate_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Carbohydrates ^bold("†"))), x=element_blank(), y = "Carbohydrate (g/100 g)")#g
    
print(carbohydrate)

#fiber############

  names(mcnut)
mcfiber<-mcnut[,grep("fiber", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcfiber$ID<-MC_Results$ID
names(mcfiber)
mcfiber<-mcfiber[,c(6,1:5)]
names(mcfiber) 
mcfiber1 <- reshape(mcfiber, direction = "long", idvar="ID", 
                           varying=2:ncol(mcfiber), sep = "g_", timevar="Type")
names(mcfiber1)#"scenario"       "ID"           "Type"           "nut_cont_fiber_"
mcfiber1$Type <- mapvalues(mcfiber1$Type, 
                                  from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                                  to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcfiber1$Type) #gl(3, 10)
y <-  mcfiber1$nut_cont_fiber_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
fiber <- ggplot(mcfiber1, aes(x=Type, y=nut_cont_fiber_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Fiber ^bold("†"))), x=element_blank(), y = "Dietary fiber (g/100 g)")#g
 
print(fiber)

#calcium############
 names(mcnut)
mccalcium<-mcnut[,grep("calcium", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mccalcium$ID<-MC_Results$ID
names(mccalcium)
mccalcium<-mccalcium[,c(6,1:5)]
names(mccalcium) 
mccalcium1 <- reshape(mccalcium, direction = "long", idvar="ID", 
                    varying=2:ncol(mccalcium), sep = "mg_", timevar="Type")
names(mccalcium1)#"scenario"       "ID"           "Type"           "nut_cont_calcium_"
mccalcium1$Type <- mapvalues(mccalcium1$Type, 
                           from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                           to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mccalcium1$Type) #gl(3, 10)
y <-  mccalcium1$nut_cont_calcium_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
calcium <- ggplot(mccalcium1, aes(x=Type, y=nut_cont_calcium_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Calcium^bold("§"))), x=element_blank(), y = "Calcium (mg/k)")#mg
  
print(calcium)

#iron############
  names(mcnut)
mciron<-mcnut[,grep("iron", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mciron$ID<-MC_Results$ID
names(mciron)
mciron<-mciron[,c(6,1:5)]
names(mciron) 
mciron1 <- reshape(mciron, direction = "long", idvar="ID", 
                      varying=2:ncol(mciron), sep = "mg_", timevar="Type")
names(mciron1)#"scenario"       "ID"           "Type"           "nut_cont_iron_"
mciron1$Type <- mapvalues(mciron1$Type, 
                             from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                             to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mciron1$Type) #gl(3, 10)
y <-  mciron1$nut_cont_iron_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
iron <- ggplot(mciron1, aes(x=Type, y=nut_cont_iron_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Iron^bold("§"))), x=element_blank(), y = "Iron (mg/100 g)")#MG
  
print(iron)

#vit_c############
  names(mcnut)
mcvit_c<-mcnut[,grep("vit_c", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcvit_c$ID<-MC_Results$ID
names(mcvit_c)
mcvit_c<-mcvit_c[,c(6,1:5)]
names(mcvit_c) 
mcvit_c1 <- reshape(mcvit_c, direction = "long", idvar="ID", 
                   varying=2:ncol(mcvit_c), sep = "mg_", timevar="Type")
names(mcvit_c1)#"scenario"       "ID"           "Type"           "nut_cont_vit_c_"
mcvit_c1$Type <- mapvalues(mcvit_c1$Type, 
                          from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                          to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcvit_c1$Type) #gl(3, 10)
y <-  mcvit_c1$nut_cont_vit_c_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
vit_c <- ggplot(mcvit_c1, aes(x=Type, y=nut_cont_vit_c_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold("Vitamin C" ^bold("‡"))), x=element_blank(), y = "Vitamin C (mg/100 g)")#mg
   
print(vit_c)

#thiamin############
  names(mcnut)
mcthiamin<-mcnut[,grep("thiamin", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcthiamin$ID<-MC_Results$ID
names(mcthiamin)
mcthiamin<-mcthiamin[,c(6,1:5)]
names(mcthiamin) 
mcthiamin1 <- reshape(mcthiamin, direction = "long", idvar="ID", 
                    varying=2:ncol(mcthiamin), sep = "mg_", timevar="Type")
names(mcthiamin1)#"scenario"       "ID"           "Type"           "nut_cont_thiamin_"
mcthiamin1$Type <- mapvalues(mcthiamin1$Type, 
                           from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                           to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcthiamin1$Type) #gl(3, 10)
y <-  mcthiamin1$nut_cont_thiamin_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
thiamin <- ggplot(mcthiamin1, aes(x=Type, y=nut_cont_thiamin_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Thiamin ^bold("‡"))), x=element_blank(), y = "Thiamin (mg/100 g)")#mg
  
print(thiamin)

#riboflavin############
  names(mcnut)
mcriboflavin<-mcnut[,grep("riboflavin", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcriboflavin$ID<-MC_Results$ID
names(mcriboflavin)
mcriboflavin<-mcriboflavin[,c(6,1:5)]
names(mcriboflavin) 
mcriboflavin1 <- reshape(mcriboflavin, direction = "long", idvar="ID", 
                      varying=2:ncol(mcriboflavin), sep = "mg_", timevar="Type")
names(mcriboflavin1)#"scenario"       "ID"           "Type"           "nut_cont_riboflavin_"
mcriboflavin1$Type <- mapvalues(mcriboflavin1$Type, 
                             from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                             to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcriboflavin1$Type) #gl(3, 10)
y <-  mcriboflavin1$nut_cont_riboflavin_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
riboflavin <- ggplot(mcriboflavin1, aes(x=Type, y=nut_cont_riboflavin_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Riboflavin ^bold("‡"))), x=element_blank(), y = "Riboflavin (mg/100 g)")#mg
  
print(riboflavin)

#niacin############
  names(mcnut)
mcniacin<-mcnut[,grep("niacin", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcniacin$ID<-MC_Results$ID
names(mcniacin)
mcniacin<-mcniacin[,c(6,1:5)]
names(mcniacin) 
mcniacin1 <- reshape(mcniacin, direction = "long", idvar="ID", 
                         varying=2:ncol(mcniacin), sep = "mg_", timevar="Type")
names(mcniacin1)#"scenario"       "ID"           "Type"           "nut_cont_niacin_"
mcniacin1$Type <- mapvalues(mcniacin1$Type, 
                                from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                                to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcniacin1$Type) #gl(3, 10)
y <-  mcniacin1$nut_cont_niacin_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
niacin <- ggplot(mcniacin1, aes(x=Type, y=nut_cont_niacin_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Niacin ^bold("‡"))), x=element_blank(), y = "Niacin (mg/100 g)")#mg
  
print(niacin)

#vit_b6############
  names(mcnut)
mcvit_b6<-mcnut[,grep("vit_b6", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcvit_b6$ID<-MC_Results$ID
names(mcvit_b6)
mcvit_b6<-mcvit_b6[,c(6,1:5)]
names(mcvit_b6) 
mcvit_b61 <- reshape(mcvit_b6, direction = "long", idvar="ID", 
                     varying=2:ncol(mcvit_b6), sep = "mg_", timevar="Type")
names(mcvit_b61)#"scenario"       "ID"           "Type"           "nut_cont_vit_b6_"
mcvit_b61$Type <- mapvalues(mcvit_b61$Type, 
                            from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                            to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcvit_b61$Type) #gl(3, 10)
y <-  mcvit_b61$nut_cont_vit_b6_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
vit_b6 <- ggplot(mcvit_b61, aes(x=Type, y=nut_cont_vit_b6_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold("Vitamin B6" ^bold("‡"))), x=element_blank(), y = "Vitamin B6 (mg/100 g)")#mg
  
print(vit_b6)

#folate_total############
  names(mcnut)
mcfolate_total<-mcnut[,grep("folate_total", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcfolate_total$ID<-MC_Results$ID
names(mcfolate_total)
mcfolate_total<-mcfolate_total[,c(6,1:5)]
names(mcfolate_total) 
mcfolate_total1 <- reshape(mcfolate_total, direction = "long", idvar="ID", 
                     varying=2:ncol(mcfolate_total), sep = "mcg_", timevar="Type")
names(mcfolate_total1)#"scenario"       "ID"           "Type"           "nut_cont_folate_total_"
mcfolate_total1$Type <- mapvalues(mcfolate_total1$Type, 
                            from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                            to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcfolate_total1$Type) #gl(3, 10)
y <-  mcfolate_total1$nut_cont_folate_total_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
folate_total <- ggplot(mcfolate_total1, aes(x=Type, y=nut_cont_folate_total_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(1,1), legend.position=c(1,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold(Folate ^bold("‡"))), x=element_blank(), y = "Total folate (mcg/100 g)")#mcg
  
print(folate_total)

#vit_a_rae############
names(mcnut)

  mcvit_a_rae<-mcnut[,grep("vit_a_rae", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcvit_a_rae$ID<-MC_Results$ID
names(mcvit_a_rae)
mcvit_a_rae<-mcvit_a_rae[,c(6,1:5)]
names(mcvit_a_rae) 
mcvit_a_rae1 <- reshape(mcvit_a_rae, direction = "long", idvar="ID", 
                           varying=2:ncol(mcvit_a_rae), sep = "mcg_", timevar="Type")
names(mcvit_a_rae1)#"scenario"       "ID"           "Type"           "nut_cont_vit_a_rae_"
mcvit_a_rae1$Type <- mapvalues(mcvit_a_rae1$Type, 
                                  from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                                  to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcvit_a_rae1$Type) #gl(3, 10)
y <-  mcvit_a_rae1$nut_cont_vit_a_rae_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
vit_a_rae <- ggplot(mcvit_a_rae1, aes(x=Type, y=nut_cont_vit_a_rae_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(0,1), legend.position=c(0,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  #labs(title = expression(bold(Fat ^bold("†"))), x=element_blank(), y = "Total lipid/Fat (g)")
labs(title = expression(bold("Vitamin A" ^bold("‡"))), x=element_blank(), y = "Vitamin A (mcg RAE/100 g)")#mcg
 
print(vit_a_rae)

#beta_carot==========
names(mcnut)
  mcbeta_carot<-mcnut[,grep("beta_carot", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcbeta_carot$ID<-MC_Results$ID
names(mcbeta_carot)
mcbeta_carot<-mcbeta_carot[,c(6,1:5)]
names(mcbeta_carot) 
mcbeta_carot1 <- reshape(mcbeta_carot, direction = "long", idvar="ID", 
                        varying=2:ncol(mcbeta_carot), sep = "mcg_", timevar="Type")
names(mcbeta_carot1)#"scenario"       "ID"           "Type"           "nut_cont_beta_carot_"
mcbeta_carot1$Type <- mapvalues(mcbeta_carot1$Type, 
                               from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                               to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcbeta_carot1$Type) #gl(3, 10)
y <-  (mcbeta_carot1$nut_cont_beta_carot_/1000)/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = T, conf.level = 0.95)
beta_carot <- ggplot(mcbeta_carot1, aes(x=Type, y=(nut_cont_beta_carot_/1000)/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(0,1), legend.position=c(0,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold("Beta carotene" ^bold("‡"))), x=element_blank(), y = "Beta carotene (mg/100 g)")#mcg
  
print(beta_carot)

#protein############
names(mcnut)
 mcprotein<-mcnut[,grep("protein", colnames(mcnut)), ]#<-mcnutrients[,85:96]
#mczinc$scenario <- MC_Results$farmer_scenario
mcprotein$ID<-MC_Results$ID
names(mcprotein)
mcprotein<-mcprotein[,c(6,1:5)]
names(mcprotein) 
mcprotein1 <- reshape(mcprotein, direction = "long", idvar="ID", 
                         varying=2:ncol(mcprotein), sep = "g_", timevar="Type")
names(mcprotein1)#"scenario"       "ID"           "Type"           "nut_cont_protein_"
mcprotein1$Type <- mapvalues(mcprotein1$Type, 
                                from = c("FRUIT" , "PULSE" ,"SPICE" , 'STAPLE', "VEG"), 
                                to = c("Fruit", "Pulse","Spice","Staple","Vegetable"))
lev <- as.factor(mcprotein1$Type) #gl(3, 10)
y <-  mcprotein1$nut_cont_protein_/9071.85 #c(rnorm(10), rnorm(10) + 0.1, rnorm(10) + 3)
d <- data.frame(lev=lev, y=y)
a <- aov(y~lev, data=d)
tHSD <- TukeyHSD(a, ordered = FALSE, conf.level = 0.95)
protein <- ggplot(mcprotein1, aes(x=Type, y=nut_cont_protein_/9071.85))+
  #geom_violin(aes(group = factor(Type), fill=Type)) +
  geom_boxplot(aes(group = factor(Type), fill=Type))+
  scale_fill_brewer(palette="Set1")+
  geom_text(data = generate_label_df(tHSD, 'lev'), aes(x = plot.labels, y = V1, label = labels), family='serif')+
  theme_classic(base_family= "serif")+
  theme(axis.text.y=element_text(hjust=0.5, angle=90))+
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 3))+
  #theme(legend.justification=c(0,1), legend.position=c(0,1))+#theme(legend.position="none")+
  scale_fill_discrete(name=element_blank())+ theme(axis.text.x = element_blank())+ #theme(plot.margin=unit(c(2,0,-2.5,0),"mm"))+  
  labs(title = expression(bold("Protein" ^bold("†"))), x=element_blank(), y = "Protein (g/100 g)")#g
print(protein)

###Plot all together=========
library(grid)
library(ggplot2)
library(gridExtra)

#Normal grid.arrange
dev.off()
grid.arrange(energy, 
                         fiber, carbohydrate, lipid_total, protein, 
            beta_carot,folate_total,niacin,riboflavin,thiamin,vit_a_rae,vit_b6,vit_c, 
             calcium, iron, zinc, ncol=4)

#with shared legend
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}


#library(Cairo)
#Cairo(type = 'png', file = 'out.png', dpi=72)
grid_arrange_shared_legend(energy, 
   fiber, carbohydrate, lipid_total, protein,
   beta_carot,folate_total,niacin,riboflavin,thiamin,vit_a_rae,vit_b6,vit_c, 
   calcium, iron, zinc) #(custom R Studio PDF 10*10 inch)

#One plot for each category
#grid.newpage()#Calories (custom R Studio PDF 3*4 inch)
#grid.draw(rbind(ggplotGrob(energy), size = "last"))
#grid.newpage()#Macronutrients (custom PDF 10*4 inch)
#grid.draw(rbind(ggplotGrob(fiber), ggplotGrob(carbohydrate), ggplotGrob(lipid_total), ggplotGrob(protein), size = "last")) 
#grid.newpage()#Vitamins (custom PDF 17*4 inch)
#grid.draw(rbind(ggplotGrob(beta_carot), ggplotGrob(folate_total), ggplotGrob(niacin), ggplotGrob(riboflavin), ggplotGrob(thiamin), ggplotGrob(vit_a_rae), ggplotGrob(vit_b6), ggplotGrob(vit_c), size = "last")) 
#grid.newpage()#Micronutrients (custom PDF 7*4 inch)
#grid.draw(rbind(ggplotGrob(calcium), ggplotGrob(iron), ggplotGrob(zinc), size = "last"))


