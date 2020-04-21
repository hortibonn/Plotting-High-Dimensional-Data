###' I used parts of the "high dimensional data" repositories code to create a figure
#' The code you find below is helpful whenever you have two parameters of the same kind (here two metabolites in plant leaves) and a rather large number of cases you want to plot those parameters for.
#' In the specific case here, we also have two levels of cases (1. Leaf number 2. Treatment).
#' The fact that leaf 9 is located above leaf 8 not only in the figure, but also in reality, makes the figure easy to understand, but this is not a necessary feature of the data.

library(ggplot2)
library(tidyverse)

#' Generate data
Treatment <- c(rep(c("Treatment A", "Treatment B", "Treatment C"), times = 18))
Leaf_position <- rep(c(rep("Leaf 1", 3), rep("Leaf 2", 3), rep("Leaf 3", 3), rep("Leaf 4", 3),
                       rep("Leaf 5", 3), rep("Leaf 6", 3), rep("Leaf 7", 3), rep("Leaf 8", 3),
                       rep("Leaf 9", 3)), 2)
Amount <- (rnorm(54, mean = 4, sd = 1.5))
Metabolite <- c(rep("Metabolite A", times = 27), rep("Metabolite B", times = 27))

data <- cbind.data.frame(Treatment, Leaf_position, Amount, Metabolite)
rm(Treatment) #' We remove this, as it would cause trouble in creating the ggplots

#' Subset the data by the factor you want to show in different parts of the figure
Metabolite_A <- subset.data.frame(data, data$Metabolite=="Metabolite A")
Metabolite_A$`Metabolite A [mg per g]` <- Metabolite_A$Amount
Metabolite_B <- subset.data.frame(data, data$Metabolite=="Metabolite B")
Metabolite_B$`Metabolite B [mg per g]` <- Metabolite_B$Amount



#' Metabolite_A plot (left)
Left_plot <- ggplot(Metabolite_A, aes(x = Leaf_position, y = `Metabolite A [mg per g]`)) +
  geom_bar(aes(fill = Treatment), stat ="identity", position = position_dodge()) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 10)) +
  scale_y_reverse() +
  coord_flip()

Left_plot

#' Metabolite_B plot (right)

Right_plot <- ggplot(Metabolite_B, aes(x = Leaf_position, y = `Metabolite B [mg per g]`)) +
  geom_bar(aes(fill = Treatment), stat ="identity", position = position_dodge()) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 10)) +
  theme(legend.position = c(0.8, 0.79)) +
  scale_y_continuous(limits = c(0,7.5)) +
  coord_flip()

Right_plot

#' Center
Center_text <- c("Leaf 1", "Leaf 2", "Leaf 3", "Leaf 4", "Leaf 5", 
                 "Leaf 6", "Leaf 7", "Leaf 8", "Leaf 9")
Center_text <- as.data.frame(Center_text)


Center <- ggplot(Center_text,aes(x = 1,y=Center_text)) + 
  geom_text(aes(label = Center_text)) +
  geom_segment(aes(x=0,xend=0,yend=Center_text))+
  geom_segment(aes(x=0,xend=0,yend=Center_text))+
  ggtitle("")+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(1.0,1.0))+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(size=25, color=NA),
        axis.ticks.x=element_line(size=25, color=NA),
        plot.margin = unit(c(1,0,1,0), "mm"))

Center


#'Putting em all together in one figure 
final_figure <- cowplot::plot_grid(Left_plot, Center, Right_plot, 
                                   ncol = 3, align = "h",
                                   rel_widths = c(4,1,4)) #'rel_width modifies the relative width of the (in this case) three plots

final_figure

#' A next cool step to improve this would be to add an image of the type of plant we are looking at here, and put it in the center of the figure. 
