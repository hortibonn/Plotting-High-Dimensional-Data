#Radial plot

#Radial plots ###
library(ggplot2)
library(tidyverse) #tidyverse includes a number of useful packages

# function to compute standard error of mean
se <- function(x) sqrt(var(x)/length(x)) 
set.seed(9876) 

#radial with a factor variable####

DF <- data.frame(variable = as.factor(1:10),
                 value = sample(10, replace = TRUE))

ggplot(DF, aes(variable, value, fill = variable)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = value - se(DF$value), 
                    ymax = value + se(DF$value), 
                    color = variable), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(DF$variable)) +
  theme_minimal() +
  coord_polar()

#radial with a data set and grid with three factor variables####

DF2 <- data.frame(name = rep(letters[1:3], length.out = 30),
                 variable = as.factor(1:5),
                 variable2 = rep(letters[4:7], length.out = 30),
                 value = sample(10, replace = TRUE))

multi_plot <- ggplot(DF2, aes(variable, value, fill = variable)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = value - se(DF2$value), 
                    ymax = value + se(DF2$value), 
                    color = variable), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(DF2$variable)) +
  theme_minimal() +
  coord_polar()

# Rows are name and columns are variable2
multi_plot + facet_grid(name ~ variable2)

#radial with a data set and grid with four factor variables and one continuous####
library(plyr)
DF3 <- data.frame(name = rep(letters[1:3], length.out = 600),
                  variable = as.factor(sample(5, replace = TRUE)),
                  variable2 = rep(letters[4:7], length.out = 600),
                  variable3 = rep(letters[8:16], length.out = 600),
                  value = sample(50, replace = TRUE))

multi_plot <- ggplot(data = DF3, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  scale_y_continuous(breaks = 0:nlevels(DF3$variable)) +
  theme_minimal() +
  coord_polar()

# Rows are name and columns are variable2
multi_plot + facet_grid(name ~ variable2)


#many continuous variables are possible####

multi_plot <- ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = Sepal.Length - se(iris$Sepal.Length), 
                    ymax = Sepal.Length + se(iris$Sepal.Length), 
                    color = Species), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(iris$Species)) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  coord_polar()

# Split in vertical direction
multi_plot + facet_grid(Species ~ .)
# Split in horizontal direction
multi_plot + facet_grid(. ~ Species)

# Facet by two variables: dose and supp.
# Rows are dose and columns are supp
bp + facet_grid(dose ~ supp)
# Facet by two variables: reverse the order of the 2 variables
# Rows are supp and columns are dose
bp + facet_grid(supp ~ dose)

bp + facet_grid(dose ~ supp, margins=TRUE)

#coord_polar options#####

# A coxcomb plot = bar chart + polar coordinates
cxc <- ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = 1, colour = "black")
cxc + coord_polar()
# A new type of plot?
cxc + coord_polar(theta = "y")

# The bullseye chart
pie + coord_polar()

library(ggplot2movies)

# Windrose + doughnut plot
if (require("ggplot2movies")) {
  movies$rrating <- cut_interval(movies$rating, length = 1)
  movies$budgetq <- cut_number(movies$budget, 4)
  
  doh <- ggplot(movies, aes(x = rrating, fill = budgetq))
  
  # Wind rose
  doh + geom_bar(width = 1) + coord_polar()
  # Race track plot
  doh + geom_bar(width = 0.9, position = "fill") + coord_polar(theta = "y")
}

#Grid radial simple tooth ####

library(ggplot2)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
df <- ToothGrowth
head(df)

#barplot
bp <- ggplot(df, aes(x=supp, group=dose)) + 
  geom_bar(aes(fill=dose)) +
  theme_minimal()+
  coord_polar()
bp

#boxplot

bp <- ggplot(df, aes(x=dose, y=len, group=dose)) + 
  geom_boxplot(aes(fill=dose)) +
  theme_minimal()+
  coord_polar()
bp

# Split in vertical direction
bp + facet_grid(supp ~ .)
# Split in horizontal direction
bp + facet_grid(. ~ supp)

#some wierd sunburst-style barplot confined to a circle

# make some fake data
df <- data.frame(
  'level1'=c('a', 'a', 'a', 'a', 'b', 'b', 'c', 'c', 'c'), 
  'level2'=c('a1', 'a2', 'a3', 'a4', 'b1', 'b2', 'c1', 'c2', 'c3'), 
  'value'=c(.025, .05, .027, .005, .012, .014, .1, .03, .18))

# sunburst plot
p <- ggplot(df, aes(y=value)) +
  geom_bar(aes(fill=level1, x=0), width=.5, stat='identity') + 
  geom_bar(aes(fill=level2, x=.25), width=.25, stat='identity') + 
  coord_polar(theta='y')
p <- p + theme_void() 



