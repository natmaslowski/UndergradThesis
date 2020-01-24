
# Attempt 1 to make transect data graph -------------------------------

# Script for making a graph using Transect Data (field data)
#Installing of packages I might need?
installed.packages("tidyverse")

library(tidyverse)
library(ggplot2)

install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")


#here I have loaded the column data I want to use to make my graph
transectdata <-read.csv("./data/seedlings.csv")
climdata <-read.csv("./data/microclim.csv")

#I don't exactly know how to merge two data sets with different lengths from two different excel files. So here I am trying to use them by assigning them names and then manually merging them?
elevation <- c(climdata$elevation)
count1 <- c(transectdata$count.1)

elevation.count1 <- c(elevation, count1)

#Should I combine the data using cband function instead? I don't know exactly how, so I shall consult google.

#I am going to try something else instead
elev.site <- c(elevation,climdata$site)
count.site <- c(transectdata$site,counts <- c(transectdata$count.1,transectdata$count.2))


#Ok so I don't know how to account for the difference in lengths for each column?

#Well I realized that I what I tried above isn't working. So I shall keep pressing buttons until the graph I want magically appears.

ggplot(aes(x = elevation, y = count1, color = voidtype)) + geom_point() + geom_path() +
  labs(x = "Elevation (m)", y = "Presence", color = "Void type")

#How do I make a graph that has a y-axis that only counts presence, and not the number of species at that site?



# Attempt at Stats take 1 -------------------------------------------------

#I am going to try to seperate burn data from unburn data, so I can then compare the two.
library("tidyverse")

early <- c(transectdata$site,transectdata$count.1)
late <- c(transectdata$site,transectdata$count.2)

#not sure how to seperate the data (burn and unburn)

#A quick and dirty attempt to compare my early and late season census to see if there was a significant difference. Apparently there was, because p-value is 0.01798?
t.test(transectdata$count.1,transectdata$count.2,paired=TRUE)

