
# Attempt 1 to make transect data graph -------------------------------

# Script for making a graph using Transect Data (field data)
#Installing of packages I might need?

library(tidyverse)

transectdata <-read.csv("./data/seedlings.csv")
climdata <-read.csv("./data/microclim.csv")

#Attempt 2 at extracting data from two different files based on a common variable with the help of Andrew. Using dplyr select to take the two columns (select) and (distinct). Then take clim data and join with seedling data?








# Attempt at Stats take 1 -------------------------------------------------

#I am going to try to seperate burn data from unburn data, so I can then compare the two.


early <- c(transectdata$site,transectdata$count.1)
late <- c(transectdata$site,transectdata$count.2)

#not sure how to seperate the data (burn and unburn)

#A quick and dirty attempt to compare my early and late season census to see if there was a significant difference. Apparently there was, because p-value is 0.01798?
t.test(transectdata$count.1,transectdata$count.2,paired=TRUE)

