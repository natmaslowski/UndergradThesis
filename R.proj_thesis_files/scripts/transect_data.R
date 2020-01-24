
# Attempt 1 to make transect data graph -------------------------------

library(tidyverse)

# for natalie to read in files
transectdata <-read.csv("./data/seedlings.csv")
climdata <-read.csv("./data/microclim.csv")

# for amy to read in files
transectdata <-read.csv("R.proj_thesis_files/data/seedlings.csv")
climdata <-read.csv("R.proj_thesis_files/data/microclim.csv")

#Attempt 2 at extracting data from two different files based on a common variable with the help of Andrew. Using dplyr select to take the two columns (select) and (distinct). Then take clim data and join with seedling data?

elevtable <- select(climdata,site,elevation)%>%
  distinct()

#This will distinguish burn and unburn data, as well as convert counts to presences.

tdata_elev <- inner_join(elevtable,transectdata)%>%
  separate(site,into="site_type",sep="-",remove=FALSE,extra="drop")%>%
  mutate("pres.1"=as.numeric(count.1!=0),"pres.2"=as.numeric(count.2!=0))

  

ggplot(tdata_elev,aes(x=elevation,y=pres.1, colour=site_type))+
  facet_wrap(~species)+
  geom_point()


# Attempt at Stats take 1 -------------------------------------------------


early <- c(transectdata$site,transectdata$count.1)
late <- c(transectdata$site,transectdata$count.2)

#not sure how to seperate the data (burn and unburn)

#A quick and dirty attempt to compare my early and late season census to see if there was a significant difference. Apparently there was, because p-value is 0.01798?
t.test(transectdata$count.1,transectdata$count.2,paired=TRUE)

