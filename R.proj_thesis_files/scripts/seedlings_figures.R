#Attempt to make figures for Results section
library(tidyverse)

# Figure of recruits~elevation as a bar graph -----------------------------

transectdata <-read.csv("R.proj_thesis_files/data/seedlings.csv")
climdata <-read.csv("R.proj_thesis_files/data/microclim.csv")
#This will make a table that associates elevations with their subsequent site code
elevtable <- select(climdata,site,elevation)%>%
  distinct()
#This will extract the total recruit data and assign it to an elevation based on site code being the common variable. tdata_elev2 is a table that lumps elevations, species, site, counts, plots and site type in one table.

tdata_elev2 <- inner_join(elevtable,transectdata)%>%
  separate(site,into="site_type",sep="-",remove=FALSE,extra="drop")

#This is my attempt at making a figure of counts~elevation as a bar graph.

ggplot(tdata_elev2,aes(x=elevation,y=count.1, colour=site_type))+
  geom_bar(stat="identity", width=15)+
  labs( 
       x="Elevation (m)", y = "Number of Recruits")+
  theme_classic()+
  geom_errorbar()

#This only graphed counts as a function of elevation for ALL species, not exacty what I need.

#I need to separate burn transect data and unburn transect data, then I need to graph their counts as a function of elevation in a bar graph. 

burndata <- select(tdata_elev2,site_type,count.1,elevation)%>%
  filter(site_type=="RPN")

nonburndata <- select(tdata_elev2,site_type,count.1,elevation)%>%
  filter(site_type=="RP")

ggplot(burndata,aes(x=elevation,y=count.1, colour=site_type))+
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()


# Figure of recruits~elevation as a regression line -----------------------


