#Attempt to make figures for Results section
library(tidyverse)
library(broom)

# Figure of recruits~elevation as a bar graph -----------------------------

transectdata <-read.csv("R.proj_thesis_files/data/seedlings.csv")
climdata <-read.csv("R.proj_thesis_files/data/microclim.csv")
#This will make a table that associates elevations with their subsequent site code
elevtable <- select(climdata,site,elevation)%>%
  distinct()
#This will extract the total recruit data and assign it to an elevation based on site code being the common variable. tdata_elev2 is a table that lumps elevations, species, site, counts, plots and site type in one table.

tdata_elev2 <- inner_join(elevtable,transectdata)%>%
  separate(site,into="site_type",sep="-",remove=FALSE,extra="drop")

# graphing raw data (show as figure panel)
ggplot(tdata_elev2,aes(x=site_type,y=count.1))+
  geom_boxplot()+
  labs( 
       x="Site type", y = "Number of Recruits")+
  theme_classic()

# statistical model (report in text or table)
mod1 <- glm(count.1 ~ site_type, family="poisson", data=tdata_elev2)
visreg(mod1)

tidy(mod1)
summary(mod1)

# Figure of recruits~elevation as a regression line -----------------------
library(visreg)
library(lme4)

# again, graph raw data
ggplot(tdata_elev2, aes(x=elevation,y=count.1)) +
  geom_point(aes(color=factor(site_type))) +  
  labs(x="Elevation", y = "Number of Recruits") +
  theme_classic()

lrmodA <- glmer(count.1 ~ poly(elevation,2)*site_type + (1|site), data=tdata_elev2, family=poisson)
#simplify A by dropping random effect
lrmodB <- glm(count.1 ~ poly(elevation,2)*site_type, data = tdata_elev2, family=poisson)

summary(lrmodB)
visreg(lrmodB, xvar="elevation", by="site_type")


# Figure of richness~elevation as a linear regression ---------------------

#This is creating a table of data that includes richness.
rich.data_lr <- inner_join(elevtable,transectdata)%>%
  separate(site,into="site_type",sep="-",remove=FALSE,extra="drop")%>%
  filter(count.1 !=0)%>%
  group_by(site_type,plot,elevation)%>%
  distinct(count.1)


lrmodD <- glm(count.1 ~ poly(elevation,2)*site_type, data=rich.data_lr, family=poisson)
summary(lrmodD)
visreg(lrmodD, xvar="elevation", by="site_type", scale="response")

# Figure of richness~elevation as a boxplot graph -------------------------
library(dplyr)

rich.data <-tdata_elev2%>%
  separate(site,into="site_type",sep="-",remove=FALSE,extra="drop")%>%
  filter(count.1 !=0)%>%
  group_by(site_type,site,plot)%>%
  distinct(species)

#boxplot modelling richness as a function of the site type.

ggplot( rich.data,aes(x=site_type,y=count.1))+
  geom_boxplot()+
  labs( 
    x="Site Type", y = "Species Richness")+
  theme_classic()

# statistical model (report in text or table)
mod2 <- glm(count.1 ~ site_type, family="poisson", data=rich.data)
visreg(mod2)

summary(mod2)
tidy(mod2)
