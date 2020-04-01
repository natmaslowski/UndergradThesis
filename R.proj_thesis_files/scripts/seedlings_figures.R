# Linear models and figures for Results section

# Load libraries
library(tidyverse)
library(broom)
library(visreg)
library(lme4)

# Load datasets
transectdata <-read.csv("R.proj_thesis_files/data/seedlings.csv")
climdata <-read.csv("R.proj_thesis_files/data/microclim.csv")

# This will make a table that associates elevations with their subsequent site code
elevtable <- select(climdata,site,elevation)%>%
  distinct()

# This will extract the total recruit data and assign it to an elevation based on site code being the common variable. tdata_elev2 is a table that lumps elevations, species, site, counts, plots and site type in one table.
# This is the complete raw data with a data line for each species in each plot
tdata_elev2 <- inner_join(elevtable,transectdata)%>%
  separate(site,into="site_type",sep="-",remove=FALSE,extra="drop")

# Now, collapse the data to one row per plot regardless of species
plot.data <- tdata_elev2 %>%
  group_by(site_type, elevation, site, plot) %>%
  summarise(total.recruits = sum(count.1),
            total.recruits.2 = sum(count.2))

# Get a count of species richness for plots with any recruits
rich.data.with <- tdata_elev2 %>% 
  filter(count.1>0) %>% 
  group_by(site_type, elevation, site, plot) %>%
  summarise(richness = n_distinct(species))

# Join the richness column onto the plot-level data and fill in NAs with 0s for plots with no recruits
plot.data <- left_join(plot.data, rich.data.with)
plot.data[is.na(plot.data)] = 0 


# Result 1: recruits~site type as a categorical variable, pooling across elevation-----------------------------

# graphing raw data (show as figure panel)
ggplot(plot.data, aes(x=site_type, y=total.recruits))+
  geom_boxplot()+
  labs( 
       x="Site type", y = "Number of Recruits")+
  theme_classic()

# statistical model (report in text or table)
mod1 <- glm(total.recruits ~ site_type, family="poisson", data=plot.data)
visreg(mod1, scale="response")

tidy(mod1)
summary(mod1)


# Result 2: recruits~elevation by site type as two regression lines -----------------------

# again, graph raw data
ggplot(plot.data, aes(x=elevation, y=total.recruits)) +
  geom_point(aes(color=factor(site_type))) +  
  labs(x="Elevation", y = "Number of Recruits") +
  theme_classic()

# this model won't run, too complicated for dataset - don't use
lrmodA <- glmer(total.recruits ~ poly(elevation,2)*site_type + (1|site), data=plot.data, family=poisson)

# simplify A by dropping random effect
# this model is still overfit - don't use
lrmodB <- glm(total.recruits ~ poly(elevation,2)*site_type, data = plot.data, family=poisson)
summary(lrmodB)
visreg(lrmodB, xvar="elevation", by="site_type")#, scale="response")

#simplify B by getting rid of quadratic term - use this one
lrmodC <- glm(total.recruits ~ elevation*site_type, data = plot.data, family=poisson)
summary(lrmodC)
visreg(lrmodC, xvar="elevation", by="site_type", scale="response")


# Result 3: richness~site type as a categorical variable, pooling across elevations -------------------------

# graph raw data
ggplot( plot.data, aes(x=site_type, y=richness))+
  geom_boxplot()+
  labs( 
    x="Site Type", y = "Species Richness")+
  theme_classic()

# statistical model (report in text or table)
mod2 <- glm(richness ~ site_type, family="poisson", data=plot.data)
visreg(mod2, scale="response")

summary(mod2)
tidy(mod2)


# Result 4: richness~elevation by site type as a linear regression ---------------------

# graph raw data
ggplot(plot.data, aes(x=elevation, y=richness)) +
  geom_point(aes(color=factor(site_type))) +  
  labs(x="Elevation", y = "Species richness of recruits") +
  theme_classic()

# this model is overfit - don't use
lrmodD <- glm(richness ~ poly(elevation,2)*site_type, data=plot.data, family=poisson)
summary(lrmodD)
visreg(lrmodD, xvar="elevation", by="site_type")#, scale="response")

#simplify by dropping quadratic term - use this
lrmodE <- glm(richness ~ elevation*site_type, data=plot.data, family=poisson)
summary(lrmodE)
visreg(lrmodE, xvar="elevation", by="site_type", scale="response")
