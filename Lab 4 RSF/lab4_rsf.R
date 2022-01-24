# ******************************************************************************
# ******************************************************************************
# Lab 4: Resource selection functions
# Description: Create a resource selection function to study habitat selection patterns of mule deer
# ******************************************************************************
# ******************************************************************************

# Remove anything in memory
rm(list=ls())

# Install necessary libraries
# ONLY RUN THIS ONCE. 
install.packages("arm")
install.packages("sjPlot")

# Load libraries used in this script
library(arm)
library(sjPlot)
library(sp)
library(raster)

# Set working directory
setwd("./Lab 4 RSF")

#####################
#### Import data ####
#####################
deer.locs <- read.csv("deer_dates.csv")	#GPS locations from collared deer in the Piceance Basin (NW CO)
mcp <- read.csv("home_range.csv")  #mcp homerange polygon

deer.locs$date <- as.POSIXct(deer.locs$date)
deer.locs$id <- 'deer.1'

plot(deer.locs$X, deer.locs$Y)
lines(deer.locs$X, deer.locs$Y)

coordinates(deer.locs) <- ~X+Y
proj4string(deer.locs) <- CRS("+init=epsg:6341") #UTM zone 12N


library(adehabitatLT)


#Load environmental covariates in as rasters, using the 'raster()' function
?raster()

elevation <- raster("elevation.asc")	
wells <- raster("wells_overlap.asc")		
shrub <- raster("shrub.asc")		
barren <- raster("barren.asc")		
rds <- raster("rds.asc")			
		
# Look at deer data
str(deer.locs)
     # Notice these data don't have timestamps associated with the locations
     # What could be a problem with conducting an analysis on these data? What are some assumptions about these data we are making by proceeding with our analyses?
     # (hint: think about some issues we discussed in the context of constructing home ranges last week)

# Convert deer locations to spatial format
deer <- SpatialPoints(deer.locs)
deer
     ## Question: What is missing from these data?

elevation
     ## Notice this is also missing in this raster. 
     ## Check other rasters -- do any have crs specified?

# .................
# Note: in this case, I'm not 100% sure I'm selecting/defining the correct projection system, because I don't have the metadata (data about the data). For the purposes of the lab, my best guess will be good enough, because I am confident all data are in the same crs, whatever it is. However, I wouldn't recommend using these datasets/rasters in other contexts.
# Looking at the deer locs, the extent looks like meters, and I know the data should be NW Colorado - my best guess is UTM
# I used https://epsg.io and google to determine this is likely in UTM and zone 12N (https://epsg.io/6341)
# .................

# We need to add coordinate reference system (CRS) info for all data

# Define projection: 
proj4string(deer) <- CRS("+init=epsg:6341") #UTM zone 12N
deer

# check that our rasters all overlap in resolution/extent
compareRaster(barren, elevation, rds, shrub, wells)

# They do, so we can stack them so that we can do the same processes to all at once
enviro <- stack(elevation, barren, shrub, wells, rds)
enviro #notice 'nlayers' dimension, imagine all the layers stacked up like a stack of papers

# can still access individual rasters:
enviro[[1]] 
# -or- 
enviro$elevation

# define crs for rasters
proj4string(enviro) <- CRS("+init=epsg:6341") 
enviro

# .................
# About the warning you may be getting: 'spatial R' has recently updated how it handles projection systems,
# and as of the past ~6 months there's a new 'best practice' for handling crs. 
# Here's more info and a bunch of other resources if you want to dig in: https://swampthingecology.org/blog/changes-to-crs-in-r/
# But, the method above still works for now, although this could change at some point. I haven't fully grasped how to update to the new method, which is why we're sticking with this for now. 
# .................

#remember, we've assigned a crs to the layers *within the stack* not the individual layers:
elevation

# to be safe, I'm going to remove the unstacked layers
rm(barren, elevation, rds, shrub, wells)

##################################
##### Plot and examine data ######
##################################
plot(enviro)

# Plot elevation 
plot(enviro$elevation)
points(deer, col="blue", pch=16, cex=0.5) # Add deer locations

# Visualize wells (number of wells within 800 m)
# wells raster was created by creating 800m buffer around wells, and calculating how many buffers overlap
plot(enviro$wells_overlap); points(deer, col="blue", pch=16, cex=0.5)

table(values(enviro$wells_overlap)) #number of grid cells with each num. of wells

# Plot shrubs
plot(enviro$shrub); points(deer, col="blue", pch=16, cex=0.5)
hist(values(enviro$shrub))
table(values(enviro$shrub))

# Plot barren
plot(enviro$barren); points(deer, col="blue", pch=16, cex=0.5)
hist(values(enviro$barren))
table(values(enviro$shrub))

# Plot distance to roads
plot(enviro$rds); points(deer, col="blue", pch=16, cex=0.5)
hist(values(enviro$rds)) # there should not be negative distances

values(enviro$rds)[values(enviro$rds)<0] <- 0 #change negative vals to 0
hist(values(enviro$rds)) 

     # Question: After visualizing the data, do you have any expectations for possible patterns that analysis may reveal?


##################################
###### Extract spatial data ######
##################################
# before extracting, standardize continuous spatial covariates before including them in analysis
values(enviro$elevation) <- scale(values(enviro$elevation)) 
values(enviro$rds) <- scale(values(enviro$rds)) 
summary(enviro_std$elevation)
summary(enviro_std$rds)

# create matrix to store spatial data
used <- matrix(1, nrow=nrow(deer.locs), ncol=6)

# Intersect deer locations with habitat layers and store in 'empty dataset'used
used[,2:6] <- extract(enviro, deer) # can extract across all layers at once 
used <- as.data.frame(used)

names(enviro) #check to make sure to assign names in correct order next:
names(used) <- c("Used","Elevation","Barren", "Shrub", "Wells", "Roads")

# Look at data 
head(used)

##### Create random data ##### 
# Consider the 'available' landscape to be the area within MCP home range 
     # Review question: what is an MCP?

# Plot mcp and deer locations
par(mfrow=c(1,1))
plot(enviro$elevation)
points(deer, col="blue", pch=16, cex=0.5)
lines(mcp, lwd=2)

set.seed(592)
random <- sp::spsample(Polygon(mcp), 6000, "random")

# add random points to plot
points(random, cex=0.4)

# Create empty dataset & fill with habitat data at the random locations
avail <- matrix(0, nrow=6000, ncol=6)
avail[,2:6] <- extract(enviro, random)

avail<- as.data.frame(avail)
names(avail) <- c("Used","Elevation","Barren", "Shrub", "Wells", "Roads")

head(avail)

# Combine used and random data
all.data <- rbind(used, avail)

# inspect 
head(all.data)
     # Question: what does the first column ('Used') mean?

# Compare covariates at used versus available points 
boxplot(Elevation ~ Used, data = all.data)
boxplot(Wells ~ Used, data = all.data)
boxplot(Roads ~ Used, data = all.data)

table(all.data$Shrub, all.data$Used)
table(all.data$Barren, all.data$Used)
     ## Question: any expectations for how these variables may be related to deer habitat selection?

# look at relationships between predictors - any concerns?
cor(all.data[,2:6])


########################
##### Analyze data #####
########################
# RSF equation: w(x) = exp(x1*beta1 + x2*beta2 + ... + x_n*beta_n)
     # Notice: RSF equations usually do not include intercepts
     # Question: what does a positive beta value indicate? negative?

## Model 1: elevation, shrub, barren ##
model1 <- glm(Used ~ Elevation + Shrub + Barren, family=binomial(link="logit"), data = all.data)

summary(model1)
plot_model(model1)
     # How do deer respond to each of these factors?


## Model 2: wells and roads ##
model2 <- glm(Used ~ Wells + Roads, family=binomial(link="logit"), data = all.data)
summary(model2)
plot_model(model2)
     # How do deer respond to each of these factors?


## Run model 3: all variables ##
model3 <- glm(Used ~  Elevation + Wells + Shrub + Barren + Roads, family=binomial(link="logit"), data=all.data)
summary(model3)
plot_model(model3)
     # How do deer respond to each of these factors?
     # Why might elevation have become 'significant' in this model, where it wasn't in model 1? is this a concern?

## Any other models you think may be biologically important? Try running one:
#.... put additonal model code here ....#


## determine best model using AIC ##
AIC(model1, model2, model3)
     # Which model has most support? 

# .....................................
##### Coefficient interpretations #####
summary(model3)

# 1. What type of variable is each covariate (continuous or categorical)?

# 2. For continuous variables, exponentiating coefficients gives relative odds of use between two locations that differ by 1 standard deviation (for scaled variables) of the variable in question, with all other factors identical and assuming the two locations are equally available

exp(0.21) 
# a 1 std. dev increase in distance from roads is associated with a location being 20% more likely (or 1.2 times more likely) to be used, all else being equal.


# 3. For categorical variables, exponentiating coefficients gives

exp(-0.42)
# use:availability ratio for barren (=1) is 0.67 x use:availability ratio for not barren (0). 


# Challenge: try accounting for relative availability of shrub and barren in interpretation of coefficients (as described by Fieberg et al.). How does this change results?

# .....................................


##### Visualize RSF as surface ##### 
# Can only predict over spatial region represented in data, i.e., within mcp

# Subset rasters to mcp:
ext <- extent(range(mcp[,1]), range(mcp[,2])) #use the mins/maxes of the mcp to crop 
ext

enviro_crop <- crop(enviro, ext)

# compare extents of enviro and enviro_crop
extent(enviro)
extent(enviro_crop)

# plot a cropped raster
plot(enviro_crop$elevation)
points(deer, col="blue", pch=16, cex=0.5)
lines(mcp, lwd=2)

# create dataframe of all grid cells
pred.data <- cbind.data.frame(Elevation=values(enviro_crop$elevation),
                              Barren=values(enviro_crop$barren), 
                              Shrub=values(enviro_crop$shrub),
                              Wells=values(enviro_crop$wells_overlap), 
                              Roads=values(enviro_crop$rds))
head(pred.data)

# use predict() to predict from model
pred.surf <- enviro_crop$elevation #use existing raster to create 'framework' to fill in w/ predicted values

values(pred.surf) <- predict(model3, newdata = pred.data)
plot(pred.surf) #values correspond to *log odds* 

#then, add used locations
points(deer, pch=16, cex=0.5)
lines(mcp, col="blue", lwd=2)
     # What figure caption would you use with this figure to summarize your results? What does the legend correspond to?
    

# Calculate/plot RSF by hand
summary(model3)

rsf <- exp(enviro_crop$wells_overlap*-0.55575 + -0.41430*enviro_crop$barren + enviro_crop$rds*0.16808)
     # RSF values correspond to *relative* probabilities of use

plot(rsf)
points(deer, pch=16, cex=0.5)
lines(mcp, col="blue", lwd=2)
     # How would you summarize these results? What does the legend correspond to?


##### Explore how number of available points influences inference #####
# Fieberg et al. (2021) suggest increasing number of available points until parameter estimates don't change much as num. avail points change. Let's try to sort out how many available points we should use, using model3
num_avail <- c(1, 5, 10, 50, 100) #proportions relative to number of used points
ests <- matrix(0, nrow=5, ncol=6)

for(i in 1:5){
     rand <- spsample(Polygon(mcp), num_avail[i]*525, "random")
     
     # create empty dataset & fill with habitat data at the random locations
     avail <- matrix(0, nrow=num_avail[i]*525, ncol=6)
     avail[,2:6] <- extract(enviro, rand)
     
     avail <- as.data.frame(avail)
     names(avail) <- c("Used","Elevation","Barren", "Shrub", "Wells", "Roads")
     
     # combine used and random data
     all.data <- rbind(used, avail)
     
     # fit model
     mod <- glm(Used ~  Elevation + Wells + Shrub + Barren + Roads, family=binomial(link="logit"), data=all.data)
     ests[i,] <- summary(mod)$coefficients[,1]
     
}

out <- cbind.data.frame(num_avail, ests)
names(out) <- c("num_avail", "int", "b1", "b2", "b3", "b4", "b5")

plot(b1~num_avail, data=out, ylim=c(-1,1))
lines(b1~num_avail, data=out)
points(b2~num_avail, data=out, col="blue")
lines(b2~num_avail, data=out, col="blue")
points(b3~num_avail, data=out, col="purple")
lines(b3~num_avail, data=out, col="purple")
points(b4~num_avail, data=out, col="green")
lines(b4~num_avail, data=out, col="green")
points(b5~num_avail, data=out, col="orange")
lines(b5~num_avail, data=out, col="orange")

     # Looks like anything greater than/equal to 525 * 10 = 5250 available points is adequate


# .................
# Final notes:
# Fieberg et al. (2021) paper is *very* useful, as is the example code in the supplement: https://doi.org/10.1111/1365-2656.13441
# These are also the authors behind R package amt, which can be a useful tool for RSFs or SSFs
# Another great RSF tutorial available here: 
#    https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/6_RSF_SSF.html
# .................
