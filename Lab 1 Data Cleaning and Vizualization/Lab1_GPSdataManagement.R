# ***************************************************************************************************************************************
# ***************************************************************************************************************************************

# Lab: Managing and cleaning GPS data 
# Description: Script to read, visualize, summarize, and animate GPS data

# ***************************************************************************************************************************************
# ***************************************************************************************************************************************

# Remove anything in memory
rm(list=ls())

# Install necessary libraries (libraries provide the functions that we will use to analyze data)
# ONLY RUN THIS ONCE. CLICK 'NO' WHEN ASKED TO RESTART R.
install.packages("proj4")
install.packages("maptools")
install.packages("rgdal")
install.packages("lubridate")
install.packages("raster")
install.packages("sp")
install.packages("sf")
install.packages("mapview")
install.packages("tidyverse")
install.packages("doParallel")
install.packages("foreach")
install.packages("move")
install.packages("moveVis")
install.packages("anipaths")


# Load necessary libraries to process data in this script
library(proj4)
library(maptools)
library(rgdal)
library(lubridate)
library(raster)
library(sp)
library(sf)
library(mapview)
library(tidyverse)
library(doParallel)
library(foreach)
library(move)
library(moveVis)
library(anipaths)

#*************************************************************************************************************
##### Import and Prep Data #####
#*************************************************************************************************************

# Step 1: Set working directory where .csv files (data) are located
setwd("./Lab 1 Data Cleaning and Vizualization")

# Now read all the files 
ele.all <- list.files(path="./", pattern='.csv', all.files=FALSE, full.names=FALSE)

# Create Null data frame to append all the data
ele <- NULL

# Loop through the ele.all file
for (i in 1:length(ele.all)){
  #Read in data
  temp <- read.csv(ele.all[i],header=TRUE,sep=",")
  # Bind together (if data from same sensor/company, structure 'should' be the same
  ele <- rbind(ele,temp)
}

# or navigate to Raw.csv file and open
#ele.locs<-read.csv(file.choose())	

# Question 1: How many columns are there in your data file? 
#             What are the names of the 2 critical columns for the GPS locations?
# Question 2: How many unique IDs are in the dataset (how many elephants are we talking about)?
#             How many data points do we have?

# Select fields we are interested in and drop the others
cols <- c('collarid', 'Name', 'DATETIME', 'LATITUDE', 'LONGITUDE', 'HDOP', 'HEADING', 'SPEED', 'ALTITUDE')
ele <- ele[,cols]

# Simplify names for future processing ease
colnames <- c('collarid',"ID","DateTime","Lat","Long","DOP","Heading","Speed","Alt")
names(ele) <- colnames

# It is critical the time stamp for each fix is in the local time of the animal.
# Specify the date/time as a POSIX object. 
# Be sure to set the time zone! 

ele$Date <- as.POSIXct(x=ele$DateTime,
                       format = "%m/%d/%y %H:%M", tz="Africa/Nairobi")

# Check to see if converted properly.  The date should not change in this instance (GMT to GMT)	
attributes(ele$Date)
#The newly added Date column should be populated. 
#Problems can emerge if the format of the DateTime column in the CSV file differ
str(ele)

#*************************************************************************************************************
##### Data Viewing #####
# *************************************************************************************************************

#Before we clean the data, let's take a look at it and see if there are any obvious errors:

ID.all <- unique(ele$ID)

## STEP 1:
# Look at data in a static plot
par(mar = rep(2, 4))  #Adjusts margins to contain all info
plot(ele$Long,ele$Lat,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,asp = 1)
lines(ele$Long,ele$Lat,lwd=0.5,col="light gray")
points(ele$Long,ele$Lat,pch=".",cex=3)

#Export this map for comparison later in the lab. To export, use the export tab in the lower right panel.

# Loop through the dataset, subsetting each animal and graphing individually
for(i in 1:length(ID.all)){
  temp <- subset(ele, ID == ID.all[i])
  temp <- temp[complete.cases(temp[,3:4]),] # Only omits if these records are blank
  xy <- temp[c("Long","Lat")]
  proj.info <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  temp2 <- SpatialPointsDataFrame(xy,temp,proj4string = CRS(proj.info))
  points(temp2,pch=".",cex=4,col=i)
}


## STEP 2:
# Dynamic map of the data

# Create a spatial dataframe 
#       We will use the sf package. the sf and sp packages are popular r packages for working with 
#       spatial data in R. The sf package works by creating a spatial dataframe that specieis the 
#       coordinates and coordinate system information. This allows us to manipulate the data 
#       (e.g. transform the coordinate system) and to align the tracking data with other spatial data 
#       like polygons and rasters. 

# Question 4: Experiment with mapview package styles and plotting functions. What does each argument in the 
# below do? Do you notice any interesting patterns that you could explore in an analysis?
# Hint: Run '?mapview' in the console to explore all of the package arguments. 

library(sf)
library(mapview)

proj.info <- 4326 # set the EPSG number (lat/long WGS84)
ele.sf <- st_as_sf(ele, coords = c("Long", "Lat"), crs = proj.info)

# Use the mapview package to plot an interactive map. The layers button can control the background map
mapview(ele.sf)

# The `zcol` and `burst` arguments allow us to plot the dataset by individual ID and toggle on and off
mapview(ele.sf, zcol = 'ID', burst = TRUE, cex = 2, alpha = 1)


# Try plotting the data for Luna, and color the points by Speed. Hint: set burst to false. 


## Export spatial dataset
# When the data is in a spatial data class in R, we can export it as a shapefile for use in GIS software 
# (e.g. for creating publication maps). The data is an sf object, so we use the sf package function for
# exporting. 

# Question: Where does the shapefile save?

st_write(ele.sf, dsn = 'elephantdata.shp')

#*************************************************************************************************************
##### Data Cleaning #####
# *************************************************************************************************************

#STEP 1
# Remove any data on the collar that was recorded previous to the deployment on the animal
nrow(ele)
ele <- subset(ele,Date >= as.POSIXct("2016-05-16 00:00:00",tz="Africa/Nairobi"))

# Order the dataset - this orders by date for each individual
temp.or <- order(ele$ID,ele$Date,decreasing=FALSE)
ele <-ele[temp.or,]
ele[1:5,]

#STEP 2
# Remove incomplete records...Lat/Long = NA
# Question 5: How many 'complete.cases' are there and how many incomplete records were in the dataset?
nrow(ele)
ele <- ele[complete.cases(ele[,3:4]),]
nrow(ele)

summary(ele$Long)
summary(ele$Lat)

#Step 3:Remove exact duplicate records
# Question 6: GPS data can have duplicate recordings. How many observations are in your data file?  
# How many are duplicates (hint: run the code)
nrow(ele)
ele <- unique(ele)
nrow(ele)

# STEP 4: Remove data with high error rates
# Restrict values with large HDOP values (Dilution of Precision): HDOP should be < 5
# Restrict values with biologicaly unplausable speeds: Speeds should be < 10 km/hour
# And restrict values that are not 3D accurate: Altitude measure should be between 6000 - 13000 meters
# Remove NA values

# Graph Dilution of Precision before cleaning data
par(mfrow=c(1,1))
hist(ele$DOP,xlab="DOP",main="DOP distribution")
plot(ele$DOP,type="p",xlab="Index",ylab="DOP",main="Overall DOP distribution",cex=.5)
 
# Outlier removal. Only accept positions with a DOP < 5.0
ele <- subset(ele, DOP<5.0)
  
# Graph Dilution of Precision after removing poor DOP fixes
hist(ele$DOP,xlab="DOP",main="DOP distribution")
plot(ele$DOP,type="p",xlab="Index",ylab="DOP",main="Overall DOP distribution",cex=.5)

nrow(ele)

# STEP 5: Remove data with 3D error (elevation problems)
# Remove problem records....Height record must be positive...and within the altitude of the study area
# Assumption here is that if the height record is negative, the position is poor....remove.

ele <- ele[which(ele$Alt > 0),]

#Since the study area never gets below 350 m, remove all fixes below that altitude
ele <- ele[which(ele$Alt > 350),]

#Since the study area never gets above 13000 m, remove all fixes above that altitude
ele <- ele[which(ele$Alt < 13000),]

# You have now been through the 5 step cleaning process
# Map the data again
plot(ele$Long,ele$Lat,xlab="Longitude",ylab="Latitude",type="n",pch=".",cex=4,frame=FALSE,asp = 1)
lines(ele$Long,ele$Lat,lwd=0.5,col="light gray")
points(ele$Long,ele$Lat,pch=".",cex=3)
nrow(ele)


#Question7: Compare your pre-cleaning and post-cleaning maps.
#           Can you see changes in the data in the new map post cleaning? 
#           Is visualizing the data in a map the easiest way to clean it?

#***********Make a table labeling the number of data points in the dataset after each of the 5 steps*********


# Let's check data quality. Some analyses require regular GPS fixes. 
# Now that we have cleaned out biologically implausible fixes, lets check what our data look like over time.

# Question 8: What is the fix schedule of the data? 
#             Are there periods of missing data for any individual?
#             How might we clean up the fixes that are outside of the regular fix interval?

ggplot(ele, aes(x = Date, y = lubridate::minute(Date))) + geom_point() + facet_wrap(.~ID)


# *************************************************************************************************************
##### Covariate extraction #####
# *************************************************************************************************************

# We often need to assess GPS locations in relation to environmental variables. Use 
# the code below to import raster data of NDVI, human footprint index, and slope. 
# You will use these rasters to extract covariate values for elephant GPS locations. 

# Question 9: Overlap the tracking data on the raster data. Hint: You can use 
#             mapview with rasters too!    



library(raster)
## STEP 1: 
# Import raster layers. This is a link to the Earth Engine script to generate layers: 
# https://code.earthengine.google.com/22b6b4ca65d31d144e101ac6f3d4e973

ndvi <- raster('MODIS_NDVI_30m_2016_samburu.tif')
gHM <- raster('gHM_1000m_samburu.tif')
slope <- raster('SRTM_slope_30m_samburu.tif')

## STEP 2:
# To extract from the raster layers, the data should be a SpatialDataFrame (sp package). 
# Lets convert it here and set the projection

# creates a spatial dataframe
ele.sp <- SpatialPointsDataFrame(data = ele, coords = cbind(ele$Long, ele$Lat), proj4string = CRS("+init=epsg:4326"))

# take some time to explore the structure of SpatialPointsDataFrame objects
(ele.sp)


## STEP 3: 
# create matrix for used points - we add the covariate values to the columns of the matrix
used <- matrix(1, nrow = nrow(ele.sp), ncol = 4) # why 4 columns?

## STEP 4:
# extract covariate values for each GPS fix and record in the used matrix
system.time({
  used[,2] <- raster::extract(ndvi, ele.sp)
  used[,3] <- raster::extract(gHM, ele.sp)
  used[,4] <- raster::extract(slope, ele.sp)
})

# check the matrix
head(used)
summary(used)

# Clean up covariate names and convert to dataframe
mode(used) = "numeric"
used <- as.data.frame(used)
colnames(used) <- c("used", "ndvi", "gHM", 'slope')
head(used)

## STEP 5:
# bind covariate info back to the ele dataframe
ele <- cbind(ele, used)
head(ele) # notice that we bind it back to the regular ele dataframe

## Example of Parallel processing for raster extraction.
# Parallel processing can speed up this process. This is useful for large datasets of
# hundreds of thousands of points.

library(doParallel)
library(foreach)
cores <- 3
cl <- makeCluster(cores, output="")
registerDoParallel(cl)

# rasters must be in a list
r.list <- list(ndvi, gHM, slope)

# Extract using
system.time({
  extract <- foreach::foreach(i = 1:3) %dopar% # corresponds to number of covariates
    raster::extract(r.list[[i]], ele.sp)
})

# create a dataframe of the covariates called 'used'
used <- do.call(cbind, extract)
stopCluster(cl) # make sure to shut down the cluster

#*************************************************************************************************************
##### Assess Covariate Use #####
#*************************************************************************************************************
## Let's look at the distribution of covariate data that elephants used
hist(ele$ndvi) # NDVI
hist(ele$gHM) # Human Footprint
hist(ele$slope) # Slope

## How are elephants using covariates in relation to time of day?
# This study system is located near the equator, so we will use a standard 6:00am/6:00pm threshold to assign 
# each GPS relocation to day or night
# Question 10: What considerations are needed for your study species when creating a similar time of day variable? 
# Question 11: NDVI is calculated as the mean NDVI for the tracking period. What are some limitations of using a 
#           static mean value for environmental variables with temporal tracking data? How could we update the 
#           NDVI covariate to better match our tracking data?

## Look at covariates by Time of Day

# Create a day/night variable
ele$TOD <- ifelse(hour(ele$Date) > 6 & hour(ele$Date) <= 18, 'day', 'night')

# Get the hour of each relocation. We treat it as a factor for plotting
ele$hour <- as.factor(lubridate::hour(ele$Date))

# plot it
ggplot(ele) + geom_boxplot(aes(x = as.factor(hour(Date)), y = ndvi, fill = TOD))

## NOTE: These boxplots are one way to look at use of the environment and specific resources. Later, we will look at how to assess selection 
## for resources (i.e. higher or lower selection for certain resources relative to their availability).



# *************************************************************************************************************
##### Animate data #####
# *************************************************************************************************************
# Animate the data 
library(move)
library(moveVis)

## Sometimes it is useful to look at telemetrey data in a time series. This can help reveal temporal patterns 
# (e.g. repeated visits to water points, range shifts, migration, etc.). We will use the the movevis package 
# which is built off of the 'move' package and tidyverse for working with movement data. We also show an 
# example of animations using the anipaths package below, which uses a different method for animating the paths
# Get more information on animations using the moveVis package: http://movevis.org/articles/example-3.html

# Question 12: What patterns do you notice in the animation? Try subsetting the data to look at the paths of 
#           Habiba and Soutine only

# STEP 1:
# create a 'move' object. The move package is popular R package for working with movement data and is integrated with MoveBank. We will 
# explore this package more in the next lab.
ele.move <- df2move(ele, x = 'Long', y = 'Lat', time = 'Date', track_id = 'ID', proj = CRS("+init=epsg:4326"))

# shift to 6 hour resolution - this helps speed up the animation
ele.move <- align_move(ele.move, res = 6, unit = "hours")

## STEP 2:       
# create frames
# NOTE: you must create a mapbox account and get a token to use satellite data. Go to www.mapbox.com to register and create a token.
frames <- frames_spatial(ele.move, path_colours = c("red", "green", "blue"),
                         map_service = "mapbox", map_type = "satellite", alpha = 0.5,
                         map_token = 'pk.eyJ1IjoibmF0aGFuaGFobjUzIiwiYSI6ImNrdDBucWdiYjA3MmQzMnA4aTBxdTV0ZG8ifQ.G27aEfAf_MDbPuxgCy0Owg',
                         equidistant = FALSE) %>%
  # use the pipe notation from tidyverse to add additional arguments like a scalebar and timestamps
  add_scalebar(colour = "white", height = 0.022, position = "bottomright", label_margin = 1.4) %>%
  add_timestamps(ele.move, type = "label")

# animate the first 100 frames as a gif. More frames takes longer...
animate_frames(frames[1:100], out_file = "example_ele_sat.gif")


## Step 3:
# Look at just Habiba and Soutine
ele.move.subset <- ele.move[ele.move@trackId %in% c('Habiba', 'Soutine')]

# Make an animation of Habiba and Soutine's movement
frames <- frames_spatial(ele.move.subset, path_colours = c("red", "blue"),
                         map_service = "mapbox", map_type = "satellite", alpha = 0.5,
                         map_token = 'pk.eyJ1IjoibmF0aGFuaGFobjUzIiwiYSI6ImNrdDBucWdiYjA3MmQzMnA4aTBxdTV0ZG8ifQ.G27aEfAf_MDbPuxgCy0Owg',
                         equidistant = FALSE) %>%
  # use the pipe notation from tidyverse to add additional arguments like a scalebar and timestamps
  add_scalebar(colour = "white", height = 0.022, position = "bottomright", label_margin = 1.4) %>%
  add_timestamps(ele.move.subset, type = "label")

# slow down or speed up the animation using the fps (frames per second) argument. Less frames per second == slower animations
animate_frames(frames[1:100], fps = 15, out_file = "example_ele_subset2.gif")


# *************************************************************************************************************
# Animate Date with the anipaths package
# *************************************************************************************************************
library(ggmap)
library(anipaths)

# The anipaths package can also be used to animate data and has some different features. The package uses interpolation to approximate 
# the movement of the animals between GPS locations. 

# Question 13: How do the two packages differ in their output? Why might interpolation be useful or misleading? 
#           Hint: try setting return.paths = TRUE in the function call. 

## STEP 1:
# Get satellite data

# You must register an API key with google. Temporary key is used here but it will expire after the lab.
ggmap::register_google(key = "AIzaSyAyZnyz0E5teo9SbaKvyvoxkV1sAz-ty10", write = TRUE)

# create a map background. Use the mean of the coordinates to set the map center, and then set the zoom level and map type
background <- ggmap::get_googlemap(center = c(mean(ele$Long), mean(ele$Lat)),
                                   zoom = 12,
                                   maptype = "satellite")

## The data should be fed into anipaths using a SpatialDataFrame from the sp package. Lets convert it here and set the projection
ele.sp <- SpatialPointsDataFrame(data = ele, coords = cbind(ele$Long, ele$Lat), proj4string = CRS("+init=epsg:4326"))

## Step 2: 
# Animate the data -- This takes ~20 minutes to run with hourly movement. Try setting delta.t = 'day' or 'week' to run faster. 
animate_paths(paths = ele.sp,
              delta.t = "hour", # 'day' movement is much faster!
              coord = c("Long", "Lat"),
              Time.name = "Date",
              ID.name = "ID",
              pt.cex = 2,
              background = background,
              tail.wd = .5,
              tail.length = 1,
              pt.colors = 
                method = "html") # 'mp4' requires ffmpeg r package. Use 'html' to view animation in browser

# *************************************************************************************************************
# *************************************************************************************************************


