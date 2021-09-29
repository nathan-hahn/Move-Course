#' ***************************************************************************************************************************************
#' ***************************************************************************************************************************************

#' Lab: Conducting home range analyses on GPS data 
#' Description: Script to analyze GPS data using several home range estimation techniques

#'  ***************************************************************************************************************************************
#'  ***************************************************************************************************************************************

#' Overview:
#' We will explore techniques to estimate home ranges of animals using GPS data. In this lab, we will look at space 
#' use by an elephant in and around Samburu National Reserve in northern Kenya. We will look at different ways of 
#' estimating the home range and some ways to use the home range after it is calculated. When assessing animal space 
#' use, the choice of model is also very reliant on what question(s) you are seeking to answer. As you work through 
#' the lab, think carefully about the research questions that each model could help address. 

# Remove anything in memory
rm(list=ls())

install.packages("data.table")
install.packages("adehabitatHR")
devtools::install_github("ctmm-initiative/ctmm")

# Load appropriate libraries and set working directory
library(data.table)
library(adehabitatHR)
library(mapview)
library(sp)
library(raster)

#### Step 1: Load and prep GPS location data ####
#' Open the file name M54short.csv and make sure the timestamp is formated correctly %m/%d/%y %H:%M
#' NOTE: We use the data.table package and the fread function to import the data. 
#' This is significantly faster than read.csv for large data files. Wrapping in 'as.data.frame()' 
#' gives us a normal df to work with

data <- as.data.frame(data.table::fread('./Lab 3 Homerange/M54short.csv')) #make sure to set the path correctly for your computer
head(data)

# Set the time zone by creating a POSIX object
data$date <- as.POSIXct(x=data$timestamp,
                           format = "%m/%d/%y %H:%M", tz="Africa/Nairobi")

#' We need to project the Lat Long data into UTM so we can get areas easily
#' Question 1: What projection is the data in? Why do we need to convert it to UTM to investigate home range size?
data.latlon <- data
coordinates(data.latlon) <- ~location.long+location.lat # create a SpatialPointsDataFrame
proj4string(data.latlon) <- CRS("+init=epsg:4326") #4326 is the EPSG code of lat/lon WGS84
data.UTM <- spTransform(data.latlon, CRS("+init=epsg:32636")) #EPSG code for UTM Zone 36N (https://epsg.io/32636)


data.UTM$individual.local.identifier <- as.factor(data.UTM$individual.local.identifier)

# check the data on a map
plot(data.UTM)

# OR if you computer can handle it:
mapview(data.UTM)

#' We now have our position data prepped in R and we can analysis of the home range  represented by the data. We will 
#' create our first home range using the most general method first: the Minimum Convex Polygon.

#### Step 2: Home Range Analysis using Minimum Convex Polygon (MCP) ####
#' The Minimum Convex Polygon draws the smallest convex polygon possible around an individual's GPS relocations. We 
#' will use the adehabitatHR package to calculate home range sizes. This package requires us to supply the coordinates
#' and animal ID in a spatial points dataframe

# Fit the mcp. Tell the function what the column with ID is
hr.100 <- mcp(data.UTM[,c("individual.local.identifier")], percent = 100)
plot(hr.100)

#' Question: What is the class of the object produced by the mcp() function? Can we plot it on a map? 
mapview(hr.100) + mapview(data.UTM, cex = 1)

#Step 5: Calculate the size of the home range by different percentiles of the data

# this function will also plot the results. Do you notice any patterns in how home range size changes with the percentile?
hr.pct <- mcp.area(data.UTM[,c("individual.local.identifier")])
hr.pct

# change the percent to see how home range size changes in relation to the observed locations
hr.50 <- mcp(data.UTM[,c("individual.local.identifier")], percent = 50)
hr.95 <- mcp(data.UTM[,c("individual.local.identifier")], percent = 95)
mapview(hr.100, col.region = 'yellow') + mapview(hr.95, col.region = 'orange') + mapview(hr.50, col.region = 'red') + 
  mapview(data.UTM, cex = 1)

#' Question: What does the 'percent' parameter mean for MCP home ranges? 
#' Question: Which percentile of data appears to contain the most outlier positions? Why? 

#' Question: MCP percentile designations are not related to the density of points but the distance from the center 
#' of the MCP. In comparing the underlying points of the elephant movements to the MCP80 and the MCP100, does the MCP 80 
#' do a better job of representing the core area used by the elephants than the MCP100? Why?


#### Step 3: Home Range using Kernel Density Estimation ####

#' Let's move to a method that provides more information on the distribution of the point density. We can compare MCP 
#' to the much more widely used Kernel density (KDE) home range approach (probably the most commonly used). With KDE, 
#' we can alter the smoothing parameter h to adjust the degree of smoothing we allow in our interpretation of the kernel 
#' around each GPS point. We will start by using a standard h parameter called h-ref.

# generate a single kernel density estimate
kde.href <- kernelUD(data.UTM[,c("individual.local.identifier")], h = 'href',
                kern = 'bivnorm', grid = 1000) # reference 

par(mfrow = c(1,2)) # make a plot grid to compare KDE methods...

# turn the kde object from adehabitatHR into a raster. 
rast <- raster(as(kde.href[[1]], "SpatialPixelsDataFrame"))
plot(rast)

# Check the smoothing parameter value...
kde.href[[1]]@h

#' The href parameter assumes that the data has a bivariate normal distribution. In cases where the animal migrates or has
#' multiple core areas, this can lead to oversmoothing of the home range. 
#' QUESTION: What is the h parameter in the model? Try running `?kernelUD` to see what this parameter controls. What
#' happens if the paramter is changed? 

h.set = 1000

kde.h <- kernelUD(data.UTM[,c("individual.local.identifier")], h = h.set, grid = 1000) # reference 
rast <- raster(as(kde.h[[1]], "SpatialPixelsDataFrame"))
raster::plot(rast)

# turn off the plotting grid...
dev.off()

# compare home range areas for your two smoothing methods across different percentiles of the kernel distribution
kernel.area(kde.href, percent = c(50,95,99))
kernel.area(kde.h, percent = c(50,95,99))

# get the isopleths and calculate home range size of 50th and 90th percentile home ranges
iso.99 <- getverticeshr(kde.href, percent = 99)
iso.95 <- getverticeshr(kde.href, percent = 95)
iso.50 <- getverticeshr(kde.href, percent = 50)
mapview(iso.100, col.region = 'yellow') + mapview(iso.95, col.region = 'orange') + mapview(iso.50, col.region = 'red') + mapview(data.UTM, cex = 1)


#' QUESTION: How do the MCP and KDE methods compare? How does the size of the home range compare?
#' And the definition of core area? 

#' We now have a density distribution of the animals space use. However if we look
#' at the values on the raster map, they don't mean much. We can create a probability distribution
#' (often referd to as a utilization distribution) that will represent the probability of space use
#' within the home range. 

?getvolumeUD

ud <- getvolumeUD(kde.href)
ud.rast <- raster(as(ud$M54,"SpatialPixelsDataFrame"))

# What do the values of the raster represent? 
plot(ud.rast)
plot(iso.95, add=TRUE)
plot(iso.50, add = TRUE)

#' QUESTION: In many cases, you will want to fit density estimates to multiple individuals. Can you expand the kernel estimate \
#' code to calculate the UD for each year of the tracking data for M54? In the code below, why do we specify same4all = TRUE? 

data.UTM$year <- lubridate::year(data.UTM$date)

# fit kernel density estimates using the year as the ID variable. 
kde <- kernelUD(data.UTM[,c("year")], h = 'href', same4all = TRUE, grid = 1000) 

# check out how the estimates are stored when there is more than one 'individual' - in this case years. 
class(kde)
kde$`2005`
kde$`2006`

# plot the results
par(mfrow = c(1,2)) # make a grid for plotting...

# Use a for loop to loop over each 'individual' in the kde list
for(i in 1:length(kde)){
  rast <- raster(as(kde[[i]], "SpatialPixelsDataFrame"))
  plot(rast, main = paste(names(kde)[i]))
}

# clear the plots
dev.off()

# Get the utilization distribution and plot it on the map 
ud <- getvolumeUD(kde) 
rast <- raster(as(ud$`2005`,"SpatialPixelsDataFrame"))
rast[rast>99] <- NA 
mapview(rast) + mapview(data.UTM[data.UTM$year == 2005,], cex = 1.5, layer.name = 'M54 - 2005') 

#' We can also compare home ranges using estimates of the overlap between kernel densities. This may be useful when 
#' assessing site fidelity between years for an individual, or looking at overlap in home ranges between different 
#' individuals in a population. The adehabitat package allows calculation of multiple overlap metrics, which can each be
#' useful depending on the questions that you are interested in. 

# explore the options here:
?kerneloverlaphr()

## home range overlap -- different methods described in Fieberg & Kochanny 2005
kerneloverlaphr(kde, method = 'HR') # proportion of home range overlap
kerneloverlaphr(kde, method = 'BA') # Battacharyya's affinity - assumes that space use is independent 

#' What are the differences in these two methods? Why is Battacharyya's affinity much higher than the regular
#' home range overlap method?

#### Continous Time Movement Models ####

#' This section of the lab will explore the `ctmm` package workflow with intergenerational datasets from Samburu.
#' The `ctmm` package has extensive documentation and papers discussing how to implement continous time home range 
#' fitting on a wide variety of data structures (e.g. migrating species, irregular fix schedules, etc.). 

dev.off()
library(ctmm)

# raw datasets
data <- as.data.frame(data.table::fread('./Lab 3 Homerange/M54short.csv')) #make sure to set the path correctly for your computer
head(data)

# format date in raw datasets
data$timestamp<-strptime(data$timestamp,format="%m/%d/%Y %H:%M")

#' Turn raw datasets into ctmm telemetry objects - note that the ctmm package will automatically read in movebank-formatted
#' datasets because it knows what the column names are for ID, X and Y. See ?as.telemtery to see how to convert data with a 
#' different strucutre.

# define projection that we want
utm_36 <- "+proj=utm +zone=36 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" #UTM Zone 36N

Ele.telemetry<-as.telemetry(data, projection = utm_36)

#plot telemetry objects
plot(Ele.telemetry)

#variograms
Ele.var<-variogram(Ele.telemetry)

plot(Ele.var)

#' QUESTION: Can you interpret the variogram? What does it tell you about the data structure? 
#' see the package docs for more details on the importance of variograms: 
#' https://ctmm-initiative.github.io/ctmm/articles/variogram.html

#' Next we create a continuous time movement model to capture the movement process. We must estimate some initial parameters,
#' and there are two ways to do this. 

# Option 1: Provide a model estimate
GUESS <- ctmm.guess(Ele.telemetry, interactive=FALSE) 

# Option 2: Use the varigram.fit function to select parameters interactively. Click the setting gear icon in the plot window
# and adjust the values as needed. When you are happy, click 'Save to GUESS'.
variogram.fit(Ele.var,interactive=TRUE)

# fit the ctmm model with multiple movement processes and select the best one
#         Note that this can take awhile to run on slower computers. For convenience, we have 
#         added the set of fitted continous time movement models to the lab folder. 
fitted<-ctmm.select(Ele.telemetry,GUESS,verbose=TRUE)

# If needed, run the line below to add a fitted model to your environment. We used the ctmm.guess() output to construct this one
#fitted <- readRDS('ctmm_fitted_model.RDS')


# Question: Which movement processes is the best for our data?
summary(fitted)

#' We can make sure that our selected model is explaining the most significant features of the animal’s movement. Let's 
#' plot the variogram again with our fitted models. How does the top model look?
plot(Ele.var,CTMM=fitted,col.CTMM=c("red","purple","blue","green"),fraction=0.65,level=0.5)

#' Now that we have a top model for the movement process, we can create an autocorrelated kernel density home range using
#' the `akde` function. 

# extract the top model
ouf<-fitted[[1]]
# fit akde 
akde.Ele<-akde(Ele.telemetry,CTMM=ouf)
# plot it
plot(Ele.telemetry,UD=akde.Ele)

class(akde.Ele)
plot(akde.Ele)

akde.rast <- raster(akde.Ele)
akde.rast[akde.rast > .99] <- NA
mapview(akde.rast)

#' Question: How does akdeHR compare to the rest of the approaches? When would this estimator be preferred?