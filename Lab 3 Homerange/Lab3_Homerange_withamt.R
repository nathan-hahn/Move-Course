#' ***************************************************************************************************************************************
#' ***************************************************************************************************************************************

#' Lab: Conducting home range analyses on GPS data 
#' Description: Script to analyze GPS data using several home range estimation techniques

#'  ***************************************************************************************************************************************
#'  ***************************************************************************************************************************************
#' We will explore techniques to estimate home ranges of animals using GPS data. 
#' In this lab, we will look at space use by an elephant in and around Samburu National Reserve in northern Kenya. 
#' We will look at different ways of estimating the home range and some ways to use the homerange after it is
#' calculated. When assessing animal space use, the choice of model is also very reliant on what question(s) you are 
#' seeking to answer. As you work through the lab, think carefully about the research questions that each model 
#' can help address. 


#' TODO: Check UTM conversion in the ctmm package. Converting the coords wrong
#' TODO: href code for KDE section. Add code to experiment with h smoothing parameter? 

# Remove anything in memory
rm(list=ls())

#install.packages("shapeflies")
install.packages("data.table")
install.packages("adehabitatHR")

# Load appropriate libraries and set working directory
library(adehabitatHR)
library(mapview)
library(sf)
library(sp)
library(raster)
library(amt)

#### Step 1: Load and prep GPS location data ####
#' Open the file name M54short.csv and make sure the timestamp is formated correctly %m/%d/%y %H:%M
#' NOTE: We use the data.table package and the fread function to import the data. 
#' This is significantly faster than read.csv for large data files. Wrapping in 'as.data.frame()' 
#' gives us a normal df to work with

data <- as.data.frame(data.table::fread('./Lab 3 Homerange/M54short.csv')) #make sure to set the path correctly for your computer
head(data)

# Set the time zone by creating a POSIX object
data$timestamp <- as.POSIXct(x=data$timestamp,
                        format = "%m/%d/%y %H:%M", tz="Africa/Nairobi")

#' We need to project the Lat Long data into UTM so we can get areas easily
#' Question 1: What projection is the data in? Why do we need to convert it to UTM to investigate homerange size?
data.latlon <- data
coordinates(data.latlon) <- ~x+y # create a SpatialPointsDataFrame
proj4string(data.latlon) <- CRS("+init=epsg:4326") #4326 is the EPSG code of lat/lon WGS84
data.UTM <- spTransform(data.latlon, CRS("+init=epsg:32636")) #EPSG code for UTM Zone 36N (https://epsg.io/32636)

# check the data
mapview(data.UTM)

# go back to a dataframe
ele.df <- as.data.frame(data.UTM)

#' We now have our position data prepped in R and we can analysis of the home range  represented by the data. We will 
#' create our first home range using the most general method first: the Minimum Convex Polygon.

#### Step 2: Home Range Analysis using Minimum Convex Polygon (MCP) ####
#' The Minimum Convex Polygon draws the smallest convex polygon possible around an individual's GPS relocations. We 
#' will use the adehabitatHR package to calculate homerange sizes. This package requires us to supply the coordinates
#' and animal ID in a spatial points dataframe

# Make a track object using the amt package
ele.track <- make_track(ele.df, x, y, timestamp, id = individual.local.identifier,
                        crs = CRS("+init=epsg:32636"))

plot(ele.track)

# make an MCP
hr.mcp <- hr_mcp(ele.track, levels = c(0.5, 0.95, 1))

# look at the structure
str(hr.mcp)

#' QUESTION: What is the levels argument doing? How does changing the mcp percentile change the home range?
mapview(hr.mcp$mcp, burst = T, zcol = 'level', col.regions = c('red', 'orange', 'yellow')) + mapview(data.UTM, cex = 1)

# Calculate the size of the home range by different percentiles of the data. What are the units??
hr.pct <- hr_area(hr.mcp)
hr.pct

#' Question: Which percentile of data appears to contain the most outlier positions? Why? 

#' QUESTION #2: MCP percentile designations are not related to the density of points but the distance from the center 
#' of the MCP. In comparing the underlying points of the elephant movements to the 50th, 95th and 100th percentiles, 
#' what does each of these ranges highlight in the data? 

#### Step 3: Home Range using Kernel Density Estimation ####

#' Let's move to a method that provides more information on the distribution of the point density. We can compare MCP 
#' to the much more widely used Kernel density (KDE) home range approach (probably the most commonly used). With KDE, 
#' we can alter the smoothing parameter h to adjust the degree of smoothing we allow in our interpretation of the kernel 
#' around each GPS point. We will start by using a standard h parameter called h-ref.

#' A KDE homerange is produced as a raster, where each pixel represents a density of use by an animal. First create a 
#' raster template for the KDE

t.rast <- make_trast(ele.track, res = 250)

kde <- hr_kde(ele.track, trast = t.rast, levels = c(0.5,0.95, 0.99))

# get the areas for the estimates - how do they compare to the MCP estimates?
hr_area(kde)

# get 50th and 95th isopleths
isopleths <- hr_isopleths(kde)
isopleths$level <- as.factor(isopleths$level)
mapview(isopleths, zcol = 'level', burst = TRUE) + mapview(data.UTM, cex = 1)

# we can also look directly at the utilization distribution
plot(kde$ud)

# look at the smoothing parameter, which is estimated when the model is fit
kde$h

#' The default h parameter assumes that the data has a bivariate normal distribution. In cases where the animal migrates or has
#' multiple core areas, this can lead to oversmoothing of the homerange. 
#' QUESTION: What is the h parameter in the model? What happens if the paramter is changed? 

h.set = 1000

kde <- hr_kde(ele.track, trast = t.rast, levels = c(0.5, 0.95), h = h.set)

# plot the kde homerange. What do the isopleths represent? 
plot(kde)

# You can also plot to utilization distribution
plot(kde$ud)


#' QUESTION xx: Can you expand the kernel estimate code to calculate the UD for each year of the tracking data for M54?
#' In the code below, why do we specify same4all = TRUE? What does the grid parameter represent? 
#' How could you expand this to multiple individuals? 

# make a year variable
ele.track$year <- lubridate::year(ele.track$t_)

# fit multiple kde's in a list
split <- split(ele.track, ele.track$year)
kde.year <- lapply(split, hr_kde, trast = t.rast, levels = c(0.5, 0.95), h = 1000)

plot(kde.year$`2005`)
plot(kde.year$`2006`)

#' QUESTION: What do the density values represent? What do the areas of higher and lower use represent
#' ecologically? How does this model compare the the MCP in defining overall homerange? What about core areas? 


#' We can also compare home ranges using estimates of the overlap between kernel densities. This may be useful when 
#' assessing site fidelity between years for an individual, or looking at overlap in homeranges between different 
#' individuals in a population. The adehabitat package allows calculation of multiple overlap metrics, which can each be
#' useful depending on the questions that you are interested in. 

#' homerange overlap -- different methods described in Fieberg & Kochanny 2005
#' see amt page for description of each type: https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html 

# lets test two methods here, but you can play around with the others easily
hr_overlap(kde.year, type = "hr") # proportion of the home range of instance i that overlaps with the home range of instance j 
hr_overlap(kde.year, type = "ba") # Bhattacharyya’s affinity



#### Step 4: Local Convex Hulls ####
#' Now lets produce Nearest Neighbor Local Convex Hull (LoCoH) home ranges based on different numbers of nearest 
#' neighbors, which we define by changing the number of neighbors assigned in the k-parameter. Using the code below, 
#' create shape files for LoCoHs created using k values of 10, and 15. We will compare these two with an adaptive 
#' smoothing approach in LoCoH called alpha LoCoH where we will assign the smoothing parameter a in relation to the 
#' distance between points in the sample (see max.dist command).

#' For LoCoH, we can use a nearest neighbor distance or number to run the process.
#' Here, we use the alphLoCoH method which uses the maximum inter-point distance to assess neighbors
#' QUESTION: What are the units for max.dist?

max.dist = (sqrt((min(data.mcp@coords[,1], na.rm = T)-max(data.mcp@coords[,1]))^2 + 
                   (min(data.mcp@coords[,2])-max(data.mcp@coords[,2]))^2))/4
(max.dist)

#' First we will extract the home range using the adaptive LoCoH approach, where we define the a parameter as half the 
#' maximum distance between points (normally we would use the maximum difference, but reduce this to account for the 
#' fact the data includes the annual migration).

locoha <- LoCoH.a(data.mcp[,'individual.local.identifier'],a=max.dist)

#Now get a summary of the results of the analysis:
summary(locoha)

plot(locoha)


locohk <- hr_locoh(ele.track, type = 'k')
locoha <- hr_locoh(ele.track, type = 'a', a=max.dist)

#' QUESTION #5: How does LoCoH quantify density and how do the different density isopleths in LoCoH compare with the GPS points?

#' Questions #6: How does LoCoH compare with the Kernel methods?  What is the strength of Kernel relative to LoCoH? What is the strength of LoCoH relative to Kernel? 


#### Continous Time Movement Models ####

#' This section of the lab will explore the `ctmm` package workflow with intergenerational datasets from Samburu.
#' The `ctmm` package has extensive documentation and papers discussing how to implement continous time home range 
#' fitting on a wide variety of data structures (e.g. migrating species, irregular fix schedules, etc.). 

# rm(list=ls())
library(ctmm)

#raw datasets
data <- as.data.frame(data.table::fread('./Lab 3 Homerange/M54short.csv')) #make sure to set the path correctly for your computer
head(data)
colnames(data) <- c('individual.local.identifier','timestamp','location.lat','location.long')

#format date in raw datasets
data$timestamp<-strptime(data$timestamp,format="%m/%d/%Y %H:%M")

#' Turn raw datasets into ctmm telemetry objects - note that the ctmm package will automatically read in movebank-formatted
#' datasets because it knows what the column names are for ID, X and Y. See ?as.telemtery to see how to convert data with a 
#' different structure.

Ele.telemetry<-as.telemetry(data)

#plot telemetry objects
plot(Ele.telemetry)

#variograms
Ele.var<-variogram(Ele.telemetry)

plot(Ele.var)

#' You can now use the `variogram.fit` function to play with the parameters of the variogram
variogram.fit(Ele.var,interactive=TRUE)

#' QUESTION: Can you interpret the variogram? What does it tell you about the data structure? 

#Provide a model estimate (you can adjust through model selection)
#GUESS <- ctmm(tau=c(1/4,12)*60^2) # NOT WORKING
GUESS <- ctmm.guess(Ele.telemetry, interactive=FALSE) # WORKS

fitted<-ctmm.select(Ele.telemetry,GUESS,verbose=TRUE)

summary(fitted)

#' We can make sure that our selected model is explaining the most significant features of the animal’s movement. Let's 
#' plot the variogram again with our fitted models. How does the top model look?
plot(Ele.var,CTMM=fitted,col.CTMM=c("red","purple","blue","green"),fraction=0.65,level=0.5)

#' Now that we have a top model for the movement process, we can create an autocorrelated kernel density homerange using
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

#Print to shapefile
akdeHR<-SpatialPolygonsDataFrame.UD(akde.Ele,folder=dir,file=akdeHR)
writeShapefile(akde.Ele,folder=dir,file=akde.Ele)

#Open the pregenerated akdeHR shapefile in ArcGIS. Explore the error bars on the estimate
Questions #6: How does akdeHR compare to the rest of the approaches? When would this estimator be preferred?





