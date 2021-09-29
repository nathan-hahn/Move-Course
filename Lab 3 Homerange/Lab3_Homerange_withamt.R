#' ***************************************************************************************************************************************
#' ***************************************************************************************************************************************

#' Lab: Conducting home range analyses on GPS data - Using amt package
#' Description: Script to analyze GPS data using several home range estimation techniques

#'  ***************************************************************************************************************************************
#'  ***************************************************************************************************************************************
#' We will explore techniques to estimate home ranges of animals using GPS data. 
#' In this lab, we will look at space use by an elephant in and around Samburu National Reserve in northern Kenya. 
#' We will look at different ways of estimating the home range and some ways to use the homerange after it is
#' calculated. When assessing animal space use, the choice of model is also very reliant on what question(s) you are 
#' seeking to answer. As you work through the lab, think carefully about the research questions that each model 
#' can help address. 


# Remove anything in memory
rm(list=ls())

install.packages("data.table")
install.packages("amt")

# Load appropriate libraries and set working directory
library(amt)
library(mapview)
library(sf)
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



