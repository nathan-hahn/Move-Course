# ******************************************************************************
# ******************************************************************************
# Lab 2: Trajectory analysis
# Description: Create trajectories, explore summary stats and path-level metrics 

# ******************************************************************************
# ******************************************************************************

# Remove anything in memory
rm(list=ls())

# Install necessary libraries
# ONLY RUN THIS ONCE. 
install.packages("adehabitatLT")
install.packages("rworldmap")

# Load libraries used in this script
library(tidyverse)
library(move)
library(moveVis)
library(mapview)

# Some packages have long-form documentation, which you can access within R studio with the `vignette()` function.
# Browse the vignettes for two popular movement packages:
vignette('move')
vignette('adehabitatLT')

# Links to the same documentation: 
# https://cran.r-project.org/web/packages/move/vignettes/move.html
# https://mran.microsoft.com/snapshot/2016-08-05/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf

## Questions:
# 1. Why is it important to consider the fix rate of your data? What type of fix rate is most appropriate for the `adehabitatLT` package?


#*******************************************************************************
##### Import, Prep, Visualize Data #####
#*******************************************************************************

# Step 1: Set working directory where .csv files (data) are located
setwd("./Lab 2 Trajectories")

#### Import data ####
# today's data can be loaded from 'moveVis' package
# details & metadata in movebank repository: https://www.doi.org/10.5441/001/1.v1cs4nn0
data("whitestork_data") 

# inspect - what types of objects are each of these?
m
n.indiv(m)
str(df)
head(df)

## start by just using the dataframe structure
# rename for easier use
stork <- df %>% dplyr::select(time=timestamp, lon=`location-long`, lat=`location-lat`, name)
str(stork)

stork$name <- as.factor(stork$name)
str(stork)

## --- Note on POSIXct times: 
str(stork$time)
        # these data loaded with the correct format, this won't always happen

# for demonstration, create a vector of the first 100 of these timestamps as characters, which is how they'll load from csv
times <- as.character(stork$time[1:100])
str(times)

# convert to POSIXct 
times2 <- as.POSIXct(times, format = "%Y-%m-%d %H:%M:%OS", tz="UTC")
str(times2)
str(stork$time[1:100])
        # these match, as expected 
        # Look back at the code from lab 1, line 88. Notice the format here is *slightly* different. 
        # How exactly is it different, and why? What happens if you change the format argument to another format?         [list of formats available by running ?as.POSIXct()]

rm(times, times2) #remove these objects from environment
## --- end of POSIXct practice


## Back to data. They have already been cleaned, but we'll do a few quick checks 
stork <- unique(stork)
stork <- stork[complete.cases(stork),]
        ## Any missing or duplicated records?

# To look at distance-related aspects of trajectory, often good to convert from lat/lon to UTM
# ** for adehabitatLT metrics, must be in UTM
library(sf)

stork_utm <- st_as_sf(stork, coords = c('lon', 'lat'), crs = "+proj=longlat +datum=WGS84") # make a spatial object
str(stork_utm)
crs(stork_utm)

stork_utm <- st_transform(stork_utm, crs = "+proj=utm +zone=32n") #transform spatial object from wgs84 to 
crs(stork_utm)
        ## What is different between the crs?

mapView(stork_utm, cex = 2)  #if you've got a speedy computer, you can try this but it is too much for mine

# convert projected object back to a dataframe (tidyverse method)
stork_utm <- stork_utm %>%
        mutate(x = unlist(map(stork_utm$geometry,1)),
               y = unlist(map(stork_utm$geometry,2)))
stork_utm$geometry <- NULL
head(stork_utm)
        ## compare the location data in this to that of original 'stork' dataframe

#### 'Move' objects (move package) #####
# One popular movement structure, designed to work well with movebank

# create moveStack from the stork_utm data
m_utm <- move(x=stork_utm$x, y=stork_utm$y, 
                   time=as.POSIXct(stork$time, format = "%Y-%m-%d %H:%M:%OS", tz="UTC"), 
                   proj=CRS("+proj=utm +zone=32n"),  #same as what we used above
                   data=stork_utm, animal=stork_utm$name)
m_utm
n.indiv(m_utm)
head(as.data.frame(m_utm)) #can return to dataframe using as.data.frame()

# plot movestack
plot(m_utm, xlab="X", ylab="Y",type="b", pch=16, cex=0.5, col=c(1:15)[m_utm@trackId])

# extract single indiv from moveStack - creates a move object (one trajectory)
louis <- m_utm[["Louis"]]

# extract a range of dates from this indiv
range(timestamps(louis))
louis_subset <- louis[as.Date(timestamps(louis)) %in% c(as.Date("2018-07-30"):as.Date("2018-08-15"))]
range(timestamps(louis_subset))

#**********************************************************************
##### Fundamental metrics: step-length, dt, turning angle, part 1 #####
#**********************************************************************
## Using the move or moveStack objects ##
# step length, dt, turning angle using move (trajectory not regular)
timeLag(louis, units="mins")[1:5] #first 5 only
distance(louis)[1:5] 
speed(louis)[1:5]
angle(louis)[1:5]
        # What units are each of these quantities in?

# Look at speed vs timeLag
plot(timeLag(louis, units="mins"), speed(louis), pch=16)

which(timeLag(louis, units="mins") > 300) #inspect the outliers
which(speed(louis) > 1000)
timeLag(louis, units="mins")[4429] #not going to do anything with these for now...

plot(timeLag(louis, units="mins"), speed(louis), pch=16, xlim=c(0,30), ylim=c(0,100))# zoom in to the majority of points

# if applied to movestack (multiple indivs), each one of the above function returns a list
str(timeLag(m_utm, units="mins")[1:5]) 

## inspect timelags/dt
summary(timeLag(louis, units="mins"))
        # is this trajectory regular? 

summary(unlist(timeLag(m_utm, units="mins")))
hist(unlist(timeLag(m_utm, units="mins")), breaks=100) # hard to see the tail
hist(unlist(timeLag(m_utm, units="mins")), breaks=100, ylim=c(0,1000)) # zoom in, cutting off the top of the largest bin
        # are the trajectories overall regular?

##--- Note: can also use these on lat/lon (m instead of m_utm)
# ** move package works with either lat/lon or UTM, some functions may only work with one or the other
speed(m[["Louis"]])[1:5] #what units?
angle(m[["Louis"]])[1:5]
turnAngleGc(m[["Louis"]])[1:5] #only for lat/lon
        # how are angle and turnAngle different?

##--- end of note

#*******************************************************************************
##### Trajectory wrangling #####
#*******************************************************************************
# recall stork_utm is the dataframe version of m_utm 
stork_utm <- stork_utm %>% group_by(name) %>% arrange(name, time) %>% ungroup() # make sure the rows are ordered by time 

# remove two individuals w/ lower resolution
stork_utm <- stork_utm[!stork_utm$name %in% c('Muffine', 'Redrunner'),]

## Easier to work with regular trajectories, especially in adehabitatLT
# Here, we can thin data to make regular because we have v high resolution fixes,
# sometimes it may not be practical to effectively discard data like this
library(lubridate)

stork_reg <- stork_utm %>%
        filter(minute(time) > 55 | minute(time) < 5 | between(minute(time), 27, 33)) %>% droplevels() # keeping fixes that occur on the half hour, give or take a few minutes
summary(stork_reg$name) # num fixes per indiv

## Create trajectory using adehabitatLT package (alternative to a move/moveStack)
library(adehabitatLT)

stork_traj <- as.ltraj(xy=stork_reg[,c("x","y")], date=stork_reg$time, id=as.character(stork_reg$name), typeII=TRUE)

# inspect this object
stork_traj
head(stork_traj[[1]])
plot(stork_traj)
plot(stork_traj[1])

# Look at fix rate (dt)
is.regular(stork_traj) # does this match what we expected?
is.regular(stork_traj[1]) 
        # What's going on?

# Can also move directly between move objects and ltraj objects
louis_traj <- as(louis, "ltraj") #move to ltraj
louis_traj

louis2 <- move(louis_traj) #ltraj back to move

## Bursts ## 
# Useful for identifying missing data and deal with large periods of missing data 
# Goal of this chunk: make the stork trajectories regular -- analyses on regular trajectories are easier

# set burst function -- no relocs within x hours creates a separate burst, dt must be specified in seconds
reg_func <- function(dt) { 
        return(dt > (24*3600)) # must supply as seconds
}

stork_burst <- cutltraj(stork_traj, 'reg_func(dt)', nextr = TRUE)

# Explore the new trajectory object -- What do the bursts look like? What happens if you change the burst function to a shorter time?
stork_burst
is.regular(stork_burst) 

## Why we don't have regularity: We have missing data points, but they aren't specified in the data. 
## Solution: Fill in NAs to be able to calculate fix rate
refda <- strptime('2018-06-30 00:00:00', "%Y-%m-%d %H:%M:%S", tz="UTC")   #add ref date
stork_burst <- setNA(stork_burst, refda, dt = 30, units = 'min')
stork_burst

stork_burst <- sett0(stork_burst, refda, 30, units = 'min') # round the timestamps to make a 'regular' trajectory
stork_burst
is.regular(stork_burst)

# create a dataframe to use for movement metrics
stork_clean <- ld(stork_burst) # ld() turns traj to df, adds columns with trajectory metrics
head(stork_clean)
        # Expectation check - why are there more observation in stork_clean than stork_reg?
        # What columns have been added to data?


#*******************************************************************************
##### Fundamental metrics: step-length, turning angle, Part 2 #####
#*******************************************************************************
# look at these metrics again, now using the regular trajectories and the adehabitatLT package
# remember: adehabitatLT requires UTM/other equidistant format!! 

## Step length
hist(stork_clean$dist)
boxplot(stork_clean$dist)
quantile(stork_clean$dist, na.rm = TRUE)

# calculate step lengths over time -- Are these values biologically reasonable? How would you adapt this if you wanted to convert to speed in km/hr?
ggplot(stork_clean, aes(x = date, y = dist/1000)) + geom_point() + geom_line() + facet_wrap(.~id) + ylab('dist moved in 30 min (km)') 

## Daily distance moved
# create a day variable
stork_clean$ymd <- lubridate::as_date(stork_clean$date)

# aggregate by day
calc_stat <- function(x) c(sum = sum(x))
mov_daily <- as.data.frame(aggregate(dist ~ ymd + id, stork_clean, calc_stat)) # aggregated stats by day
head(mov_daily)

# plot
ggplot(mov_daily, aes(x = ymd, y = dist)) + geom_point() +geom_line() + facet_wrap(.~id)

## Turning angles
rose.diag(stork_clean[!is.na(stork_clean$rel.angle),]$rel.angle, bins=24, prop=1.8, main = 'rose diagram of turning angles')

# how do turning angles relate to step length? 
plot(stork_clean$dist, stork_clean$rel.angle, pch = 19)

##--- Extra: write some code to explore how the summary metrics/relationships between them are different using the regular/thinned vs irregular/original data. What do you see? How does fix rate/regularity impact these metrics?

#-- put code here --#

##---

#*******************************************************************************
##### Net-squared displacement & tortuosity #####
#*******************************************************************************
# What is net squared displacement?


# Where is net squared displacement stored in the dataset?
head(stork_clean)

# What does it look like over time? How could we use this to identify migration patterns?
ggplot(stork_clean, aes(x = date, y = R2n/1000)) + geom_point() + facet_wrap(.~id)

# See R package 'migrateR' for additional methods related to net squared displacement 
# The supplemental file for this paper is r code for NSD in practice:
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0149594#pone.0149594.s004


### Tortuosity ###
# What is tortuosity?

# Look at a trajectory:
plot(stork_traj[2])
        # What things stand out about/what are any tortuosity patterns?

?sliwinltr() # pull up help file for this function - look through the details (what type of object can you use, what are the arguments/options?)

stork_traj[2] # approx 2400 relocations

step_len <- 400 # what does this value mean for the size of the sliding window?
sliwinltr(stork_traj[2], fun = function(x) mean(cos(x$rel.angle)), type="locs", step=step_len)
# In broad terms (don't worry about looking at specific cosine values), does the tortuosity indicated here match the pattern you saw in the trajectory by eye?
# Try changing the step_len or looking at another trajectory


#*******************************************************************************
##### Note on irregular fixes #####
#*******************************************************************************
# In practice, we often don't have enough data to discard a bunch to make a regular trajectory
# Consider one stork with lower resolution than the others
red <- m[["Redrunner"]]
plot(red)
red
nrow(red@data) #600 total fixes

summary(timeLag(red, units="mins")) # most fixes are every ~90 min
plot(timeLag(red, units="mins"))
summary(distance(red))

plot(timeLag(red, units="mins"), distance(red))

## Options for what to do:
# 1. Could follow adehabitat approach to thin data and create bursts (lines 184-219) but will lose a fair number of fixes:
sum(timeLag(red, units="mins")>100) #10% of all fixes

# 2. move also has a function to thin trajectory to make the trajectory regular-ish (turns into a burst) 
red_thin <- thinTrackTime(red, interval = as.difftime(90, units='mins'),
                            tolerance = as.difftime(5, units='mins')) 
summary(timeLag(red_thin, units="mins"))

plot(red_thin)
nrow(red_thin@data) 

red_thin
# `bursts` attribute: 528 fixes are part of 'selected' (aka regularish, 90 min), 62 are not - can use the 538 as a 'regular' subset. still then lose some data 
# this "regularizing" approach is a less code than what's above, and just labels the fixes that meet the timestep interval specified

# 3. Could interpolate using move to fill in missing timestamps -- is only appropriate in certain instances/if there's a good reason, such as running an analysis that requires a regular trajectory. 
# I would probably be comfortable with it with these data because migration is a very directed movement, so it's unlikely that in the places missing fixes, the bird did some movement way off track. I would then want to follow up my analysis with a sensitivity analysis to ensure the interpolated points weren't biasing results
# also using move package

red_interp <- interpolateTime(red, time=as.difftime(90, units="mins"), spaceMethod='greatcircle') #interpolate a fix every 90 min, using great circle interp method
plot(red) #black circles = actual data
points(red_interp, col="red", pch=20, cex=0.6) #red = actual data and interpolated points

summary(timeLag(red_interp, "mins"))

# 4. Carefully proceed with analysis on irregular fixes, making sure that results aren't correlated with timesteps (i.e., that fix rate isn't driving the results)



####--- END OF LAB ---####


#*******************************************************************************
##### Movebank (Don't try to run this - we'll talk about it during lab) #####
#*******************************************************************************
# all from move package - documentation for more details
# store the movebank credentials
cred <- movebankLogin() #can specify username, password inside parentheses (username="", password="")

# download data from movebank directly into R
stork1 <- getDataRepositoryData("doi:10.5441/001/1.v1cs4nn0") # get data via movebank repository
stork2 <- getMovebankData(study="MPIO white stork lifetime tracking data (2013-2014)",login=cred,
                         animalName="DER AR439",includeExtraSensors=TRUE) #get data for one animal