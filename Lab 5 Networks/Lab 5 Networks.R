
#### Lab 5 - Network Connectivity ####

#' # Introduction
#' This document introduces the various approaches developed to help in maintaining movement 
#' connectivity for African elephants in Northern Kenya, but they can be applied to any systems 
#' where GPS tracking data are available. The training module focuses on  approaches introduced 
#' in the following manuscript:
#'  
#' * 1. Bastille-Rousseau, G., Douglas-Hamilton, I., Blake, S., Northrup, J., Wittemyer, G. (2018) Applying network theory to animal movements to identify properties of landscape space use. Ecological Applications 28: 854-864
#' 
#' The code for this lab was originally developed by Guillaume Bastille-Rousseau, and modified 
#' for this course.
#'
#' #0 - Getting started
#' To install/download the data as well as other packages on GitHub, the user should first 
#' install/load the R package *devtools* as well as install the package associated to this 
#' training module. Installing and loading the *training_connectivity* packages will install 
#' other packages and facilitate access to the datasets.  

# install packages
install.packages('devtools') # this allows you to download packages from github
devtools::install_github("BastilleRousseau/moveNT")
install.packages('adehabitatLT')

# load packages
library(moveNT)
library(mapview)
library(adehabitatLT)

#' #2 - Applying network theory to movement data (package *moveNT*)
#' This training module presents a new approach to study animal movement 
#' based on network theory. Network (graph) theory is a popular analytical framework to 
#' characterize the structure and dynamics among discrete objects (nodes) and the connections 
#' among them (edges) and is particularly effective at identifying critical hubs and patterns 
#' of connectivity. For example, a network can be represented from the different airports
#' within a country (hubs) and the connections among them (edges). Network theory can be used to 
#' calculate network metrics that would help identify airports with higher connectivity or 
#' importance. Similarly, the identification of attributes related to connectivity is a critical
#' component of understanding animal movement, yet network theory has rarely been applied 
#' directly to animal relocation data. We develop an approach that allows the analysis of 
#' movement data using network theory by defining occupied pixels as nodes and connections 
#' among these pixels as edges. By identifying critical nodes, our approach provides a robust 
#' quantitative framework to identify local properties of space use. The approach is intuitive, 
#' and can be implemented across imperfectly sampled or large-scale data sets efficiently, 
#' providing a new valuable framework. The main steps of the approach are presented in Figure 2.
#' Basically, a grid is overlayed over the GPS locations and transitions among all pixels 
#' (estimated from the movement data) are tallied into and adjacency matrix. From this matrix, 
#' it is possible to calculate various network metrics. 

#' #A- Opening data and creation of trajectory object. 
#' We will load and open the elephant data and create a trajectory object using the 
#' `adeHabitatLT` package

#setwd("./Move-Course/Lab 5 Networks") # set working directory as needed

## Load the data
load("./Lab 5 Networks/elephants.rda")

# how many elephants are there?
unique(elephants$id)

# what is the size of the dataset?
dim(elephants)

## Create trajectory using adehabitatLT package
library(adehabitatLT)
elephants$date<-as.POSIXct(strptime(as.character(elephants$date),"%Y-%m-%d %H:%M:%S"))
temp_id<-paste(elephants$id, elephants$date)
elephants<-elephants[!duplicated(temp_id),]

# what columns contain the coordinates? Does the project need to be modified before creating a trajectory?
head(elephants)

# create trajectory
traj<-as.ltraj(xy=elephants[,2:3], date=elephants$date, id=elephants$id)
ref.dat<-strptime("2000-01-01 00:00","%Y-%m-%d %H:%M")
traj2<- sett0(traj, ref.dat, 1, units = "hour", tol=0.5)
traj3<-cutltraj(traj2, "dt > 3600*24",nextr = T)
traj4<-redisltraj(traj3, 3600, type="time")

#' * QUESTION: Explain the difference between traj, traj2, traj3, and traj4? 
#' 
#' 
#' ##B- Converting movement to adjacency matrix  - *traj2adj*
#' 
#' The function *traj2adj* converts a trajectory object of class *ltraj* to an adjacency matrix.
#' This is done by overlapping a grid over the relocation data and tallying the number of 
#' transitions among each pixel. Users need to specify the grid size, which can be based on 
#' distance travelled. The function *quant* is a wrapper that allows to sample a quantile of 
#' step length distribution from a *ltraj* object. Output produced by *traj2adj* is a list 
#' containing the adjacency matrix, the grid used (raster format), and a raster indicating 
#' pixel numbers that are occupied. These rasters are used by other functions such as 
#' *adj2stack* and *clustnet*. 
#'  
#' First, we will apply *traj2adj* to the first elephant. 

adj_patches<-traj2adj(traj4[1], res=quant(traj4[1], p=0.5)) #Grid size based on median
dim(adj_patches[[1]]) # Size of the adjacency matrix 
plot(adj_patches[[2]]) #Plot grid used
plot(adj_patches[[3]]) #Plot occupied pixels 

#' ##C- Calculation of network metrics  - *adj2stack*
#' 
#' The function *adj2stack* takes the output of function *traj2adj* and calculates a series of 
#' node- and graph-level metrics. Each metric is stored as an individual raster and the output 
#' is a raster stack combining each metric. Graph-level metrics are also stored as a raster, 
#' each containing an unique value. The function *graphmet* extracts graph-level metrics. The 
#' function *val* extracts only the occupied cells (remove NA) in a raster and allows the 
#' calculation of statistics from node-level metrics. 

# Using multi-patches movement and median distance traveled 
stck<-adj2stack(adj_patches,grph=T) #Plot the node-level metrics at the same time 
plot(stck) #Plot also the graph-level metrics (not really useful)
plot(stck[[4]], main = 'Apollo - Degree') #Plot only one metric (degree)
graphmet(stck) # Extract graph-level metrics 

#' * QUESTION: What does degree relate to for the elephant Apollo? What do areas with higher 
#' values mean ecologically? 
#' * QUESTION: Can you try plotting betweeness (stck[[5]]) and see what changes? What do high 
#' values mean for betweeness?

#' Three metrics that can be particularly informative for animal movement are the weight, 
#' degree, and betweenness. Weight simply represents the number of locations within a pixel. 
#' Degree represents the number of different pixels a given pixel is connected too. Betweenness
#' indicates the importance of a given pixel to access any other pixels and therefore indicates
#' its importance for connectivity. 

#' #D- Calculating network metrics for multiple individuals

#' When applying the analyses to multiple individuals and hoping to summarize them together, 
#' we need to use the same grid. *traj2adj* allows the creation of a user-defined grid that 
#' will be constant across individuals. 

# create user defined grid - first we need to define the bounding area of the GPS data 
# I'm going to use the min and max of the x and y coordinates in the tracking data to define
# the bounding box
tt<-SpatialPoints(ld(traj4)[,1:2])
tt1<-apply(coordinates(tt), 2, min)
tt2<-apply(coordinates(tt), 2, max)

# create the raster 
# I am using the floor and ceiling commands to round the coordinates down/up
# We also define the grid resolution based on rounded median step length (250m)
median(ld(traj4)$dist, na.rm=T) # check median step length
grid.res = 250 # round down a bit for a standard grid size
ras250<-raster(xmn=floor(tt1[1]), ymn=floor(tt1[2]),xmx=ceiling(tt2[1]), ymx=ceiling(tt2[2]), res= grid.res) 

# create adjacency matrix
adj_patches2<-traj2adj(traj4, res=quant(traj4, p=0.5), grid=ras250) #Grid size based on median
plot(adj_patches2[[2]]) #Crop version of the grid created
plot(adj_patches2[[3]]) #Plot occupied pixels 

#' #E- Looping over all individuals *loop*
#' The function *loop* is a wrapper of *traj2adj* and *adj2stack* applied to all individuals 
#' within a trajectory. The function will keep the same grid for all individuals. The user 
#' simply need to specify the trajectory object and the grid size. The loop function also adds 
#' additional movement properties regarding speed, absolute angle, and turning angle. We will 
#' use *loop* to apply the network approach to our XX elephants and display the results for the
#' second and third elephants. 
#' 
#' WARNING: This will take a few minutes to run

# apply the loop function
out1<-loop(traj4)

# plot results
plot(out1[[2]]) #Plot the second elephant
plot(out1[[3]]) #Plot the third elephant

#' ##F- Mosaic individual
#' 
#' Even if the the function *loop* perform the analysis to every individuals, the outputs 
#' produced are at the individual-level. We can see this by looking at the list elements in the
#' object `out1` - there is one for each individual. The function *mosaic_network* can combine 
#' the different individual levels into a single raster representation. When multiple 
#' individuals overlap, *mosaic_network* applies a function (mean or max) to calculate a 
#' population-level value for that pixel. To use the function, the user needs to specify which 
#' variable to mosaic (using index), whether to scale the individual layers (recommended) and 
#' the function to apply. We recommend to use mean for degree and weight and max for the 
#' betweenness. 
#' 
#' * QUESTION: Once you have run the code below, try changing the function (mean, median, max) 
#' to see how it affects the outputs. In terms of the ecology, what is the difference between 
#' summarizing individual differences in landscape use by mean vs. max? 

mean_weight<-mosaic_network(out1, index=2, sc=T, fun=mean) #Perform mean weight (not-interpolated)
plot(mean_weight, main = 'mean weight')

mean_degree<-mosaic_network(out1, index=4, sc=T, fun=mean) #Perform mean weight (not-interpolated)
plot(mean_degree, main = 'mean degree')

max_between<-mosaic_network(out1, index=5, sc=T, fun=max) #Perform max weight (not-interpolated)
plot(max_between, main = 'max betweeness')

# for any of the rasters, use this code to plot in mapview for closer inspection
r.plot <- max_between
crs(r.plot) <- '+init=epsg:32637' #EPSG code for UTM Zone 37N (https://epsg.io/32637)
crs(tt) <- '+init=epsg:32637'

mapview(r.plot)

# you can try plotting it with the tracking data - will be slow
#mapview(r.plot) + mapview(tt, cex = 1)


#' ##F- Linear interpolation 
#' As can be seen in the last plot produced, one of the limitations of the current approach is 
#' that it creates gaps in areas where no locations are observed (only pixels with gps locations
#' in them have values). This can sometimes limit interpretability or the visual appeal of the
#' maps produced. To assist with this, we created a linear interpolation approach that can be
#' applied to the individual level network calculation (i.e. after *loop*). The interpolation
#' linearly interpolate each step (i.e. straight line) and assign the network metric of each 
#' starting location to the whole step. When multiples overlap in a pixel, a function is 
#' applied to summarize these steps (e.g. mean or max). This function will take an output from
#' *loop* and performed the interpolation for five metrics (weight, degree, betweenness, speed,
#' and turning angles). We recommend to take the mean for weight, degree, betweenness, and 
#' speed, the max for betweenness, and the dot-product for the turning angles (default).   

# The interpolation function takes awhile to run, but you can try it if you have time
#out2<-interpolation(traj4, out1)

# As a shortcut, I provided the interpolated data. Read in RData file with the interpolation
out2 <- readRDS('./Lab 5 Networks/network_interpolation_out2.RDS')

mean_mean_degree <- mosaic_network(out2, index=2, sc=T, fun=mean)
max_max_between <- mosaic_network(out2, index=3, sc=T, fun=max)
mean_mean_speed <- mosaic_network(out2, index=4, sc=T, fun=mean)
mean_dot_TA <- mosaic_network(out2, index=5, sc=T, fun=mean)

par(mfrow=c(2,2))
plot(mean_mean_degree, "Degree", main = "Degree")
plot(max_max_between, "Betweenness", main = "Betweeness")
plot(mean_mean_speed, "Speed", main = "Speed")
plot(mean_dot_TA, "Directionality", main = "Directionality")

# plot interactive connectivity map
r.plot <- max_max_between
crs(r.plot) <- '+init=epsg:32637' #EPSG code for UTM Zone 37N (https://epsg.io/32637)
crs(tt) <- '+init=epsg:32637'
mapview(r.plot)

#' These four layers are showing interpolated and mosaicked population-level network or 
#' movement properties. Again, these raster could be exported to be opened in ArcGIS using 
#' *writeRaster*. 
#' 
#' * QUESTION: What is the difference between the interpolated and non-interpolated rasters?
#' What are the pros and cons of each approach to displaying the data? Hint: Plot the 
#' interpolated raster with the GPS movement data.





