# Filename
# Author: Gijs van Lith
# Date: 7-11-2013
# Description: Script creates a sample of 30 temperature points for values in and around Kenya, 
#   then calculates the median, mean, and s.d, and then visualises this in a map.
 
rm()
setwd("D:\\Rcourse\\exercises")
getwd()
datdir <- file.path("data")
datdir <- "data"

## Needed Packages
library(rgdal) # reading shapefiles and raster data
library(rgeos) # vector manipulations
library(maptools) # mapping
library(spdep) # useful spatial stat functions
library(spatstat) # functions for generating random points
library(raster) # raster data analysis
library(rasta) 

# Check if up to date.  
update.packages(checkBuilt = TRUE, ask = FALSE)

# Download and unzip the Kenya shapefile

download.file("http://rasta.r-forge.r-project.org/kenyashape.zip",file.path(datdir, "kenyashape.zip"))
unzip(file.path(datdir, "kenyashape.zip"), exdir = datdir)
kenya <- readOGR(dsn = datdir, layer = "kenya")


# extract the data of the kenya shapefile

dsdat <- as(kenya, "data.frame")
kenya$new <- 1:nrow(dsdat)

# create a SpatialPointsDataFrame with 30 random samples point locationsin and around Kenya
#   and add their temperature values
win <- extent(kenya)
dran <- runifpoint(n = 30, win = as.vector(win))
dp <- as.data.frame(dran)

# Import the temperature data for the sampled points
filepath <- system.file("extdata", "anom.2000.03.tiff", package ="rasta")
g <- raster(filepath)
dp$values<-extract(g$anom.2000.03, dp)
dsp <- SpatialPointsDataFrame(coords = dp[, c("x","y")], data = data.frame("values" = dp$values))
dsp@proj4string <- kenya@proj4string

# get statistics of the sampled points
standard_deviation <- sd(x=dsp$values, na.rm = FALSE)
mean_ <- mean(x=dsp$values, na.rm=FALSE)
medi <- median(x=dsp$values, na.rm=FALSE)

# perform the overlay of the datapoints and the kenya province map
dsdat <- over(kenya, dsp, fn = mean)
inds <- row.names(dsdat)
kenya@data[inds, "pntvals"] <- dsdat

# crop the raster, plot the result with added median and s.d

# arg <- list(at=c(300,200,100), labels=c("Low","Med.","High"))
# plot(gc, xlab="Latitude", ylab="Longitude",axis.args= arg)

gc <- crop(g, kenya)
plot(gc, xlab="Latitude", ylab="Longitude")
plot(kenya, add = TRUE)
plot(dsp, add = TRUE,col = "blue" )

mtext(side = 3, line = 1, "Map of random sample points of Temperature Anomalies in Kenya in Deg. Celsius", cex = 1.2)
mtext(side = 1, line = 5, paste("median of sampled values is",medi ), cex = 1.2,adj=0)
mtext(side = 1, line = 4, paste("s.d of sampled values is",standard_deviation ), cex = 1.2,adj=0)
mtext(side = 1, line = 3, paste("mean of sampled values is",mean_ ), cex = 1.2, adj=0,)
