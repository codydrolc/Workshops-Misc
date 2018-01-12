################################################################################
##### Graph Google Location History

# Note: This example only works if your phone has location tracking turned on
# and you are using Google's location services.

# 1) Download "Location History" from https://takeout.google.com/settings/takeout
# Do not download all data; it will take a substantial amount of time. 
# 2) Save the .zip file in your desired location
# 3) Extract the "Location History.json" file
# The location data are not completely accurate; you may notice random points 
# in the ocean or other places you have never been. Subset your data  based on
# the "accuracy" variable to remove erroneous points. 

# Set directory with "Location History.json" file
setwd("C:/generic/directory/please/set")

# Install packages
install.packages(c("jsonline", "lubridate", "ggplot2", "ggmap", "maps"),
  dependencies = TRUE)

# Load required packages
library(jsonlite); library(lubridate); library(ggplot2)
library(ggmap); library(maps)

##### Prepare data
# Read raw JSON data
raw <- fromJSON("Location History.json") # this may take several minutes

# Extract locations
lc <- raw$locations
rm(raw) # remove object; it will be large

# Convert posix milliseconds into readable time: year-month-day time
lc$time <- as.POSIXct(as.numeric(lc$timestampMs)/1000, origin = "1970-01-01")

# Extract year
lc$year <- year(lc$time)

# Longitude and latitude from E7 to GPS coordinates
# We are just moving the decimal point at this step
lc$lat <- lc$latitudeE7/1e7
lc$lon <- lc$longitudeE7/1e7

# Remove inaccurate points
# I am going to define these as points where this accuracy is greater than
# 1000 meters. You can adjust the threshold has desired. 
ggplot() + geom_histogram(data = lc, aes(accuracy)) # view basic distribution
lcsub <- subset(lc, accuracy <= 1000)

##### Map points
# There are other things that can be done with these data, but I'm just going
# to move into how to actually map your location. The following example is for
# the United States but anywhere in the world works--just change the location.

# Basic United States map
usmap <- map_data("state")

# Map points to ggplot2 object
usmap <- ggplot() + 
  geom_polygon(data = usmap, aes(x = long, y = lat, group = group),
  color = "white")
usmap # view map

# Add points
usmap + geom_point(data = lcsub, aes(lon, lat), alpha = 0.5, color = "gold3") +
  labs(x = "", y = "") + theme(axis.text = element_blank(), 
  axis.ticks = element_blank()) # removing unnecessary labels

# Or, color by year (much more interesting)
usmap + geom_point(data = lcsub, aes(lon, lat, color = as.factor(year)), alpha = 0.5) +
  labs(x = "", y = "", color = "Year") + theme(axis.text = element_blank(), 
  axis.ticks = element_blank())

# Google United States map
gusmap <- get_map(location = "united states", zoom = 4, maptype = "terrain",
  source = "google", color = "color") # pulling from Google's API
ggmap(gusmap) # view map, adjust zoom as desired (zoom ranges from 3 to 21)

# Plot GPS coordinates (be patient: a lot of data needs to be plotted)
ggmap(gusmap) + geom_point(data = lcsub, aes(lon, lat, color = as.factor(year)), alpha = 0.5) +
  labs(x = "", y = "", color = "Year") + theme(axis.text = element_blank(), 
  axis.ticks = element_blank())
