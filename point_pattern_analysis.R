################## Lab 4: Spatial Point Pattern Analysis #######################


# Step 1: Install packages
install.packages("maptools")
install.packages("spatstat")
install.packages("sf")


# Step 2: Load packages
library(maptools)
library(spatstat)
library(sf)


# Step 3: Set working directory 
setwd("~/Documents/Graduate Courses/3_Fall 2022 Semester/Public Health GIS/Midterm/Midterm materials")


# Read in shp
PA <- st_read("PA_county.shp")
plot(PA["COUNTYFP"])


# Set as owin - observation window 
PA_window <- as.owin(PA)


# Read in fracking wells
fracking_wells <- st_read("fracking_wells.shp")


# Set frackingwells as a point pattern proccess
fracking_wells_ppp <- as.ppp(fracking_wells)


# Combine Spatial Points with Observation Window 

fracking_wells_PA <- ppp(fracking_wells_ppp$x,
                  fracking_wells_ppp$y,
                  window = PA_window)
plot(fracking_wells_PA)


# Create heatmap of TRI sites
plot(density(death_soho),
     main = "Kernel density map of cholera deaths")


# Change the buffer to 50 meters
plot(density(death_soho, sigma = 50),
     main = "Kernel density map of cholera deaths")

# Quadrad Test
qt <- quadrat.test(death_soho, nx = 6, ny = 10)
qt
plot(qt, 
     cex = .7,
     main = "Quadrat Test")


# Run L function with 10 simulations and 95% confidence interval (rank=2) on all points (global=T)
L <- envelope(death_soho, 
              Lest, 
              nsim = 20,
              rank = 2, 
              global = T)


# From the results we can see that at very small distances between points (x-axis) there is an 
# immediate spike in density (y-axis)


plot(L, 
     main = "Global Clustering Results", 
     ylab = "Density Function",
     xlab = "Distance") 

















