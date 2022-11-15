####################### Cluster Analysis in R ###########################


## Install packages
install.packages("sf")
install.packages("tmap")
install.packages("tidyverse")
install.packages("rgeoda")


## Load packages
library(sf)
library(tmap)
library(tidyverse)
library(rgeoda)


# Set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PH GIS/Labs/Lab7")


# Read-in shp
covid_zip <- st_read("NYC_zip_COVID19.shp")


# Map death rayes
tm_shape(covid_zip) +
  tm_polygons(col = "rate", 
              style = "jenks",
              n = 5) 


# Queen 1st order weights matrix 
queen_w <- queen_weights(covid_zip, order = 1)
summary(queen_w)


# Local Moran's I
lisa <- local_moran(queen_w, covid_zip["rate"],
                    permutations = 9999)


# What's contained under the "lisa" object?
names(lisa)


# Get cluster column and join it to zip shp
covid_zip$cluster <- as.factor(lisa$GetClusterIndicators())
str(covid_zip)


# Add labels to clusters
levels(covid_zip$cluster) <- lisa$GetLabels()


# Map clusters
tm_shape(covid_zip) +
  tm_fill("cluster") 


# GeoDa colors
lisa_colors <- lisa_colors(lisa)


# Map using GeoDa colors
tm_shape(covid_zip) +
  tm_polygons(col = "cluster", 
              palette =  c("#eeeeee", 
                      "#FF0000",
                      "#0000FF",
                      "#a7adf9",
                      "#f4ada8", 
                      "#464646",
                      "#999999"),
              alpha = .8,
              border.col = "black",
              border.alpha = .9)


# Recode clusters using tidyverse (dyplr)
covid_zip %>% 
  mutate(cluster = recode_factor(.x = cluster,
                                  `Undefined` = "Not significant",
                                  `Isolated` = "Not significant")) -> covid_zip


# Map new categories 
tm_shape(covid_zip) +
  tm_polygons(col = "cluster", 
              palette = c("#eeeeee", 
                      "#FF0000",
                      "#0000FF",
                      "#a7adf9",
                      "#f4ada8"),
              alpha = .8,
              title = "Clusters",
              border.col = "black",
              border.alpha = .9) 


# Select just the clusters you want to map
covid_zip %>% 
  filter(cluster %in% c("High-High", "Low-Low")) ->  covid_zip_subset


# Map selected clusters
tm_shape(covid_zip) +
  tm_fill(col = "#eeeeee") +
tm_shape(covid_zip_subset) +
  tm_fill(col = "cluster",
          palette =  c("#eeeeee", 
                         "#FF0000",
                         "#0000FF",
                         "#a7adf9",
                         "#f4ada8"),
          alpha = .8)











