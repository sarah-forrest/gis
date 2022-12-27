############################# Interactive Maps  ################################

# Install packages
install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
install.packages("tidyverse")


# Load packages
library(sf)
library(tidyverse)
library(tmap)
library(tmaptools)




# Set your working directory
setwd("~/Documents/Graduate Courses/3_Fall 2022 Semester/Public Health GIS/Lab 11/Lab11")

# Read in shapefiles
deaths <- st_read("NYC_covid19_zip.shp")

clusters <-  st_read("clusters_death.shp")

sites <- st_read("NYC_COVID19_vacc_sites.shp")

names(deaths)

deaths %>% 
  rename("per.65yrs" = X..over.65y,
         "per.HS" = X..HS.degre,
         "per.poverty" = X..Poverty,
         "per.PP" = X..Public.T,
         "infection" =  Infection.) -> deaths

names(deaths)

palette_explorer()

# Sample map in "Plot View"
tmap_mode("plot")

tm_shape(deaths) +
  tm_polygons(col = "Death.Rate",
          palette = "GnBu",
          alpha = .8,
          style = "quantile",
          n = 4,
          border.col = "white",
          lwd = .5,
          title = "Death Rate")



# Interactive model
tmap_mode("view")

tm_shape(deaths) +
  tm_polygons(col = "Death.Rate",
              palette = "GnBu",
              alpha = .8,
              style = "quantile",
              n = 4,
              border.col = "white",
              lwd = .5,
              title = "Death Rate")

# Add additional variables 
tm_shape(deaths) +
  tm_polygons(col = "Death.Rate",
              palette = "GnBu",
              alpha = .8,
              style = "quantile",
              n = 4,
              border.col = "white",
              lwd = .5,
              title = "Death Rate",
              interactive = TRUE,
              popup.vars = c("per.65yrs",  "per.HS", "per.poverty", "per.PP", "Death.Rate"))



# Add the clusters and sites
tm_shape(deaths) +
  tm_polygons(col = "Death.Rate",
              palette = "GnBu",
              alpha = .8,
              style = "quantile",
              n = 4,
              border.col = "white",
              lwd = .5,
              title = "Death Rate",
              interactive = TRUE,
              popup.vars = c("per.65yrs",  "per.HS", "per.poverty", "per.PP", "Death.Rate")) +
  tm_shape(clusters) + 
    tm_polygons(col = "cluster",
                palette =  c("#FF0000",
                            "#0000FF"),
                title = "Clusters",
                alpha = .4) +
  tm_shape(sites) +
    tm_dots(col = "white",
            border.col = "red",
            title = "Vaccination Sites")
    

# Save as object

tm_shape(deaths) +
  tm_polygons(col = "Death.Rate",
              palette = "GnBu",
              alpha = .8,
              style = "quantile",
              n = 4,
              border.col = "white",
              lwd = .5,
              title = "Death Rate",
              interactive = TRUE,
              popup.vars = c("per.65yrs",  "per.HS", "per.poverty", "per.PP", "Death.Rate")) +
  tm_shape(clusters) + 
  tm_polygons(col = "cluster",
              palette =  c("#FF0000",
                          "#0000FF"),
              title = "Clusters",
              alpha = .4) +
  tm_shape(sites) +
  tm_dots(col = "white",
          border.col = "red",
          title = "Vaccination Sites") -> web_map


# Save interactive maps 
tmap_save(web_map, "web_map.html")




























