########################## Animated Maps ###########################


## Install packages
install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
install.packages("tidyverse")
install.packages("gifski")


## Load packages
library(sf)
library(tidyverse)
library(tmap)
library(gifski) # create gifs


## Set working directory
setwd("~/Documents/Graduate Courses/3_Fall 2022 Semester/Public Health GIS/Lab 9/Lab9")


## Read shps
pa <- st_read("PA_county.shp")
head(pa)


## Change format from "wide" to 

pa %>% 
  pivot_longer(cols = disabl_14:disabl_20,
               names_to = "year",
               names_prefix = "disabl_",
               values_to = "perc_disabl") -> pa_long


## Animated map
tm_shape(pa_long) +
  tm_polygons(col = "perc_disabl",
              style = "fisher",
              palette = "PuBu",
              alpha = .8,
              n = 5,
              border.col = "white",
              border.alpha = .8) +
  tm_facets(along = "year") + # maps same variable for every year 
  tm_layout(title = "Disability Status from 2014 to 2020",
            title.size = 2,
            title.fontfamily = "Avenir",
            title.position = c(.18,.9),
            legend.show = FALSE) +
  tm_text("perc_disabl",    
          size = "AREA",
          remove.overlap = TRUE) -> animation 
  
  
## create animation   
tmap_animation(animation,
               filename = "animated_map.gif",  
               delay = 140,
               width = 800,
               height = 600)

  

































