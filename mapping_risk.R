#### Lab 2 Part 2: Mapping Risk with R ####

# Three steps to minimize errors

# Step 1: install the packages (need to install only once)
install.packages("tidyverse")    # data management/visualization (several packages)
install.packages("sf")           # GIS package
install.packages("tmap")         # mapping package
install.packages("tmaptools")    # additional tools for tmap 
install.packages("cowplot")      # combine plots


# Step 2: load packages 
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(cowplot)


# Step 3: Set your working directory 
setwd("~/Documents/Graduate Courses/3_Fall 2022 Semester/Public Health GIS/Lab 2/Lab2")


# Read-in shapefile 
zip_nyc <- st_read("MODZCTA_2010.shp")

str(zip_nyc)

tm_shape(zip_nyc) +
  tm_polygons()


# Read-in data
covid <- read_csv("data-by-modzcta.csv")
str(covid)
summary(covid)


# Join COVID data with sf object
covid_zip <- merge(zip_nyc, covid, 
                        by.x = "MODZCTA",
                        by.y = "modified_ZCTA")


# Generate rate and RR
covid_zip %>% 
  mutate(rate = 100000*(death_count/population),
         expected = population*(35253/8336817),
         RR = death_count/expected) -> covid_zip_rr


# Death Rate map 

# Step 1: set colors 
tm_shape(covid_zip_rr) +
  tm_polygons(col = "rate",
              style = "quantile",
              n = 5,
              palette = "YlOrBr",
              border.col = "black",
              title = "Rate per 100k")


# Step 2: adjust legend 
tm_shape(covid_zip_rr) +
  tm_polygons(col = "rate",
              style = "quantile",
              n = 5,
              palette = "YlOrBr",
              border.col = "black",
              title = "Rate per 100k") +
  tm_legend(legend.position = c(0.12, 0.47),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1)


# Step 3: add title
tm_shape(covid_zip_rr) +
  tm_polygons(col = "rate",
              style = "quantile",
              n = 5,
              palette = "YlOrBr",
              border.col = "black",
              title = "Rate per 100k") +
  tm_legend(legend.position = c(0.12, 0.47),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "COVID19 Death Rate \nper 100,000",
            title.size = 1.4,                                                  
            title.position = c(0.12, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE)


# Step 4: North arrow and scale 
tm_shape(covid_zip_rr) +
  tm_polygons(col = "rate",
              style = "quantile",
              n = 5,
              palette = "YlOrBr",
              border.col = "black",
              title = "Rate per 100k") +
  tm_legend(legend.position = c(0.12, 0.47),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "COVID19 Death Rate \nper 100,000",
            title.size = 1.4,                                                  
            title.position = c(0.12, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_compass(type = "arrow",
             position = c(0.23, 0.05)) +
  tm_scale_bar(position = c(0.32, 0.04))


# Step 5: Save in object
tm_shape(covid_zip_rr) +
  tm_polygons(col = "rate",
              style = "quantile",
              n = 5,
              palette = "YlOrBr",
              border.col = "black",
              title = "Rate per 100k") +
  tm_legend(legend.position = c(0.12, 0.47),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "COVID19 Death Rate \nper 100,000",
            title.size = 1.4,                                                  
            title.position = c(0.12, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_compass(type = "arrow",
             position = c(0.23, 0.05)) +
  tm_scale_bar(position = c(0.32, 0.04)) -> death_rates



# Excess Risk map 

# Step 1: get colors
tm_shape(covid_zip_rr) +
  tm_polygons(col = "RR",
              style = "quantile",
              n = 5,
              palette = "RdBu",
              border.col = "black",
              title = "Excess Risk")


# Step 2: invert ramp 
tm_shape(covid_zip_rr) +
  tm_polygons(col = "RR",
              style = "quantile",
              n = 5,
              palette = "-RdBu",  # invert color ramp  
              border.col = "black",
              title = "Excess Risk")


# Step 3: add custom breaks
tm_shape(covid_zip_rr) +
  tm_polygons(col = "RR",
              n = 5,
              breaks = c(0, 
                         0.50, 
                         0.90, 
                         1.10, 
                         1.50, 
                         2.91),
              palette = "-RdBu",  
              border.col = "black",
              title = "Excess Risk")


# Step 4: add remaining elements
tm_shape(covid_zip_rr) +
  tm_polygons(col = "RR",
              n = 5,
              breaks = c(0, 
                         0.50, 
                         0.90, 
                         1.10, 
                         1.50, 
                         2.91),
              palette = "-RdBu",  
              border.col = "black",
              title = "Excess Risk") +
  tm_legend(legend.position = c(0.12, 0.47),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "Excess Risk of Dying \nfrom COVID19",
            title.size = 1.4,                                                  
            title.position = c(0.12, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_credits("Author: Sarah Forrest \nSource: NYC Health \nDate: September 14, 2022",
                        size = .7,
                        position = c(.66,.04))


# Step 5: assign to an object
tm_shape(covid_zip_rr) +
  tm_polygons(col = "RR",
              n = 5,
              breaks = c(0, 
                         0.50, 
                         0.90, 
                         1.10, 
                         1.50, 
                         2.91),
              palette = "-RdBu",  
              border.col = "black",
              title = "Excess Risk") +
  tm_legend(legend.position = c(0.12, 0.47),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "Excess Risk of Dying \nfrom COVID19",
            title.size = 1.4,                                                  
            title.position = c(0.12, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_credits("Author: Sarah Forrest \nSource: NYC Health \nDate: September 14, 2022",
             size = .7,
             position = c(.66,.04)) -> excess_risk


# Turn tmap objects into cowplot objects
tmap_grob(death_rates) -> rates_cow
tmap_grob(excess_risk) -> excess_cow


# Basic combined figure with cowplot

plot_row <- plot_grid(rates_cow, excess_cow)

# Write title
title <- ggdraw() + 
  draw_label(
    "New York City COVID-19 Deaths By Zip Code, 2020 - 2021",
    fontfamily = "Avenir",
    x = 0,
    hjust = 0,
    size = 18) +
  theme( plot.margin = margin(0, 0, 0, 25))


# Finish it off
plot_grid(title,
          plot_row,
          ncol = 1,
          rel_heights = c(.2,1)) 


plot_grid(title,
          plot_row,
          ncol = 1,
          rel_heights = c(.2,1)) -> combined_cow


# Save map 
ggsave(combined_cow,
         width = 18, 
         height = 10,
         file = "risk.jpeg")










