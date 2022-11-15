#### Lab 1 Part 2: Acquiring and Making Administrative Data Spatial ####


# Three steps to minimize errors


# Step 1: install the packages (need to install only once)
install.packages("tidycensus")   # access US census data
install.packages("tidyverse")    # data management/visualization (several packages)
install.packages("sf")           # GIS package
install.packages("tmap")         # mapping package
install.packages("tmaptools")    # additional tools for tmap 
install.packages("shiny")
install.packages("shinyjs")


# Step 2: load packages 
library(tidycensus)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(shiny)
library(shinyjs)


# Step 3: Set your working directory 
setwd("~/Documents/Graduate Courses/3_Fall 2022 Semester/Public Health GIS/Lab 1")


### Get census data from tidycensus 

# Set API key
census_api_key("1a8824382c3c969cf1c9e208ac0685009dc78a1d")

# Get a list of all variables available from census
census_data <- load_variables(year = 2020, 
                              dataset = "acs5", 
                              cache = TRUE)
View(census_data)


# Define your query
nj_data <- get_acs(state = "nj",
               year = 2020,
               geography = "county",
               variables = c("S2701_C01_001E",
                             "S2701_C02_001E"),
               geometry = TRUE,
               survey = "acs5",
               output = "wide")

class(nj_data)


# Use dplyr (from tidyverse) to rename variables and calculate percent insured

nj_data %>% 
  rename(total_pop = S2701_C01_001E,
         no_insured = S2701_C02_001E) %>% 
  mutate(p_insured = 100*(no_insured/total_pop),
         county = str_extract(NAME, "(\\w+)")) -> nj_insured


# Use tmap to build out map

# Basic map - just geometry 
tm_shape(nj_insured) +    # data 
  tm_polygons() 


# Basic map - add color 
tm_shape(nj_insured) +    
  tm_polygons(col = "p_insured") 


# Add classification method and number of classes
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4) 


# See tmap tools to see options
  palette_explorer()


# Add classification method and number of classes
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues") 


# Change stroke color and transparency
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black")  # also accepts hex color codes (https://htmlcolorcodes.com/) 


# Rename legend and adjust it 
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + # name of legend 
  tm_legend(legend.position = c("left", "center")) # basic adjustment 


# More fine tune adjustment of legend 
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c("0.08", ".47")) # advanced adjustment 


# Add title and other elements
tm_shape(nj_insured) +  
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c("0.08", ".47")) +
  tm_layout(title = "Percent of population who are \n  insured, New Jersey 2020",  # title
            title.size = 1.5,                          # font size                             
            title.position = c(.2, .95),               # title position                         
            inner.margins = c(0.10, 0.10, 0.1, 0.01),  # margins (bottom, left, top, right)                          
            frame = FALSE,                             # get rid of frame                          
            legend.title.size = .9)                    # legend size                          
  

# Add scale bar and north arrow 
  tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c(0.08, .47)) +
  tm_layout(title = "Percent of population who are \n  insured, New Jersey 2020",  
            title.size = 1.5,                                                      
            title.position = c(.2, .95),                                         
            inner.margins = c(0.10, 0.10, 0.1, 0.01),                            
            frame = FALSE,                                                       
            legend.title.size = .9) +        
  tm_compass(type = "arrow",                             # north arrow        
             position = c(0.01,0.16),                      
             size = 1.2) +                             
  tm_scale_bar(position = c(.08, 0.14),                  # scale bar 
               size = 0.6) 


# Add information on author, date, and text 
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c(0.08, .47)) +
  tm_layout(title = "Percent of population who are \n  insured, New Jersey 2020",  
            title.size = 1.5,                                                      
            title.position = c(.2, .95),                                         
            inner.margins = c(0.10, 0.10, 0.1, 0.01),                            
            frame = FALSE,                                                       
            legend.title.size = .9) +        
  tm_compass(type = "arrow",                             
             position = c(0.01,0.16),                     
             size = 1.2) +                             
  tm_scale_bar(position = c(.08, 0.14),                   
               size = 0.6) +
  tm_credits("Author: Sarah Forrest \nSource: US Census \nDate:",
             size = .7,
             position = c(.024,.07)) +
  tm_credits("Counties labeled  \nfall in the lower \n25% of the distribution",
             size = .7,
             fontface = "bold",
             position = c(.68,.18))


# Add label 
# Add information on author, date, and text 
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c(0.08, .47)) +
  tm_layout(title = "Percent of population who are \n  insured, New Jersey 2020",  
            title.size = 1.5,                                                      
            title.position = c(.2, .95),                                         
            inner.margins = c(0.10, 0.10, 0.1, 0.01),                            
            frame = FALSE,                                                       
            legend.title.size = .9) +        
  tm_compass(type = "arrow",                             
             position = c(0.01,0.16),                     
             size = 1.2) +                             
  tm_scale_bar(position = c(.08, 0.14),                   
               size = 0.6) +
  tm_credits("Author: Sarah Forrest \nSource: US Census \nDate:",
             size = .7,
             position = c(.024,.07)) +
  tm_credits("Counties labeled  \nfall in the lower \n25% of the distribution",
             size = .7,
             fontface = "bold",
             position = c(.68,.18)) +
  tm_text(text  = "county", size = "AREA")


# Make a filter 
nj_insured %>% 
  filter(p_insured < 92.27) -> nj_filtered

tm_shape(nj_filtered) +
  tm_polygons()


# Label using filter NJ layer 
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c(0.08, .47)) +
  tm_layout(title = "Percent of population who are \n  insured, New Jersey 2020",  
            title.size = 1.5,                                                      
            title.position = c(.2, .95),                                         
            inner.margins = c(0.10, 0.10, 0.1, 0.01),                            
            frame = FALSE,                                                       
            legend.title.size = .9) +        
  tm_compass(type = "arrow",                             
             position = c(0.01,0.16),                     
             size = 1.2) +                             
  tm_scale_bar(position =  c(.08, 0.14),                   
               size = 0.6) +
  tm_credits("Author: Sarah Forrest \nSource: US Census \nDate:",
             size = .7,
             position = c(.024,.07)) +
  tm_credits("Counties labeled  \nfall in the lower \n25% of the distribution",
             size = .7,
             fontface = "bold",
             position = c(.68,.18)) +
  tm_shape(nj_filtered) +                               # add filtered layer
  tm_text(text  = "county", size = "AREA")              # add labels


# Make a filter 
nj_insured %>% 
  filter(p_insured < 92.27) -> nj_filtered

tm_shape(nj_filtered) +
  tm_polygons()


# Assign final map to an object 
tm_shape(nj_insured) +   
  tm_polygons(col = "p_insured",
              style = "quantile",
              n = 4,
              palette = "Blues",
              border.col = "black",
              title = "Percent Insured") + 
  tm_legend(legend.position = c(0.08, .47)) +
  tm_layout(title = "Percent of population who are \n  insured, New Jersey 2020",  
            title.size = 1.5,                                                      
            title.position = c(.2, .95),                                         
            inner.margins = c(0.10, 0.10, 0.1, 0.01),                            
            frame = FALSE,                                                       
            legend.title.size = .9) +        
  tm_compass(type = "arrow",                             
             position = c(0.01,0.16),                     
             size = 1.2) +                             
  tm_scale_bar(position = c(.08, 0.14),                   
               size = 0.6) +
  tm_credits("Author: Sarah Forrest \nSource: US Census \nDate: September 09, 2022",
             size = .7,
             position = c(.024,.07)) +
  tm_credits("Counties labeled  \nfall in the lower \n25% of the distribution",
             size = .7,
             fontface = "bold",
             position = c(.68,.18)) +
  tm_shape(nj_filtered) +                               
  tm_text(text  = "county", size = "AREA") -> final_map             

final_map 


# Save map in wd jpg/tiff/pgn

tmap_save(final_map, 
          dpi = 300,
          width = 6,
          height = 10,
          filename = "NJ_Insured_Map_R.png")


# Save sf dataframe as shapefile
st_write(nj_insured,"NJ_Insured_Map_R.shp")









