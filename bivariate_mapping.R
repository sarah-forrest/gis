####################### Bivariate Mapping in R ###########################


## Install packages
install.packages("sf")
install.packages("tmap")
install.packages("tidyverse")
install.packages("rgeoda")
install.packages("biscale")  # Bivariate maps
install.packages("cowplot")  # Combine plots
install.packages("RcolorBrewer")  # Access color palettes 


## Load packages
library(sf)
library(tmap)
library(tidyverse)
library(rgeoda)
library(biscale)
library(cowplot)
library(RColorBrewer)


# Set working directory
setwd("~/Library/CloudStorage/OneDrive-Personal/PH GIS/Labs/Lab8")


# Read-in shp
covid_zip <- st_read("NYC_covid19_zip.shp")


###### Small multiples map  ##########

tm_shape(covid_zip) +
  tm_polygons(col = c("r_death", "per_pov"),
              style = "quantile",
              n = 4,
              title = c("Death Rate", "Percent Poor"),
              palette = "Purples",
              border.col = "black") 


###### Multi layer map  ##########

tm_shape(covid_zip) +
  tm_polygons(col =  "per_pov",
              style = "quantile",
              n = 4,
              title = "Percent Poor",
              palette = "Purples",
              border.col = "black") +
tm_shape(covid_zip) +
  tm_dots(col = "black",
          size = "r_death",
          style = "quantile",
          title = "Death Rate",
          palette = "Purples",
          border.col = "black") 


###### Bivariate Moran's I  ##########

# Queen 1st order weights matrix 
queen_w <- queen_weights(covid_zip, order = 1)
summary(queen_w)


# Bivariate Local Moran's I
bi_lisa <- local_bimoran(queen_w, covid_zip[c("per_pov", "r_death")],
                    permutations = 9999)


# What's contained under the "bi_lisa" object?
names(bi_lisa)


# Get cluster column and join it to zip shp
covid_zip$cluster <- as.factor(bi_lisa$GetClusterIndicators())
str(covid_zip)


# Add labels to clusters
levels(covid_zip$cluster) <- bi_lisa$GetLabels()


# Map clusters
tm_shape(covid_zip) +
  tm_fill("cluster") 


# We need the GeoDa colors
lisa_colors <- lisa_colors(bi_lisa)


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
              palette =  c("#eeeeee", 
                      "#FF0000",
                      "#0000FF",
                      "#a7adf9",
                      "#f4ada8"),
              alpha = .8,
              title = "Clusters",
              border.col = "black",
              border.alpha = .9) 


###### Bivariate Map  ##########

bi_zip <- bi_class(covid_zip, 
                   x = per_pov, 
                   y = r_death, 
                   style = "quantile",
                   dim = 3)


# Basic map
ggplot() +
  geom_sf(data = bi_zip, aes(fill = bi_class), 
          color = "white", 
          size = 0.1, 
          show.legend = FALSE)

# Add color palette 

ggplot() +
  geom_sf(data = bi_zip, aes(fill = bi_class), 
          color = "white", 
          size = 0.1, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(title = "COVID-19 Death Rate and Poverty",
       subtitle = "New York City, 2020 - 2021",
       caption = "Sarah Forrest \nSources: NYC DOH, US Census \nDate:") +
  theme_map() -> bi_map


# Create legend 

bi_legend(pal = "DkBlue",
          dim = 3,
          xlab = "Percent Poor",
          ylab =  "COVID-19 Deaths",
          size = 8) -> legend 


# Combine map and legend 
  ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.30, 0.50, 0.18, 0.18) 


###### Principal Component Analysis  ##########

# Go from sf and dataframe
covid_zip %>% 
  as_data_frame() %>% 
  select(-NYCZipCode) -> pca_data

  
# PCA for columns 2 - 7 
pca <- prcomp(pca_data[,2:7],
              center = TRUE,
              scale. = TRUE)


# Attach predictions to original data

covid_zip$PC1 <- pca$x[,1]


# Mapping with ggplot

ggplot(data = covid_zip) +
  geom_sf(aes(fill = PC1),  color = "white", size  = .15) +
  scale_fill_distiller(name = "Disadvantage",
                        palette = "Purples",
                        direction = 1) +
  labs(title = "Disadvatange Index",
       subtitle = "New York City, 2022") +
  theme_map()




