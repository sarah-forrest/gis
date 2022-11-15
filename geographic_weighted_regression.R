#################### Geographically-Weighted Regression ########################


# Install packages
install.packages("sf")
install.packages("tmap")
install.packages("tidyverse")
install.packages("spgwr") # gwr modeling
install.packages("jtools") # regression visualizations
install.packages("tmaptools")
install.packages("ggstance")


# Load packages
library(sf)
library(tmap)
library(tidyverse)
library(spgwr)
library(jtools)
library(tmaptools)
library(ggstance)


# Set working directory
setwd("~/Documents/Graduate Courses/3_Fall 2022 Semester/Public Health GIS/Lab 10/Lab10")


# Read shps
data <- st_read("NYC_zip_COVID.shp")


# Scatter plots 

# Essential Workers
data %>% 
  ggplot(aes(x = per_ssn,  y = deth_rt, color = CovidZip_2)) +
  geom_point() + 
  labs(title = "Percent Essential Worker and COVID-19 Death Rate",
       x = "Percent Essential Worker",
       y = "Death Rate",
       color = "Borough") +
  theme_minimal()


# Add linear fit and facet
data %>% 
  ggplot(aes(x = per_ssn,  y = deth_rt, color = CovidZip_2)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Percent Essential Worker and COVID-19 Death Rate",
       x = "Percent Essential Worker",
       y = "Death Rate",
       color = "Borough") +
  facet_wrap(~CovidZip_2)  +
  theme_minimal() +
  theme(legend.position = "bottom")


# Percent 65yrs and above 
data %>% 
  ggplot(aes(x = pr_pp65,  y = deth_rt, color = CovidZip_2 )) +
  geom_point() + 
  labs(title = "Percent over 65yrs and COVID-19 Death Rate",
       x = "Percent over 65yrs",
       y = "Death Rate",
       color = "Borough") +
  theme_minimal()


# Add linear fit and facet
data %>% 
  ggplot(aes(x = pr_pp65,  y = deth_rt, color = CovidZip_2 )) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  labs(title = "Percent over 65yrs and COVID-19 Death Rate",
       x = "Percent over 65yrs",
       y = "Death Rate",
       color = "Borough") +
  facet_wrap(~CovidZip_2)  +
  theme_minimal() +
  theme(legend.position = "bottom")


# Multiple Regression

mreg <- lm(deth_rt ~ pr_pp65 + per_ssn + pr_hghd + med_inc  + pr_pbtr,
           data = data) 


summary(mreg)
AIC(mreg)


plot_summs(mreg, scale = TRUE, inner_ci_level = .95)


# Geographically-Weighted Regression 

# Fixed Bandwidth 
class(data)
data <- as_Spatial(data)


fixed_bw <- gwr.sel(deth_rt ~ pr_pp65 + per_ssn + pr_hghd + med_inc  + pr_pbtr,
               data = data, 
               adapt = FALSE, 
               gweight = gwr.Gauss,
               verbose = TRUE)


fbw_gwr <- gwr(deth_rt ~ pr_pp65 + per_ssn + pr_hghd + med_inc  + pr_pbtr,
               data = data, 
               bandwidth = fixed_bw,
               gweight = gwr.Gauss,
               hatmatrix = TRUE, 
               se.fit = TRUE)

fbw_gwr

names(fbw_gwr)


# Grab results 
results <- fbw_gwr$SDF

names(results)
summary(results)


# Mapping Results

# Local R Square
tm_shape(results) +
  tm_polygons(col = "localR2",
              palette = "magma",
              style = "cont",
              border.col = "black",
              lwd = .5,
              title = "R-Square") +
  tm_legend(legend.position = c(0.18, 0.50),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "Local R-Square",
            title.size = 2,                                                  
            title.position = c(0.12, .90),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_credits("Author: Sarah Forrest \nSource: NYC Health \nDate: November 15, 2022",
             size = .7,
             position = c(.66,.04)) -> r2

tmap_save(r2, 
          dpi = 300,
          width = 6,
          height = 6,
          filename = "local_r2.png")


# Percent 65yrs and over 
tm_shape(results) +
  tm_polygons(col = "pr_pp65",
              palette = "magma",
              style = "cont",
              border.col = "black",
              lwd = .5,
              title = "Coefficient") +
  tm_legend(legend.position = c(0.18, 0.50),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "Percent 65 \nand Over",
            title.size = 2,                                                  
            title.position = c(0.12, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_credits("Author: Sarah Forrest \nSource: NYC Health \nDate: November 15, 2022",
             size = .7,
             position = c(.66,.04)) -> coef_65yrs

tmap_save(coef_65yrs, 
          dpi = 300,
          width = 6,
          height = 6,
          filename = "65yr_coef.png")


# Significance of 65yrs and over 

results$tval_65 <- results$pr_pp65/results$pr_pp65_se

results$t_65_cat <- cut(results$tval_65,
                             breaks = c(min(results$tval_65), -2, 2, max(results$tval_65)),
                             labels = c("sig","nonsig", "sig"))

tm_shape(results) + 
  tm_fill(col = "t_65_cat", 
          title = "Statistical Significant",
          legend.hist = TRUE) 


# Percent Essential Workers
tm_shape(results) +
  tm_polygons(col = "per_ssn",
              palette = "magma",
              style = "cont",
              border.col = "black",
              lwd = .5,
              title = "Coefficient") +
  tm_legend(legend.position = c(0.18, 0.50),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "Percent Essential \nWorkers",
            title.size = 2,                                                  
            title.position = c(0.05, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_credits("Author: Sarah Forrest \nSource: NYC Health \nDate: November 15, 2022",
             size = .7,
             position = c(.66,.04)) -> essen


tmap_save(essen, 
          dpi = 300,
          width = 6,
          height = 6,
          filename = "essential_coef.png")


# Significance Essential Workers

results$tval_essen <- results$per_ssn/results$per_ssn_se

results$t_essen_cat <- cut(results$tval_essen,
                        breaks = c(min(results$tval_essen), -2, 2, max(results$tval_essen)),
                        labels = c("sig","nonsig", "sig"))

tm_shape(results) + 
  tm_fill(col = "t_essen_cat", 
          title = "Statistical Significant",
          legend.hist = TRUE) 


# Percent High School Diploma
tm_shape(results) +
  tm_polygons(col = "pr_hghd",
              palette = "magma",
              style = "cont",
              midpoint = 0,
              border.col = "black",
              lwd = .5,
              title = "Coefficient") +
  tm_legend(legend.position = c(0.18, 0.50),
            legend.text.fontfamily = "Avenir",
            legend.text.size = 1) +
  tm_layout(title = "Percent HS Diploma",
            title.size = 2,                                                  
            title.position = c(0.05, .85),  
            title.fontfamily = "Avenir",
            frame = FALSE) +
  tm_credits("Author: Sarah Forrest \nSource: NYC Health \nDate: November 15, 2022",
             size = .7,
             position = c(.66,.04))


# Significance of High School Diploma
results$tval_hs <- results$pr_hghd/results$pr_hghd_se

results$t_hs_cat <- cut(results$tval_hs,
                           breaks = c(min(results$tval_hs), -2, 2, max(results$tval_hs)),
                           labels = c("sig","nonsig", "sig"))

tm_shape(results) + 
  tm_fill(col = "t_hs_cat", 
          title = "Statistical Significant",
          legend.hist = TRUE) 




