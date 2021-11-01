library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(here)
library(sf)
library(dplyr)
library(stringr)
# library(tictoc)
library(shinycssloaders)
library(leaflet.multiopacity)
library(raster)
library(shinyjs)
library(zip)
#### Load misc functions
source(here::here("R", "app_functions.R"))

# Dataframe with conversion table for raster values. 
# TODO add to documentation
mydf <- data.frame(
  ID = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
  category = c("00", "11", "12", "13", "21", "22", "23", "31", "32", "33")
)

#### Output data is on milkun archive
bivariate_names <- here("data", "bivariate") %>% 
  list.files(full.names = TRUE, pattern = "leaflet")

data_points <- st_read(here::here("data", "species", "alpine_salamander_points.gpkg"), quiet = TRUE)

update = today(format = "human")

tiles <- c(providers$OpenStreetMap, providers$Stamen.Terrain, 
           providers$Esri.WorldImagery)
