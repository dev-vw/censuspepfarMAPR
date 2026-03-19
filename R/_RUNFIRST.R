library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(ggplot2)

# load shapefile data structure
load("../pepfar-census_poly-match/outputs/objects/shp_lsts.rda")

# load facilities point layer
facilities <- st_read("data/pepfar-facilities-shp/PEPFAR_Facilities.shp")

# helps avoid "Loop 0 is not valid: Edge X has duplicate vertex with edge Y"
sf_use_s2(FALSE)

