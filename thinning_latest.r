rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rgdal)
library(sf)
library(tidyverse)
library(rmapshaper)


# ------------------------------------------------------
## basins ------------------------------------------
# -------------------------------------------------------

{
  keep <- 0.1
basins84 <- readOGR(".", "cnrfc_basins_09192019_wgs84") 

#plot(basins84)

basins_thin <- ms_simplify(basins84, keep = keep)
plot(basins_thin)


writeOGR(obj = basins_thin, dsn="tempdir", layer = paste0("cnrfc_basins_09192019_wgs84_thin_", keep, "_ret"),
                           driver = "ESRI Shapefile") 
}

#----------------------------------------------
## zones --------------------------------
## ---------------------------------------------

{
  
keep <- 0.3
zones84 <- readOGR(".", "cnrfc_zones_09192019_wgs84") 
  
#plot(zones84)
  
zones_thin <- ms_simplify(zones84, keep_shapes = TRUE, keep = keep) #keep_shapes for multi-part polygons

  
  
writeOGR(obj = zones_thin, dsn="tempdir", layer = paste0("cnrfc_zones_09192019_wgs84_thin_", keep, "_ret"),
           driver = "ESRI Shapefile") 
}


