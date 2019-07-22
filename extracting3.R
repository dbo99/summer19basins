rm(list = ls())
#setwd("~/Documents/basins19")
setwd("~/R/proj/basins2019")
source("libs.r")

fcast_gps <- c("CachePutah", "RussianNapa", "Klamath" , "FeatherYuba",  "Tulare" ,"American", "SanJoaquin","SouthernCalifornia", 
               "EastSierra" , "NorthCoast", "CentralCoast" , "LowerSacramento" , "UpperSacramento","N_SanJoaquin", "Humboldt")

grouptoextract <- fcast_gps[3]

source("newzones2019.r")


#######################################################
######## elevation (15 x 15 m integer grid) ########
#######################################################

# # Elevation ------------------------------------------------------------
# ihabbs_elev_ned2010_int_m.tif - from RTI's 2016 compilation - Nad83 albers equal area conformic usgs

#{
 # setwd("~/Documents/basins19/data")
setwd("~/R/proj/basins2019/data")
  zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% 
   # filter(ForecastGr == grouptoextract ) %>%
    filter(Name %in% zones2019 ) %>%
   
    transmute(zone = Name, fcast_grp = ForecastGr, basin = Basin, geometry) %>%
   # filter(zone == "ICHC1HOF")
  shp_prj <- crs(zones)
  shp_prj
  zonenumid <- data.frame(c(1:length(zones$geometry)),as.character(zones$zone),stringsAsFactors = F) # for join
  colnames(zonenumid) <- c("zonenumid","zone") 
  elev_int_m <- raster("ihabbs_elev_ned2010_int_m.tif")
  crs(elev_int_m) <- shp_prj
  crs(elev_int_m)
  crs(shp_prj)
  
  #zones <- spTransform(zones, rastprj)
  beginCluster(n=2)  #library(snow)
  extracted <- extract(elev_int_m, zones, progress = 'text')
  endCluster() #library(snow)
  #extractedbu <- extracted
  
  extracted <- do.call(rbind, lapply(1:length(extracted), function(zonenumid) cbind(zonenumid, extracted[[zonenumid]])))
  #as_tibble(extracted)
  colnames(extracted) <- c("zonenumid","elev_m") # house keeping again
  #as_tibble(extracted)
  
  extracted <- data.frame(extracted)
  
  extracted <- inner_join(extracted,zonenumid,by="zonenumid") %>% transmute(zone, elev_m)# 
  #as_tibble(extracted)
  write_csv(extracted, paste0("elev_", grouptoextract ,".csv"))
  write_csv(extracted, paste0("elev_", grouptoextract ,"_bu.csv"))
  
#}

###################################
######## mean ann temp ###########
###################################

# # Temperature - prism new  ------------------------------------------------------------

{
  setwd("~/Documents/basins19/data")
filename <- "PRISM_tmean_30yr_normal_800mM2_annual_asc.asc"
  zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% 
    # filter(ForecastGr == grouptoextract ) %>%
    #filter(Name %in% zones2019 ) %>%
    transmute(zone = Name, fcast_grp = ForecastGr, basin = Basin, geometry)
  alb83_eacusgscrs_prj <- crs(zones)
  zonenumid <- data.frame(c(1:length(zones$geometry)),as.character(zones$zone),stringsAsFactors = F) # for join
  colnames(zonenumid) <- c("zonenumid","zone") 

  r <- raster(filename)
  #res(r)
  r <- projectRaster(r, crs = alb83_eacusgscrs_prj)
  #crs(r)
  #res(r)
  beginCluster(n=2)  #library(snow) - speeds up extraction
  extracted <- extract(r, zones, progress = 'text')
  endCluster() #library(snow)
 
  
  extracted <- do.call(rbind, lapply(1:length(extracted), function(zonenumid) cbind(zonenumid, extracted[[zonenumid]])))
  #as_tibble(extracted)
  colnames(extracted) <- c("zonenumid","t_c") # house keeping again
  #as_tibble(extracted)
  
  extracted <- data.frame(extracted)
  
  extracted <- inner_join(extracted,zonenumid,by="zonenumid") %>% transmute(zone, t_c)# 
  #as_tibble(extracted)
  write_csv(extracted, paste0(filename,"_extract.csv"))
  write_csv(extracted, paste0(filename,"_extract_bu.csv"))
  
}

  # # Temperature - prism ihabbs  ------------------------------------------------------------
  

  {
  setwd("~/Documents/basins19/data")
  filename <- "ihabbscap_Tann.tif"
  
  zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% 
    # filter(ForecastGr == grouptoextract ) %>%
    #filter(Name %in% zones2019 ) %>%
    transmute(zone = Name, fcast_grp = ForecastGr, basin = Basin, geometry)
  alb83_eacusgscrs_prj <- crs(zones)
  zonenumid <- data.frame(c(1:length(zones$geometry)),as.character(zones$zone),stringsAsFactors = F) # for join
  colnames(zonenumid) <- c("zonenumid","zone") 
  
  r <- raster(filename)
  #crs(r)
  r <- projectRaster(r, crs = alb83_eacusgscrs_prj) #appears same, but jic
  # plot(r) #quickcheck looks ok
  beginCluster(n=2)  #library(snow)
  extracted <- extract(r, zones, progress = 'text')
  endCluster() #library(snow)
  
  
  extracted <- do.call(rbind, lapply(1:length(extracted), function(zonenumid) cbind(zonenumid, extracted[[zonenumid]])))
  #as_tibble(extracted)
  colnames(extracted) <- c("zonenumid","t_c") # house keeping again
  #as_tibble(extracted)
  
  extracted <- data.frame(extracted)
  
  extracted <- inner_join(extracted,zonenumid,by="zonenumid") %>% transmute(zone, t_c)# 
  write_csv(extracted, paste0(filename,"_extract.csv"))
  write_csv(extracted, paste0(filename,"_extract_bu.csv"))
  
  }
  
  ###################################
  ######## mean ann precip ###########
  ###################################
  
  # # Precip - prism new  ------------------------------------------------------------
  
  {
  
  setwd("~/Documents/basins19/data")
  filename <- "PRISM_ppt_30yr_normal_800mM2_annual_asc.asc"
  zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% 
    # filter(ForecastGr == grouptoextract ) %>%
    #filter(Name %in% zones2019 ) %>%
    transmute(zone = Name, fcast_grp = ForecastGr, basin = Basin, geometry)
  alb83_eacusgscrs_prj <- crs(zones)
  zonenumid <- data.frame(c(1:length(zones$geometry)),as.character(zones$zone),stringsAsFactors = F) # for join
  colnames(zonenumid) <- c("zonenumid","zone") 
  
  r <- raster(filename)
  r <- projectRaster(r, crs = alb83_eacusgscrs_prj)
  #crs(r)
  beginCluster(n=2)  #library(snow)
  extracted <- extract(r, zones, progress = 'text')
  endCluster() #library(snow)
  
  
  extracted <- do.call(rbind, lapply(1:length(extracted), function(zonenumid) cbind(zonenumid, extracted[[zonenumid]])))
  #as_tibble(extracted)
  colnames(extracted) <- c("zonenumid","p_mm") # house keeping again
  #as_tibble(extracted)
  
  extracted <- data.frame(extracted)
  
  extracted <- inner_join(extracted,zonenumid,by="zonenumid") %>% transmute(zone, p_mm)# 
  write_csv(extracted, paste0(filename,"_extract.csv"))
  write_csv(extracted, paste0(filename,"_extract_bu.csv"))
  
  }
  
  # # Temperature - prism ihabbs  ------------------------------------------------------------
  
  
  {
  setwd("~/Documents/basins19/data")
  filename <- "ihabbscap_Pann.tif"
  
  zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% 
    # filter(ForecastGr == grouptoextract ) %>%
    #filter(Name %in% zones2019 ) %>%
    transmute(zone = Name, fcast_grp = ForecastGr, basin = Basin, geometry)
  alb83_eacusgscrs_prj <- crs(zones)
  zonenumid <- data.frame(c(1:length(zones$geometry)),as.character(zones$zone),stringsAsFactors = F) # for join
  colnames(zonenumid) <- c("zonenumid","zone") 
  
  r <- raster(filename)
  crs(r)
  r <- projectRaster(r, crs = alb83_eacusgscrs_prj) #appears same, but jic
  plot(r) #quickcheck looks ok
  beginCluster(n=2)  #library(snow)
  extracted <- extract(r, zones, progress = 'text')
  endCluster() #library(snow)
  
  
  extracted <- do.call(rbind, lapply(1:length(extracted), function(zonenumid) cbind(zonenumid, extracted[[zonenumid]])))
  #as_tibble(extracted)
  colnames(extracted) <- c("zonenumid","p_mm") # house keeping again
  #as_tibble(extracted)
  
  extracted <- data.frame(extracted)
  
  extracted <- inner_join(extracted,zonenumid,by="zonenumid") %>% transmute(zone, p_mm)# and now we merge the data tables to make nice looking dataset
  #as_tibble(extracted)
  write_csv(extracted, paste0(filename,"_extract.csv"))
  write_csv(extracted, paste0(filename,"_extract_bu.csv"))
  
  }
