
# Hijmas Lydholm combo  ----------------------------------------------

#setwd("~/GISdata/Elevation/forhome")
rm(list = ls())
setwd("~/Documents/basins19")
source("libs.r")

fcast_gps <- c("CachePutah", "RussianNapa", "Klamath" , "FeatherYuba",  "Tulare" ,"American", "SouthernCalifornia", 
               "EastSierra" , "NorthCoast", "CentralCoast" , "LowerSacramento" , "UpperSacramento","SanJoaquin",
               "N_SanJoaquin", "Humboldt")

grouptoextract <- fcast_gps[3]

# # Extractor ------------------------------------------------------------

{
setwd("~/Documents/basins19/data")
zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% 
  filter(ForecastGr == grouptoextract ) %>% transmute(zone = Name, fcast_grp = ForecastGr, basin = Basin, geometry)
zonenumid <- data.frame(c(1:length(zones$geometry)),as.character(zones$zone),stringsAsFactors = F) # for join
colnames(zonenumid) <- c("zonenumid","zone") 
elev_int_m <- raster("ihabbs_elev_ned2010_int_m.tif")


extracted <- extract(elev_int_m, zones, progress = 'text')
extractedbu <- extracted

extracted <- do.call(rbind, lapply(1:length(extracted), function(zonenumid) cbind(zonenumid, extracted[[zonenumid]])))
as_tibble(extracted)
colnames(extracted) <- c("zonenumid","elev_m") # house keeping again
as_tibble(extracted)

extracted <- data.frame(extracted)

extracted <- inner_join(extracted,zonenumid,by="zonenumid") %>% transmute(zone, elev_m)# and now we merge the data tables to make nice looking dataset
as_tibble(extracted)
write_csv(extracted, paste0(grouptoextract ,".csv"))
write_csv(extracted, paste0(grouptoextract ,"_bu.csv"))

}
  

