rm(list = ls())
#setwd("~/Documents/basins19")
setwd("~/R/proj/basins2019/Shiny")
source("libs.r")


maptypes = c("Stamen.TonerLite", "Stamen.Terrain", "Stamen.TopOSMRelief", "Esri.WorldTopoMap" , "Esri.WorldPhysical",  "OpenTopoMap" ,
              "NASAGIBS.ModisTerraTrueColorCR")

grp <- c("usgs hydrography",   "0.5 reflectivity") 

allzones <- readOGR(".", "cnrfc_zones_07122019_83alb_wgs84") %>% st_as_sf() 


csvdata <- read_csv("zone_means.csv")

allzones <- inner_join(allzones, csvdata, by = "Name") %>% transmute(
            zone = Name, fcast_gp = ForecastGr, basin = Basin, lat_cent_x = lat_cent.x,
            lon_cent_y = lon_cent.x, square_mi, mean_m, mean_ft, p13_meanann_p_mm, p13_meanann_p_in, p13_meanann_t_c, 
            p13_meanann_t_f, p19_meanann_p_mm, p19_meanann_p_in, p19_meanann_t_c, p19_meanann_t_f, p19p13_p_perdiff,
            p19p13_t_perdiff, geometry)

source("zones2019.r")
oldercap_p <- read_csv("mapcompare_pf.csv")

allzones <- inner_join(allzones, oldercap_p) %>% mutate(p19xlsoldercap_perdiff = (p19_meanann_p_in - cap_old)/p19_meanann_p_in *100,
                                                        p19xlsoldestcap_perfiff = (p19_meanann_p_in - cap_oldest)/p19_meanann_p_in *100)

newzones <- allzones %>% filter(zone %in% zones2019 )



#selectedzones  <- allzones  %>%   filter(zone %in% input$zone) 

zcol = colnames(allzones) %>% .[.!="geometry"]
pal <-  mapviewPalette("mapviewSpectralColors")

m <- mapview(allzones["p19p13_p_perdiff"], #burst = TRUE, hide = TRUE, 
             col.regions = pal(100), 
             # cex = zones$PrismDiff_percent,
             alpha.regions = 0.5,
             map.types = maptypes,
             popup = popupTable(allzones, zcol = zcol),
             layer.name = "p19p13_p_perdiff") + 
  
  mapview(allzones["p19p13_t_perdiff"], #burst = TRUE, hide = TRUE, 
          col.regions = rev(pal(100)), 
          # cex = zones$PrismDiff_percent,
          alpha.regions = 0.5,
          map.types = maptypes,
          popup = popupTable(allzones, zcol = zcol),
          layer.name = "p19p13_t_perdiff") + 
  
  mapview(newzones["p19p13_p_perdiff"], #burst = TRUE, hide = TRUE, 
          col.regions = pal(100), 
          # cex = zones$PrismDiff_percent,
          alpha.regions = 0.9,
          map.types = maptypes,
          popup = popupTable(newzones, zcol = zcol),
          layer.name = "p19p13_p_perdiff_new") +
  
  mapview(allzones["p19xlsoldercap_perdiff"], #burst = TRUE, hide = TRUE, 
          col.regions = rev(pal(100)), 
          # cex = zones$PrismDiff_percent,
          alpha.regions = 0.5,
          map.types = maptypes,
          popup = popupTable(allzones, zcol = zcol),
          layer.name = "p19xlsoldercap_perdiff") + 
  
  mapview(newzones["p19xlsoldestcap_perfiff"], #burst = TRUE, hide = TRUE, 
          col.regions = pal(100), 
          # cex = zones$PrismDiff_percent,
          alpha.regions = 0.9,
          map.types = maptypes,
          popup = popupTable(newzones, zcol = zcol),
          layer.name = "p19xlsoldestcap_perfiff") +
  
  mapview(selectedzones["p19p13_p_perdiff"], color = "red", 
          alpha.regions = 0.0, 
          popup = popupTable(selectedzones, zcol = zcol),
          layer.name = "selected basin(s' ) outline", legend = FALSE) 


m@map = m@map %>% 
  
  addTiles() %>%
  #leaflet(height = "100%") %>%
  setView(-119.6, 38.05, zoom = 5)  %>%   
  
  addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
              options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
  
  addWMSTiles( group = grp[2],baseUrl = 
                 "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
               #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
               layers = "nexrad-n0r-900913",
               options = WMSTileOptions(format = "image/png", transparent = TRUE),
               attribution = "Weather data  2012 IEM Nexrad")     %>%
  mapview:::mapViewLayersControl(names = grp) %>% hideGroup(grp[2]) #%>% 
#hideGroup(grp[2]) 

#m@map <- m@map %>% addStaticLabels(m@map, data = zones, label = zones$PrismDiff_percent, labelOptions = lopt) %>% 
#  setView(-119.6, 38.05, zoom = 6.5) 

#})
m@map
#saveWidget(m, file="m.html")
#mapshot(m, url = "m.html")
#  }
#  )}
#
#shinyApp(ui, server)



