rm(list = ls())
setwd("~/R/proj/basins2019/Shiny/shiny_mdntue/shiny_mdntue~")

    
    maptypes = c(#"Stamen.TonerLite", 
                 #"Stamen.Terrain", 
                 "Stamen.TopOSMRelief", 
                 "Esri.WorldTopoMap" , 
                 "Esri.WorldPhysical", 
                 "OpenTopoMap" ,
                 "NASAGIBS.ModisTerraTrueColorCR")
    
    grp <- c(  "p19p13_p_perdiff", "p19p13_t_perdiff","p19xlsoldercap_perdiff",
             "p19xlsoldestcap_perdiff", "p19_meanann_p_in", "square_mi" , "mean_ft", "usgs hydrography", "0.5 reflectivity") 
    
    allzones <- readOGR(".", "cnrfc_zones_07122019_83alb_wgs84") %>% st_as_sf() 
    
    
    csvdata <- read_csv("zone_means.csv")
    
    allzones <- inner_join(allzones, csvdata, by = "Name") %>% transmute(
      zone = Name, fcast_gp = ForecastGr, basin = Basin, lat_cent_x = lat_cent.x,
      lon_cent_y = lon_cent.x, square_mi, mean_m, mean_ft, p13_meanann_p_mm, p13_meanann_p_in, p13_meanann_t_c, 
      p13_meanann_t_f, p19_meanann_p_mm, p19_meanann_p_in, p19_meanann_t_c, p19_meanann_t_f, p19p13_p_perdiff,
      p19p13_t_perdiff, geometry)
    

    
    source("zones2019.r")
    
    oldercap_p <- read_csv("mapcompare_pf.csv")
    
    allzones <- inner_join(allzones, oldercap_p) %>% mutate(p19xlsoldercap_perdiff = (p19_meanann_p_in - cap_old)/p19_meanann_p_in * 100,
              p19xlsoldestcap_perdiff = (p19_meanann_p_in - cap_oldest)/p19_meanann_p_in * 100)
   
    
    zcol = colnames(allzones) %>% .[.!="geometry"]
    pal <-  mapviewPalette("mapviewSpectralColors")
    
    m <- mapview(allzones["p19p13_p_perdiff"], hide = TRUE, 
                 col.regions = pal(100), 
                 alpha.regions = 0.35,
                 map.types = maptypes,
                 popup = popupTable(allzones, zcol = zcol),
                 layer.name = "p19p13_p_perdiff") + 
      
      mapview(allzones["p19p13_t_perdiff"], hide = TRUE, 
              col.regions = pal(100), 
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19p13_t_perdiff") + 
      
      mapview(allzones["p19xlsoldercap_perdiff"],  hide = TRUE, 
              col.regions = pal(100), 
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19xlsoldercap_perdiff") + 
      
      mapview(allzones["p19xlsoldestcap_perdiff"],  hide = TRUE, 
              col.regions = pal(100), 
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19xlsoldestcap_perdiff") +
      
      mapview(allzones["p19_meanann_p_in"],  hide = TRUE, 
              col.regions = viridisLite::viridis, 
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19_meanann_p_in") +
      
      mapview(allzones["square_mi"],  hide = TRUE, 
              col.regions = viridisLite::viridis, 
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "square_mi") +
    
      mapview(allzones["mean_ft"],  hide = TRUE, 
              col.regions = viridisLite::viridis, 
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "mean_ft") 

    
    
    m@map = m@map %>% 
      
      addTiles() %>%
      #leaflet(height = "100%") %>%
      setView(-119.3, 38.4, zoom = 6)  %>%   
      
      addWMSTiles(group= grp[8], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
      
      addWMSTiles( group = grp[ 9],baseUrl = 
                     "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
                   #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                   layers = "nexrad-n0r-900913",
                   options = WMSTileOptions(format = "image/png", transparent = TRUE),
                   attribution = "Weather data  2012 IEM Nexrad")     %>%
      mapview:::mapViewLayersControl(names = grp) %>% hideGroup(grp[2]) %>% hideGroup(grp[3]) %>%
       hideGroup(grp[4]) %>% hideGroup(grp[5]) %>% hideGroup(grp[6]) %>% hideGroup(grp[7]) 

    m@map



