rm(list = ls()) 
setwd("~/R/proj/basins2019")
source("libs.r")
source("zones2019.r")

#ui <- dashboardPage(
#  dashboardHeader(),
#  dashboardSidebar(),
#  dashboardBody(
#    
#    tags$style(type = "text/css", "#mapplot {height: calc(100vh - 80px) !important;}"),
#    leafletOutput("mapplot"),
#    useShinyjs()
#  )
#)


#server <- function(input, output) {
#  addClass(selector = "body", class = "sidebar-collapse")
#  options(shiny.maxRequestSize=225*1024^2) 
#  
#  
#  output$mapplot <- renderLeaflet({

zones <- readOGR(".", "cnrfc_zones_07122019_wgs84") %>% st_as_sf()   


csvdata <- read_csv("basins2019_means.csv")

zones <- inner_join(zones, csvdata, by = "Name") %>% 
         mutate(Basin = Basin.x) %>%
        select(-Basin.x) %>% 
         select(-Basin.y) %>%
         mutate(PrismDiff_mm = MeanAnnPrismPrcp81_10_v07122019_mm - 
                  MeanAnnPrismPrcp81_10v2016cap_mm)  %>%
         mutate(PrismDiff_percent = round(PrismDiff_mm/MeanAnnPrismPrcp81_10v2016cap_mm*100, 1))

newzones <- zones %>% filter(Name %in% zones2019)
#colnames(csvdata)
maptypes = c(
  "Stamen.TopOSMRelief", 
  "Stamen.TerrainBackground",
  "NASAGIBS.ModisTerraTrueColorCR",
  #"ESRI.WorldImagery",
  "Esri.WorldPhysical",  
  "OpenTopoMap" )



grp <- c(    "usgs hydrography",   "0.5 reflectivity") #,"hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
#"mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 


lopt <- labelOptions(noHide = TRUE,
                     direction = 'top',
                     textOnly = TRUE)
zcol = colnames(zones) %>% .[.!="geometry"] %>% .[.!="Shape_Area"]  %>% .[.!="Shape_Leng"] 
zcol
colnames(zones)
m <- mapview(zones["PrismDiff_percent"], #burst = TRUE, hide = TRUE, 
             col.regions = viridisLite::viridis, 
            # cex = zones$PrismDiff_percent,
             alpha.regions = 0.3,
             map.types = maptypes,
             popup = popupTable(zones, zcol = zcol),
                           
             
             layer.name = "PrismDiff_percent") +
  
  mapview(zones["PrismDiff_mm"], #burst = TRUE, hide = TRUE, 
          col.regions = viridisLite::viridis, 
          #cex = zones$PrismDiff_percent,
          alpha.regions = 0.3,
          map.types = maptypes,
          popup = popupTable(zones, zcol = zcol),
          
          
          layer.name = "PrismDiff_mm") +
  
  mapview(zones["MeanAnnPrismPrcp81_10v2016cap_mm"], #burst = TRUE, hide = TRUE, 
          col.regions = viridisLite::viridis, 
          #cex = zones$PrismDiff_percent,
          alpha.regions = 0.3,
          map.types = maptypes,
          popup = popupTable(zones, zcol = zcol),
          
          
          layer.name = "MeanAnnPrismPrcp81_10v2016cap_mm") +
  
  mapview(zones["MeanAnnPrismPrcp81_10_v07122019_mm"], #burst = TRUE, hide = TRUE, 
          col.regions = viridisLite::viridis, 
          #cex = zones$PrismDiff_percent,
          alpha.regions = 0.3,
          map.types = maptypes,
          popup = popupTable(zones, zcol = zcol),
          
          
          layer.name = "MeanAnnPrismPrcp81_10_v07122019_mm") +
  
  mapview(zones["MeanElev_ft"], #burst = TRUE, hide = TRUE, 
          col.regions = viridisLite::viridis, 
          #cex = zones$PrismDiff_percent,
          alpha.regions = 0.3,
          map.types = maptypes,
          popup = popupTable(zones, zcol = zcol),
          
          
          layer.name = "MeanElev_ft") +
  
  mapview(newzones["MeanAnnPrismPrcp81_10v2016cap_mm"], #burst = TRUE, hide = TRUE, 
          col.regions = viridisLite::viridis, 
          #cex = zones$PrismDiff_percent,
          alpha.regions = 0.3,
          map.types = maptypes,
          popup = popupTable(newzones, zcol = zcol),
          
          
          layer.name = "newzones") 
  
  

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
