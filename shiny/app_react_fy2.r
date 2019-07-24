source("shiny_dfbuild_react.R")

ui <- 
  
  shinyUI(fluidPage(
    useShinyjs(),
    br(),
    
    sidebarLayout(sidebarPanel(
      
      fluidRow(

        selectizeInput(
        "zone", "zone", choices = unique(p13$zone), 
        selected = c("CEGC1LLF", "CEGC1LUF"), 
        multiple = TRUE)),
      
      fluidRow(
        
       column(6,checkboxGroupInput("dsrce", "dsrce (ned: only new zones)",                
                          choices = c("ned2010", "prsm13", "prsm19"), 
                          selected = "ned2010", inline = F)),
       
           column(4, checkboxGroupInput("param", "parameter",                
                       choices = c("elev", "p", "t"), selected = "elev", inline = F))
      
               ),
      
      #fluidRow(

       radioButtons("free_scale", "y",                
                             choices = c("free", "fixed"), selected = "fixed", inline = T),
      
    #  radioButtons("units", "units",                
    #               choices = c("metric", "english"), selected = "english", inline = T),
    #  
      
      radioButtons('color', 'color', c("none",  "zone", "basin", "param", "dsrce" ), selected  = "none", inline = T ),
      
      radioButtons('linetype', 'line type (eg dashed)', c("none",  "zone", "basin","param", "dsrce"), selected  = "none", inline = T ),
      
      
      radioButtons('facet_row', 'split by column',
                   
                   c(none = '.',   "zone", "basin",  "param", "dsrce"), inline = T,
                   selected = "zone"),
      
      radioButtons('facet_col', 'split by row',
                   
                   c(none = '.',  "zone", "basin","param", "dsrce" ), inline = T,
                   selected = '.')
      
 
    ),
    
    mainPanel(
      
      tabsetPanel(position=c("right"),
                  
                  tabPanel(strong("static cdf"), 
                           br(),
                           plotOutput("reg_plot",  height = "750px")) ,
                  
                 tabPanel(strong("interactive cdf"), 
                          br(),
                          plotlyOutput("plotly_plot",  height = "750px")) ,
                  
                  
                  # tabPanel(strong("map1"), 
                  # br(),
                  # leafletOutput("mapplot",  height = "750px")) ,
                  
                  tabPanel(strong("map"), 
                           br(),
                           leafletOutput("mapplot",  height = "750px")) )))
  ) 
  )

########################
#### server.r
########################


server <- function(input, output) {
  
  options(shiny.maxRequestSize=225*1024^2) 
  
  
  # this might be the reactive your question was about?

  
  data <- reactive({
    
     df %>%
      
      filter(param %in% input$param, zone %in% input$zone, dsrce %in% input$dsrce) %>%
      bind_rows({elev %>% mutate(param = "elev", dsrce = "ned2010") %>%
      filter(param %in% input$param, zone %in% input$zone, dsrce %in% input$dsrce) %>%
              uncount(count) %>% mutate(basin = substr(zone,1,nchar(zone)-3)) 
    
     #  if (input$units == "metric")  
     #    
     #    df %>% mutate(unit = ifelse(unit == "in", "mm", unit)) %>%
     #    mutate(unit = ifelse(unit == "fahr", "cels", unit)) %>%
     #    mutate(unit = ifelse(unit == "ft", "m", unit)) %>%
     #    mutate(val = ifelse(unit == "mm", val * 25.4, val )) %>%
     #    mutate(val = ifelse(unit == "cels", (val - 32) * 5/9, val)) %>%
     #    mutate(val = ifelse(unit == "m", val * 0.3048, val))  
     
      })
   
  })

  
  output$reg_plot <- renderPlot({
    
    print(input$param)
    print(input$zone)
    print(input$dsrce)
    print(data())
    
    
    p <- ggplot(data(), aes(val)) +   labs(y = "proportion of total", x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + 
      scale_x_continuous(sec.axis = dup_axis(name = NULL))  + 
      stat_ecdf(pad = FALSE)  + coord_flip()
      
    
    if (input$color != 'none')
      p <- p + aes_string(color = input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype = input$linetype)
    
    facets <- paste0(input$facet_col, '~', input$facet_row)
    
   if (facets != '. ~ .' &&  input$free_scale == "free"  )
     #if (input$free_scale == "free"  )
     p <- p + facet_grid(facets, scales = "free") + theme(strip.text.y = element_text(angle = 0)) 
   
   if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
     p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
   
    
    print(p)
    
  }) 
  
 output$plotly_plot <- renderPlotly({
   
 
   
   (p <- ggplot(data(), aes(val) ) + 
       stat_ecdf(pad = FALSE) + coord_flip()) +  #facet_wrap(~zone, nrow = 1)) +
     scale_x_continuous(sec.axis = dup_axis(name = NULL)) + 
     scale_y_continuous(sec.axis = dup_axis(name = NULL))  
   
   if (input$color != 'none')
     p <- p + aes_string(color = input$color)
   
   if (input$linetype != 'none')
     p <- p + aes_string(linetype = input$linetype)
   
   facets <- paste0(input$facet_col, '~', input$facet_row)
   
   if (facets != '. ~ .' &&  input$free_scale == "free"  )
     #if (input$free_scale == "free"  )
     p <- p + facet_grid(facets, scales = "free") + theme(strip.text.y = element_text(angle = 0)) 
   
   if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
     p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
   
  # data <- data[order(data$val), ]  
   
   p <- ggplotly(p) 
   
  
   p
   #print(p)
   
  }) 
  
  
  output$mapplot <- renderLeaflet({

    
    maptypes = c(#"Stamen.TonerLite", 
                 #"Stamen.Terrain", 
                 "Stamen.TopOSMRelief", 
                 "Esri.WorldTopoMap" , 
                 "Esri.WorldPhysical", 
                 "OpenTopoMap" ,
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
    
    
   # selectedzones  <- allzones  %>%   filter(zone %in% input$zone) 
    
    zcol = colnames(allzones) %>% .[.!="geometry"]
    pal <-  mapviewPalette("mapviewSpectralColors")
    
    m <- mapview(allzones["p19p13_p_perdiff"], #burst = TRUE, hide = TRUE, 
                 col.regions = pal(100), 
                 # cex = zones$PrismDiff_percent,
                 alpha.regions = 0.35,
                 map.types = maptypes,
                 popup = popupTable(allzones, zcol = zcol),
                 layer.name = "p19p13_p_perdiff") + 
      
      mapview(allzones["p19p13_t_perdiff"], #burst = TRUE, hide = TRUE, 
              col.regions = rev(pal(100)), 
              # cex = zones$PrismDiff_percent,
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19p13_t_perdiff") + 
      
      mapview(newzones["basin"], color = "red", #burst = TRUE, hide = TRUE, 
              col.regions = pal(100), 
              # cex = zones$PrismDiff_percent,
              alpha.regions = 0.15,
              map.types = maptypes,
              popup = popupTable(newzones, zcol = zcol),
              layer.name = "new zones") +
      
      
      mapview(allzones["p19xlsoldercap_perdiff"], #burst = TRUE, hide = TRUE, 
              col.regions = rev(pal(100)), 
              # cex = zones$PrismDiff_percent,
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19xlsoldercap_perdiff") + 
      
      mapview(allzones["p19xlsoldestcap_perfiff"], #burst = TRUE, hide = TRUE, 
              col.regions = pal(100), 
              # cex = zones$PrismDiff_percent,
              alpha.regions = 0.35,
              map.types = maptypes,
              popup = popupTable(allzones, zcol = zcol),
              layer.name = "p19xlsoldestcap_perfiff")# +
      
    # mapview(selectedzones["basin"], color = "red", 
    #         alpha.regions = 0.0, 
    #         popup = popupTable(selectedzones, zcol = zcol),
    #         layer.name = "selected basin(s') outline", legend = FALSE) 
    
    
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

    m@map

    
  })
  
}

shinyApp(ui, server)
