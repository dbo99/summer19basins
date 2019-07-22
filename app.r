library(tidyverse)
library(sf)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(leaflet)
library(mapview)


#source("df_build.r")


ui <- 
  
  shinyUI(fluidPage(
    useShinyjs(),
    br(),
    
    sidebarLayout(sidebarPanel(
      
      fluidRow(

        selectizeInput(
        "zone", "Zone", choices = unique(df$zone), 
        selected = c("CEGC1LLF", "CEGC1LUF"), 
        multiple = TRUE)),
      
      fluidRow(
        
        
       column(6,checkboxGroupInput("dsrce", "dsrce",                
                          choices = c("ned2010", "prsm13", "prsm19"), 
                          selected = "ned2010", inline = F)),
       
        
           column(4, checkboxGroupInput("param", "parameter",                
                       choices = c("elev", "p", "t"), selected = "elev", inline = F))
      
               ),
      
      fluidRow(

        column(4,radioButtons("free_scale", "y scale",                
                              choices = c("free", "fixed"), selected = "free", inline = T))),
      
      radioButtons("units", "units",                
                   choices = c("metric", "english"), selected = "english", inline = T),
      
      
      radioButtons('color', 'color', c("none",  "zone", "param", "dsrce" ), selected  = "dsrce", inline = T ),
      
      radioButtons('linetype', 'line type (eg dashed)', c("none",  "zone", "param", "dsrce"), selected  = "none", inline = T ),
      
      
      radioButtons('facet_row', 'split by row',
                   
                   c(none='.',   "zone", "param", "dsrce"), inline = T,
                   selected = "none"),
      
      radioButtons('facet_col', 'split by column',
                   
                   c(none='.',  "zone", "param", "dsrce" ), inline = T,
                   selected = "zone")
      
 
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
  
  output$reg_plot <- renderPlot({
    
      df <- df  %>%  filter(zone %in% input$zone)   %>% 
                     filter(param %in% input$param) %>%
                     filter(dsrce %in% input$dsrce) 
                   
      if (input$units == "metric")  

      df <- df %>% mutate(unit = ifelse(unit == "in", "mm", unit)) %>%
        mutate(unit = ifelse(unit == "fahr", "cels", unit)) %>%
        mutate(unit = ifelse(unit == "ft", "m", unit)) %>%
        mutate(val = ifelse(unit == "mm", val * 25.4, val )) %>%
        mutate(val = ifelse(unit == "cels", (val - 32) * 5/9, val)) %>%
        mutate(val = ifelse(unit == "m", val * 0.3048, val))
      

     # df <- df %>% mutate(basin = substr(zone,1,nchar(zone)-3))
      
    p <- ggplot(df, aes(val)) +   labs(y = "proportion of total", x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + stat_ecdf(pad = FALSE) + coord_flip() 
      
    
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype=input$linetype)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    if (facets != '. ~ .' &&  input$free_scale == "free"  )
      p <- p + facet_grid(facets, scales = 'free_y') + theme(strip.text.y = element_text(angle = 0)) 
    
    if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
      p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
    
    
    print(p)
    
  }) 
  
  
  output$plotly_plot <- renderPlotly({
    
    #input$goButton
    
    startdowy <- dowy %>% filter(date == input$start_date)
    startdowy <- startdowy$dowy
    enddowy <- dowy %>% filter(date == input$end_date) 
    enddowy <- enddowy$dowy
    
    if (input$resctricttodowy == "no" && input$entirebasin == "yes")
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(param %in% input$parameter)
    
    if (input$resctricttodowy == "yes" && input$entirebasin == "yes")
      
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(param %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy)
    
    if (input$resctricttodowy == "no" && input$entirebasin == "no")
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(param %in% input$parameter) %>% filter(basin_zone == "Entire Basin")
    
    if (input$resctricttodowy == "yes" && input$entirebasin == "no")
      
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(param %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy) %>%
      filter(basin_zone == "Entire Basin")
    
    df <- df %>% mutate(wy = as.factor(wy))
    p <- ggplot(df, aes_string(x=input$x, y= "numval")) + geom_point(size = 0.5) + geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + theme_bw(base_size=18)
    
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype=input$linetype)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    if (facets != '. ~ .' &&  input$free_scale == "free"  )
      p <- p + facet_grid(facets, scales = 'free_y') + theme(strip.text.y = element_text(angle = 0)) 
    
    if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
      p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
    
    p <- ggplotly(p)
    
    print(p)
    
  })
  
  output$mapplot <- renderLeaflet({
    
    # input$goButton
    
    df_map <- df %>% filter(basin_zone == "Entire Basin", date == input$map_date, p_unit %in% input$map_param)
    df_mapcolrange <- df %>% filter(basin_zone == "Entire Basin", p_unit %in% input$map_param)
    viridmax <- max(df_mapcolrange$numval)
    viridmin <- min(df_mapcolrange$numval)
    
    # Convert spatialpolydf to an sf object
    sf_ebasin_kml  <- ebasin_kml  %>%  st_as_sf() %>% transmute(Name, geometry)  %>%
      left_join(df_map, by = c("Name" = "nwscode"))
    
    sf_ebasin_kml_hlite  <- sf_ebasin_kml  %>%  
      filter(nws_basin_code %in% input$nws_basin_code) 
    
    maptypes = c("Stamen.TonerLite", "Stamen.Terrain", "Stamen.TopOSMRelief", "Esri.WorldTopoMap" , "Esri.WorldPhysical",  "OpenTopoMap" ,
                 "NASAGIBS.ModisTerraSnowCover", "NASAGIBS.ModisTerraTrueColorCR", "NASAGIBS.ModisTerraBands367CR")
    
    grp <- c(    "usgs hydrography",   "0.5 reflectivity") #,"hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
    #"mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 
    
    
    m <- mapview(sf_ebasin_kml["numval"], burst = TRUE, hide = TRUE, col.regions = viridisLite::viridis, 
                 alpha.regions = 0.4,  map.types = maptypes,
                 popup = popupTable(sf_ebasin_kml, zcol = c("nws_basin_code", "date", "numval", "param")), 
                 layer.name = "nohrsc daily data")   +
      mapview(sf_ebasin_kml_hlite["numval"], color = "red", 
              alpha.regions = 0.0, 
              popup = popupTable(sf_ebasin_kml_hlite, zcol = c("nws_basin_code", "date", "numval", "param")),
              layer.name = "selected basin(s' ) outline", legend = FALSE) 
    
    m@map = m@map %>% 
      
      addTiles() %>%
      setView(-119.6, 38.05, zoom = 7)  %>%   
      
      addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
      
      
      
      addWMSTiles( group = grp[2],baseUrl = 
                     "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
                   #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                   layers = "nexrad-n0r-900913",
                   options = WMSTileOptions(format = "image/png", transparent = TRUE),
                   attribution = "Weather data  2012 IEM Nexrad") %>%
      
      # addWMSTiles( group = grp[3],baseUrl = 
      #                "https://gibs.earthdata.nasa.gov/twms/epsg4326/best/twms.cgi", 
      #              #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
      #              layers = "0",
      #              options = WMSTileOptions(format = "image/png", transparent = TRUE),
      #              attribution = "NASA GIBS imagery") %>%
      #
      
      
      
    mapview:::mapViewLayersControl(names = grp) %>% #hideGroup(grp[1]) #%>% 
      hideGroup(grp[2]) 
    
    
    
    m@map
  })
  
}

shinyApp(ui, server)
