


{
rm(list= ls())
setwd("~/R/proj/basins2019/area_elev")
source("libs.r")

setwd("~/R/proj/basins2019/area_elev/csv/zones")
files <- list.files(pattern = "*.csv$") 
newbasins <- gsub(".csv", "", files) 



#csvnewbasins <- data.frame(newbasins)
#write_csv(csvnewbasins, "newbasins.csv")



newbasins <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "zone")
newbasins$zone <- gsub(".csv", "", newbasins$zone) 
setwd("~/R/proj/basins2019/area_elev/csv/groups")
fcastgrps <- read_csv("newbasins_fcastgroup.csv")

newbasins <- newbasins %>%
                mutate(basin =  substr(newbasins$zone,1,nchar(newbasins$zone)-3)) %>%
                mutate(elevgrid_ft = elevgrid_meter * 3.28084)


newbasins <- right_join(newbasins, fcastgrps)
unique(newbasins$fcast_group)
nb_fy  <- newbasins %>% filter(fcast_group == "FeathYuba")
nb_kl  <- newbasins %>% filter(fcast_group == "Klamath")
nb_sj  <- newbasins %>% filter(fcast_group == "SanJoaquin")
nb_nsj <- newbasins %>% filter(fcast_group == "N_SanJoaquin")
nb_rn  <- newbasins %>% filter(fcast_group == "RussianNapa")
nb_tl  <- newbasins %>% filter(fcast_group == "Tulare")
nb_am  <- newbasins %>% filter(fcast_group == "American")
nb_sc  <- newbasins %>% filter(fcast_group == "SoCal")


}
ggplot(newbasins, aes(x = elevgrid_ft, y = zone,
                      color = basin,
                      fill = zone)) + geom_density_ridges(show.legend = F,
                        rel_min_height = 0.01,
                        alpha = 0.3) + facet_wrap(~fcast_group, ncol = 4) #+ 
                        #scale_y_discrete(sec.axis = dup_axis())

setwd("~/R/proj/basins2019/area_elev")
ggsave("newbasins.jpg", dpi = 300, width = 18, height = 13, units = "in") 


roundDown <- function(x,to=10)
{
  to*(x%/%from + as.logical(x%%from))
}



roundUp <- function(x,to=10)
{
  to*(x%/%to + as.logical(x%%to))
}


tc <- newbasins %>% filter(zone == "TCCC1HUF") %>% select(elevgrid_ft) 
tc <- newbasins$elevgrid_ft

lower <- min(tc$elevgrid_ft) %>% roundDown(10)
upper <- max(tc$elevgrid_ft) %>% roundUp(10)



br <- seq(lower,upper, by=10)

ranges = paste(head(br,-1), br[-1], sep = " - ")
freq   = hist(tc, breaks=br, include.lowest=TRUE, plot=FALSE)











#summer19newbasins <- read_excel("basins2019_gisdataforcalibration_r.xlsx") 
                   
#areaelevs <- summer19newbasins %>%
  #excel_sheets()# %>%
  #set_names() %>% 
  #map_df(~ read_excel(path = path, sheet = .x, range = "A5:F15"), .id = "sheet")

elev <- tccc1hlf$count
hist(elev)

h = hist(elev) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$count/sum(h$count)*100
plot(h,freq=FALSE)

