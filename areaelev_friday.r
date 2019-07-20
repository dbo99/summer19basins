setwd("~/R/proj/basins2019/area_elev")
source("libs.r")
# thinking clearly ## workflow :

# clip every zone, save each dem,


#{
rm(list = ls())
set.seed(1)
gridft_1 <- rnorm(200000, 2750, 30) %>% as.integer()
df <- data.frame(gridft_1) #%>% mutate(zone = "a") 
tccc1hlf <- as.data.frame(table(unlist(df))) 
tccc1hlf <- tccc1hlf %>% transmute(gridft_1 = as.character(Var1),
                                   gridft_1 = as.integer(gridft_1),
                                   count = Freq)
as_tibble(tccc1hlf)
tccc1hlf <- rep(tccc1hlf$gridft_1, tccc1hlf$count)

set.seed(2)
gridft_2 <- rnorm(200000, 6750, 60) %>% as.integer()
df <- data.frame(gridft_2) #%>% mutate(zone = "a") 
tccc1huf <- as.data.frame(table(unlist(df))) 
tccc1huf <- tccc1huf %>% transmute(gridft_2 = as.character(Var1),
                                   gridft_2 = as.integer(gridft_2),
                                   count = Freq)
as_tibble(tccc1huf)
tccc1huf <- rep(tccc1huf$gridft_2, tccc1huf$count)
#}

######### base R hist() and table ####
f <- ecdf(tccc1hlf)
p <- plot(f, main="Default Plot")
p

h <- hist(tccc1hlf, breaks = 20)
cum.prob <- data.frame(value=h$mids, prob=cumsum(h$counts)/sum(h$counts))
cum.prob

############# ggplot ##########

tccc1hlf <- data.frame(tccc1hlf) %>% transmute(zone = "tccc1hlf", gridft = tccc1hlf)
tccc1huf <- data.frame(tccc1huf) %>% transmute(zone = "tccc1huf", gridft = tccc1huf)
tccc1 <- rbind(tccc1hlf, tccc1huf)

f <- ecdf(tccc1hlf)
plot(f, main="Default Plot")


expand <- function(x, f=1.05, ...) {# Expand a data range
  r <- range(x, ...)
  (r - mean(r)) * f + mean(r) 
}
X <- with(environment(f), {
  x.range <- expand(x)
  data.frame(x=c(x.range[1], x), 
             xend=c(x, x.range[2]),
             y=c(0, y),
             yend=c(0, y))
})



p <- ggplot(X, aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_hline(yintercept=0:1, linetype=2, size=1.0, color="Gray") +
  geom_segment(color="#b0b0b0", size=0.8) +
  geom_point(data=X[-1,]) +
  xlab("Elevation") + ylab("Proportion of Total") + 
  ggtitle("Hypsometric Curve") + coord_flip()

p



#### 

# https://stackoverflow.com/questions/3544002/easier-way-to-plot-the-cumulative-frequency-distribution-in-ggplot

df <- data.frame(x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
                 g = gl(2, 100))
ggplot(df, aes(x, colour = g)) + stat_ecdf()

p <- ggplot(tccc1, aes(gridft)) + stat_ecdf() + coord_flip() +facet_wrap(~zone, nrow = 1, scales = "free")
p
ggplotly(p)


# youtube ---------------------https://www.youtube.com/watch?v=zRMikMaQE3I
#library(raster)
#ras <- raster("bigraster.tif")
#raszone <- raster("rasterzones.tif")
#ras_zstats <- zonal(ras, raszone, fun = 'mean', digits = 3, na.rm = T)





# SO tests -------------------------------------------------------------

 ### Robert Hijmas Solution 2a:

##  (reading raster from .gdb https://gis.stackexchange.com/questions/267186/importing-raster-with-r-from-file-geodatabase)
#library(arcgisbindings)
#arc.check_product()
#raster <- as.raster(arc.raster(arc.open("path/to/geodatabase.gdb/rasterlayername")))
#setwd("~/GISdata/Basin_delineation_2019/master_rast.gdb")
#elev_int_m <- raster("elev_m_ned2010_mostNVclipped")

setwd("~/GISdata/Elevation")
#elev_int_m <- raster("elev_m_ned2010_mostNVclipped.tif")
#elev_int_m <- raster("elev_m_ned2010_NVclip2.tif")
elev_int_m <- raster("elev_m_ned2010_071219zclip.tif")
values(elev_int_m) <- 1:ncell(elev_int_m)
setwd("~/GISdata/Basin_delineation_2019/shapefiles")
zones <- readOGR(".", "cnrfc_zones_07122019") %>% st_as_sf() 

setwd("~/R/proj/basins2019/area_elev")
source("zone_elevs_ns.r")
zones <- zones %>% filter(Name %in% newzones)


x <- extract(elev_int_m, zones, progress = 'text')
z <- do.call(rbind, lapply(1:length(x), function(i) cbind(i, x[[i]])))
as_tibble(z)

i <- sapply(x, length)
j <- rep(1:length(i), i)
z <- cbind(j, unlist(x))


# Mikkel Lydholm SO -------------------------------------------------------

setwd("~/GISdata/Elevation")
#elev_int_m <- raster("elev_m_ned2010_mostNVclipped.tif")
#elev_int_m <- raster("elev_m_ned2010_NVclip2.tif")
#elev_int_m <- raster("elev_m_ned2010_071219zclip.tif")
elev_int_m <- raster("elev_m_ned2010_071219zclip_n.tif")
res(elev_int_m)
#elev_int_m <- aggregate(elev_int_m, fact=2)  #coarsen resolution
#res(elev_int_m)
#values(elev_int_m) <- 1:ncell(elev_int_m)


setwd("~/GISdata/Basin_delineation_2019/shapefiles")
zones <- readOGR(".", "cnrfc_zones_07122019") %>% st_as_sf() 
zones <- zones %>% filter(Name %in% newzones)

zones_ID <- data.frame(c(1:length(zones$geometry)),as.character(zones$Name),stringsAsFactors = F) # create a frame to link geometry ID to the basin name
colnames(zones_ID) <- c("ID","Name") # a bit of house-keeping to make the frame easier to understand
vlx_data <- velox(elev_int_m) # velox rasters are super fast for extracting. Normal raster-package ones are sloooow.
extracted_data <- vlx_data$extract(zones,df=T,small=T) # extracting with velox is easy
colnames(extracted_data) <- c("ID","Elev") # house keeping again
merged_data <- merge(extracted_data,zones_ID,by="ID") # and now we merge the data tables to make nice looking dataset
head(merged_data)



# Hijmas Lydholm combo  ----------------------------------------------

setwd("~/GISdata/Elevation/forhome")
zones <- readOGR(".", "cnrfc_zones_07122019") %>% st_as_sf() %>% filter(ForecastGr == "CachePutah")
zones_ID <- data.frame(c(1:length(zones$geometry)),as.character(zones$Name),stringsAsFactors = F) # create a frame to link geometry ID to the basin name
colnames(zones_ID) <- c("ID","Name") # a bit of house-keeping to make the frame easier to understand
#elev_int_m <- raster("elev2010_int_m.tif")
elev_int_m <- raster("elev_m_capu.tif")

values(elev_int_m) <- 1:ncell(elev_int_m)

extracted_data <- extract(elev_int_m, zones, progress = 'text')
extracted_data2 <- extracteddata

z <- do.call(rbind, lapply(1:length(extracted_data), function(ID) cbind(ID, extracted_data[[ID]])))
as_tibble(z)
colnames(z) <- c("ID","Elev") # house keeping again
as_tibble(z)


merged_data <- merge(z,zones_ID,by="ID") # and now we merge the data tables to make nice looking dataset
as_tibble(merged_data)



