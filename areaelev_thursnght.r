
library(tidyverse)
library(plotly)

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
library(raster)
ras <- raster("bigraster.tif")
raszone <- raster("rasterzones.tif")
ras_zstats <- zonal(ras, raszone, fun = 'mean', digits = 3, na.rm = T)




If I have an integer raster, say of ground elevation data for a country, 
and one polygon shapefile, say of 300 river basins in that country with a 
unique name for each, how would I most easily get output like this:
  
basin, gridcellelevation
a, 320
a, 321
a, 321
b, 17,
b, 18,
b, 19,

The most burdensome way seems that I could turn the single shapefile into 300 shapefiles, 
clip the raster 300 times into uniqueID rasters, read them in, generate
individual tables for each basin, then finally rbind() all together. The ideal
seems to be able to create that same table with just the one raster and one shapefile
I'm not looking for any statistics or counts, basically just the raw data listing of the grid
cell elevations that would have been part of a clip.  I don't have any data at the moment,
so I'd just be grateful for any tips or libraries or functions that could achieve this, so
I can look into them before I have access to the data soon (in a day or so).





