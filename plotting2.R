rm(list = ls())
setwd("~/Documents/basins19")
source("libs.r")
setwd("~/Documents/basins19/data")
elev_zones19 <- read_csv("elev_zones19.csv") %>% mutate(elev_ft = elev_m * 3.28084)
as_tibble(elev_zones19)
elev_zones19_freq <- elev_zones19 %>% group_by(zone, elev_ft) %>% summarize(count=n())
as_tibble(elev_zones19_freq)


#cegc1llf <- df %>% 

z5 <- df %>% filter(zone == "FRAC1LLF", unit == "ft", dsrce == "ned2010")

p <- ggplot(z5, aes(val)) + stat_ecdf(pad = FALSE) + coord_flip() +facet_wrap(~zone, nrow = 1, scales = "fixed")
p
#https://community.plot.ly/t/bug-with-ggplot2-stat-ecdf-function/1187/3
b <- z5[order(z5$elev_m), ]
(gg_sort <- ggplot(b, aes(x = elev_ft) ) + 
    stat_ecdf(pad = FALSE) + coord_flip() +facet_wrap(~zone, nrow = 1, scales = "free"))
ggplotly(gg_sort)

### test with shiny df

df3 <- df  %>%  filter(zone %in% input$zone_id)   %>% 
  filter(paramnam %in% input$paramnam) %>%
  filter(dsrce %in% input$dscrce) 

  
  if (input$units == "metric")  
dfe <- read_csv("shinydf_english.csv")
dfet <- df %>% mutate(unit = ifelse(unit == "in", "mm", unit)) %>%
             mutate(unit = ifelse(unit == "fahr", "cels", unit)) %>%
             mutate(unit = ifelse(unit == "ft", "m", unit)) %>%
             mutate(val = ifelse(unit == "mm", val * 25.4, val )) %>%
             mutate(val = ifelse(unit == "cels", (val - 32) * 5/9, val)) %>%
             mutate(val = ifelse(unit == "m", val * 0.3048, val))




# plotting ----------------------------------------------------------------

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

p <- ggplot(z5, aes(elev_m)) + stat_ecdf() + coord_flip() +facet_wrap(~zone, nrow = 1)#, scales = "free")
p
#https://community.plot.ly/t/bug-with-ggplot2-stat-ecdf-function/1187/3
b <- z5[order(z5$elev_m), ]
(gg_sort <- ggplot(b, aes(x = elev_m) ) + 
    stat_ecdf() + coord_flip() +facet_wrap(~zone, nrow = 1))
ggplotly(gg_sort)

#
ggplotly(p)


# youtube ---------------------https://www.youtube.com/watch?v=zRMikMaQE3I
#library(raster)
#ras <- raster("bigraster.tif")
#raszone <- raster("rasterzones.tif")
#ras_zstats <- zonal(ras, raszone, fun = 'mean', digits = 3, na.rm = T)

animals <- c("duck", "llama", "python")
n <- 50
animals_sd <- seq_along(animals)^2
a <- data.frame(anim = rep(animals, each = n), size = rpois(length(animals) * n, rep(animals_sd, each = n)))

# Works fine in ggplot2
(gg <- ggplot(a, aes(x = size, colour = anim) ) +
    stat_ecdf())
# but ggplotly messes it up
ggplotly(gg)

# This can be fixed by sorting by the x aesthetic:
b <- a[order(a$size), ]

(gg_sort <- ggplot(b, aes(x = size, colour = anim) ) +
    stat_ecdf())
ggplotly(ge_sort)






