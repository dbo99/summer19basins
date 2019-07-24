rm(list = ls())
#setwd("~/Documents/basins19/shiny")
setwd("~/R/proj/basins2019/Shiny")
source("libs.r")

#zoneselect <- c("CEGC1LLF", "CEGC1LUF", "TCCC1HUF", "TCCC1HLF")

#elev_csv <- read_csv("elevft_newbasins.csv") %>% mutate(unit = "ft", param = "elev", dsrce = "ned2010")
elev_csv <- read_csv("elevft_newbasins.csv") %>% mutate(param = "elev")
head(elev_csv)
#elev_csv <- read_csv("elevft_newbasins.csv") %>% mutate(unit = "ft", param = "elev", dsrce = "ned2010")
#head(elev)
t13 <- read_csv("t13.csv") %>% mutate(param = "t", dsrce = "prsm13") 
p13 <- read_csv("p13.csv") %>% mutate(param = "p", dsrce = "prsm13") 
t19 <- read_csv("t19.csv") %>% mutate(param = "t", dsrce = "prsm19") 
p19 <- read_csv("p19.csv") %>% mutate(param = "p", dsrce = "prsm19")  
head(t13)

p_t <- rbind(t13, p13, t19, p19)
head(p_t)


#df <- df  %>%    
#  filter(param %in% input$param) %>%
#  filter(dsrce %in% input$dsrce) 
#
#if (input$units == "metric")  
#  
#  df <- df %>% mutate(unit = ifelse(unit == "in", "mm", unit)) %>%
#  mutate(unit = ifelse(unit == "fahr", "cels", unit)) %>%
#  mutate(unit = ifelse(unit == "ft", "m", unit)) %>%
#  mutate(val = ifelse(unit == "mm", val * 25.4, val )) %>%
#  mutate(val = ifelse(unit == "cels", (val - 32) * 5/9, val)) %>%
#  mutate(val = ifelse(unit == "m", val * 0.3048, val))
#
#
#
#
#df <- df %>% mutate(basin = substr(zone,1,nchar(zone)-3))#

