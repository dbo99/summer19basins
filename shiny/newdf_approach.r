

rm(list = ls())
wd <- "~/R/proj/basins2019/Shiny/shiny_mdntue/shiny_mdntue~/newdfapproach"
source("libs.r")


# elev --------------------------------------------------------------------

{

setwd <- setwd(wd)   
zelevcounts <- read_csv("elevft_newbasins.csv")
#head(zelevcounts)
source("zones2019.r")
zones2019  
  
setwd("./elev")
for (i in 1:length(zones2019)) {
  
  zelevcounts_i <- zelevcounts %>% filter(zone %in% zones2019[i])
  zelevcounts_i  #%>% transmute(val, count )
  write_csv(zelevcounts_i, paste0(zones2019[i],".csv"))
  
                               }

}

# p13 ---------------------------------------------------------------------

{
setwd(wd)
p13 <- read_csv("p13.csv")
allzones <- unique(p13$zone)
head(p13)
#  length(unique(p13$zone))   #501

setwd("./p13")
for (i in 1:length(unique(p13$zone))) {
  
  pcounts_i <- p13 %>% filter(zone %in% allzones[i])
  pcounts_i  #%>% transmute(val, count )
  write_csv(pcounts_i, paste0(allzones[i],".csv"))
  
                                      }
}

# p19 --------------------------------------------------------------------

{
setwd(wd)
p19 <- read_csv("p19.csv")
allzones <- unique(p19$zone)
head(p19)
setwd("./p19")

for (i in 1:length(unique(p19$zone))) {

  pcounts_i <- p19 %>% filter(zone %in% allzones[i])
  pcounts_i  #%>% transmute(val, count )
  write_csv(pcounts_i, paste0(allzones[i],".csv"))
  
                                      }

}

# t13 ---------------------------------------------------------------------


{
setwd(wd)
t13 <- read_csv("t13.csv")
allzones <- unique(t13$zone)
head(t13)
#  length(unique(t13$zone))   #501

setwd("./t13")
for (i in 1:length(unique(t13$zone))) {
  
  pcounts_i <- t13 %>% filter(zone %in% allzones[i])
  pcounts_i  #%>% transmute(val, count )
  write_csv(pcounts_i, paste0(allzones[i],".csv"))
  
                                       }

}
# t19 ---------------------------------------------------------------------

{
setwd(wd)
t19 <- read_csv("t19.csv")
allzones <- unique(t19$zone)
head(t19)
setwd("./t19")
for (i in 1:length(unique(t19$zone))) {
  
  pcounts_i <- t19 %>% filter(zone %in% allzones[i])
  pcounts_i  #%>% transmute(val, count )
  write_csv(pcounts_i, paste0(allzones[i],".csv"))
  
                                      }

}
