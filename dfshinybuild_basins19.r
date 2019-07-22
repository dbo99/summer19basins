rm(list = ls())
setwd("~/Documents/basins19")
source("libs.r")
setwd("~/Documents/basins19/data")

zones <- readOGR(".", "cnrfc_zones_07122019_83alb_eacusgs") %>% st_as_sf() %>% as.data.frame() %>% 
  transmute(zone = Name, basin = Basin, area_m = Shape_Area, lat_cent, lon_cent) %>% distinct()

{
#`19` = summer19 cnrfc basin additions
## annual means

#####################
## elevation ##
#####################

{
elev_zones19new_m <- read_csv("elev_zones19new.csv")  %>% transmute(zone, val = elev_m, unit = "m") 
elev_zones19new_f <- elev_zones19new_m %>% mutate(val = val * 3.28084 %>% round(), unit = "ft")
elev_zones19new <- rbind(elev_zones19new_m, elev_zones19new_f) %>% mutate(dsrce = "ned2010")
rm(elev_zones19new_m, elev_zones19new_f)
#head(elev_zones19new)
#tail(elev_zones19new)
}

#elev_zones19new_mean <- elev_zones19new %>% group_by(zone) %>% 
#                         summarize(mean_ft = round(mean(elev_ft),0),
#                                  mean_m = round(mean(elev_m),0))
#write_csv(elev_zones19new_mean, "elev_zones19new_mean.csv")
{
elev_zones19new_mean_m <- read_csv("elev_zones19new_mean.csv") %>% transmute(zone, annmean = mean_m, unit = "m")
elev_zones19new_mean_ft <- read_csv("elev_zones19new_mean.csv") %>% transmute(zone, annmean = mean_ft, unit = "ft")
elev_zones19new_mean <- rbind(elev_zones19new_mean_m,elev_zones19new_mean_ft) %>% mutate(dsrce = "ned2010")
rm(elev_zones19new_mean_m,elev_zones19new_mean_ft)
head(elev_zones19new_mean)
tail(elev_zones19new_mean)

elev_zones19new <- elev_zones19new %>% mutate(param = "elev")

elev_mn = elev_zones19new_mean %>% mutate(param = "elev")
rm(elev_zones19new_mean)
}
#####################
## precip ##
#####################

####### ihabbs 2013/2016 precip #######
{
p13_zones19all_mm <- read_csv("ihabbscap_Pann.tif_extract.csv") %>% transmute(zone, val = p_mm, unit = "mm")
p13_zones19all_in <- p13_zones19all_mm %>% mutate(val = val * 0.0393701 %>% round(2), unit = "in")
p13_zones19all <- rbind(p13_zones19all_mm , p13_zones19all_in) %>% mutate(dsrce = "prsm13")
rm(p13_zones19all_mm , p13_zones19all_in)
head(p13_zones19all)
tail(p13_zones19all)
}
#p13_zones19all_mean <- p13_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_mm = round(mean(p_mm),1),
#                                  mean_in = round(mean(p_in),2))
#as_tibble(p13_zones19all_mean)
#write_csv(p13_zones19all_mean, "precip13_zones19all_mean.csv")
{
p13_zones19all_mean_mm <- read_csv("precip13_zones19all_mean.csv") %>% transmute(zone, annmean = mean_mm, unit = "mm")
p13_zones19all_mean_in <- read_csv("precip13_zones19all_mean.csv") %>% transmute(zone, annmean = mean_in, unit = "in")
p13_zones19all_mean <- rbind(p13_zones19all_mean_in ,p13_zones19all_mean_mm ) %>% mutate(dsrce = "prsm13")
rm(p13_zones19all_mean_in ,p13_zones19all_mean_mm)
#head(p13_zones19all_mean)
#tail(p13_zones19all_mean)
}




####### prism 2019 precip 07/20/19 dwnld #######

{
p19_zones19all_mm <- read_csv("PRISM_ppt_30yr_normal_800mM2_annual_asc.asc_extract.csv")  %>% transmute(zone, val = p_mm, unit = "mm")  
p19_zones19all_in <- p19_zones19all_mm %>% mutate(val = val * 0.0393701 %>% round(2), unit = "in")
p19_zones19all <- rbind(p19_zones19all_in , p19_zones19all_mm ) %>% mutate(dsrce = "prsm19")
rm(p19_zones19all_mm, p19_zones19all_in )
head(p19_zones19all)
tail(p19_zones19all)
}
#p19_zones19all_mean <- p19_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_mm = round(mean(p_mm),1),
#                                  mean_in = round(mean(p_in),2))
#as_tibble(p19_zones19all_mean)
#write_csv(p19_zones19all_mean, "precip19_zones19all_mean.csv")

{
p19_zones19all_mean_mm <- read_csv("precip19_zones19all_mean.csv") %>% transmute(zone, annmean = mean_mm, unit = "mm")
p19_zones19all_mean_in <- read_csv("precip19_zones19all_mean.csv") %>% transmute(zone, annmean = mean_in, unit = "in")
p19_zones19all_mean <- rbind(p19_zones19all_mean_mm, p19_zones19all_mean_in) %>% mutate(dsrce = "prsm19")
rm(p19_zones19all_mean_mm, p19_zones19all_mean_in)
head(p19_zones19all_mean)
tail(p19_zones19all_mean)
}

{
p_zones19all <- rbind(p13_zones19all , p19_zones19all) %>% mutate(param = "p")
#head(p_zones19all)
rm(p13_zones19all , p19_zones19all)
p_zones19all_mn <- rbind(p13_zones19all_mean, p19_zones19all_mean) %>% mutate(param = "p")
#head(p_zones19all_mn )
rm(p13_zones19all_mean, p19_zones19all_mean)

}
######################
## temp ##
#####################

####### ihabbs 2013/2016 temp #######
{
t13_zones19all_c <- read_csv("ihabbscap_Tann.tif_extract.csv")  %>% transmute(zone, val = t_c, unit = "cels")
t13_zones19all_f <- t13_zones19all_c %>% mutate(val = val * 9 / 5 + 32 %>% round(2), unit = "fahr")
t13_zones19all <- rbind(t13_zones19all_c ,t13_zones19all_f ) %>% mutate(dsrce = "prsm13")
rm(t13_zones19all_c ,t13_zones19all_f )
head(t13_zones19all)
tail(t13_zones19all)
}
#t13_zones19all_mean <- t13_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_c = round(mean(t_c),2),
#                                  mean_f = round(mean(t_f),1))
#as_tibble(t13_zones19all_mean)
#write_csv(t13_zones19all_mean, "temp13_zones19all_mean.csv")
{
t13_zones19all_mean_c <- read_csv("temp13_zones19all_mean.csv") %>% transmute(zone, annmean = mean_c, unit = "cels") %>% mutate(dsrce = "prsm13")
t13_zones19all_mean_f <- read_csv("temp13_zones19all_mean.csv") %>% transmute(zone, annmean = mean_f, unit = "fahr") %>% mutate(dsrce = "prsm13")
t13_zones19all_mean <- rbind(t13_zones19all_mean_c, t13_zones19all_mean_f) %>% mutate(param = "t")
rm(t13_zones19all_mean_c, t13_zones19all_mean_f)
head(t13_zones19all_mean)
tail(t13_zones19all_mean)
}

####### prism 2019 temp 07/20/19 dwnld #######
{
t19_zones19all_c <- read_csv("PRISM_tmean_30yr_normal_800mM2_annual_asc.asc_extract.csv")  %>% transmute(zone, val = t_c, unit = "cels")
t19_zones19all_f <- t19_zones19all_c %>% mutate(val = val * 9 / 5 + 32 %>% round(2), unit = "fahr")
t19_zones19all <- rbind(t19_zones19all_f, t19_zones19all_c ) %>% mutate(dsrce = "prsm19")
rm(t19_zones19all_c, t19_zones19all_f )
head(t19_zones19all)
tail(t19_zones19all)
}

#t19_zones19all_mean <- t19_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_c = round(mean(t_c),2),
#                                  mean_f = round(mean(t_f),1))
#as_tibble(t19_zones19all_mean)
#write_csv(t19_zones19all_mean, "temp19_zones19all_mean.csv")

{
t19_zones19all_mean_c <- read_csv("temp19_zones19all_mean.csv") %>% transmute(zone, annmean = mean_c, unit = "cels")  %>% mutate(dsrce = "prsm19")
t19_zones19all_mean_f <- read_csv("temp19_zones19all_mean.csv") %>% transmute(zone, annmean = mean_f, unit = "fahr")  %>% mutate(dsrce = "prsm19")
t19_zones19all_mean <- rbind(t19_zones19all_mean_f, t19_zones19all_mean_c) %>% mutate(param = "t")
rm(t19_zones19all_mean_f, t19_zones19all_mean_c)
head(t19_zones19all_mean )
tail(t19_zones19all_mean )
}

{
head(t13_zones19all )
head(t19_zones19all )

t_zones19all <- rbind(t13_zones19all , t19_zones19all) %>% mutate(param = "t")
rm(t13_zones19all , t19_zones19all)
head(t13_zones19all_mean)
head(t19_zones19all_mean)

t_zones19all_mn <- rbind(t13_zones19all_mean, t19_zones19all_mean)
rm(t13_zones19all_mean, t19_zones19all_mean)


#### compile all & discard intermediates
head(elev_zones19new)
head(p_zones19all)
head(t_zones19all)
df <- rbind(elev_zones19new, p_zones19all,t_zones19all)
rm(elev_zones19new, p_zones19all,t_zones19all)
df_mn <- rbind(t_zones19all_mn , p_zones19all_mn, elev_mn )
rm(t_zones19all_mn , p_zones19all_mn, elev_mn )
}

  }  
  

df_english <- df %>% filter(unit == "ft" | unit == "in" | unit == "fahr")
df_english_mn <- df_mn %>% filter(unit == "ft" | unit == "in" | unit == "fahr")

#write_csv(df, "shinydf.csv")
#write_csv(df_mn, "shinydf_mn")

write_csv(df_english, "shinydf_english.csv")
write_csv(df_english_mn, "shinydf_mn_english")
