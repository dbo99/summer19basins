rm(list = ls())
setwd("~/Documents/basins19")
source("libs.r")
setwd("~/Documents/basins19/data")

#19 = summer19
## annual means

#####################
## elevation ##
#####################

elev_zones19new <- read_csv("elev_zones19new.csv")  %>% mutate(elev_ft = elev_m * 3.28084 %>% round())
as_tibble(elev_zones19new)


#elev_zones19new_mean <- elev_zones19new %>% group_by(zone) %>% 
#                         summarize(mean_ft = round(mean(elev_ft),0),
#                                  mean_m = round(mean(elev_m),0))
#write_csv(elev_zones19new_mean, "elev_zones19new_mean.csv")

elev_zones19new_mean <- read_csv("elev_zones19new_mean.csv")
as_tibble(elev_zones19new_mean)


#####################
## precip ##
#####################

####### ihabbs 2013/2016 precip #######

p13_zones19all <- read_csv("ihabbscap_Pann.tif_extract.csv")  %>% mutate(p_in = p_mm * 0.0393701 %>% round(2))
as_tibble(p13_zones19all)

#p13_zones19all_mean <- p13_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_mm = round(mean(p_mm),1),
#                                  mean_in = round(mean(p_in),2))
#as_tibble(p13_zones19all_mean)
#write_csv(p13_zones19all_mean, "precip13_zones19all_mean.csv")

p13_zones19all_mean <- read_csv("precip13_zones19all_mean.csv")
as_tibble(p13_zones19all_mean)

####### prism 2019 precip 07/20/19 dwnld #######

p19_zones19all <- read_csv("PRISM_ppt_30yr_normal_800mM2_annual_asc.asc_extract.csv")  %>% mutate(p_in = p_mm * 0.0393701 %>% round(2))
as_tibble(p19_zones19all)

#p19_zones19all_mean <- p19_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_mm = round(mean(p_mm),1),
#                                  mean_in = round(mean(p_in),2))
#as_tibble(p19_zones19all_mean)
#write_csv(p19_zones19all_mean, "precip19_zones19all_mean.csv")

p19_zones19all_mean <- read_csv("precip19_zones19all_mean.csv")
as_tibble(p19_zones19all_mean)


######################
## temp ##
#####################

####### ihabbs 2013/2016 precip #######

t13_zones19all <- read_csv("ihabbscap_Tann.tif_extract.csv")  %>% mutate(t_f = ((t_c * 9 / 5) + 32) %>% round(2))
as_tibble(t13_zones19all)

#t13_zones19all_mean <- t13_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_c = round(mean(t_c),2),
#                                  mean_f = round(mean(t_f),1))
#as_tibble(t13_zones19all_mean)
#write_csv(t13_zones19all_mean, "temp13_zones19all_mean.csv")

t13_zones19all_mean <- read_csv("temp13_zones19all_mean.csv")
as_tibble(t13_zones19all_mean)

####### prism 2019 precip 07/20/19 dwnld #######

t19_zones19all <- read_csv("PRISM_tmean_30yr_normal_800mM2_annual_asc.asc_extract.csv")   %>% mutate(t_f = ((t_c * 9 / 5) + 32) %>% round(2))
as_tibble(t19_zones19all)

#t19_zones19all_mean <- t19_zones19all %>% group_by(zone) %>% 
#                         summarize(mean_c = round(mean(t_c),2),
#                                  mean_f = round(mean(t_f),1))
#as_tibble(t19_zones19all_mean)
#write_csv(t19_zones19all_mean, "temp19_zones19all_mean.csv")

t19_zones19all_mean <- read_csv("temp19_zones19all_mean.csv")
as_tibble(t19_zones19all_mean)
