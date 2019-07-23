rm(list = ls())
setwd("~/Documents/basins19/shiny")
source("libs.r")

elevs <- read_csv("elevft_newbasins.csv")
t13 <- read_csv("t13.csv")
p13 <- read_csv("p13.csv")
t19 <- read_csv("t19.csv")
p19 <- read_csv("p19.csv")

head(elevs)
head(t13)
head(p13)
head(t19)
head(p19)

t_p <- rbind(t13,p13,t19,p19)
write_csv(t_p, "t_p.csv")


e <- elevs %>% uncount(count)
head(e)
#write_csv(e, "etest.csv")

unit_m <- c("mm", "cels", "m")
unit_e <- c("in", "fahr", "ft")
unit <- data.frame(unit_e, unit_m)
head(unit)

df <- inner_join(p19, unit)

head(p19)
head(unit)



elev <- elevcsv %>% filter(zone %in% input$zone) %>% uncount(count) %>% 
  mutate(unit_e = "ft", param = "elev", dsrce = "ned2010")

df <- rbind(elev, t13, p13, t19, p19)

df <- inner_join(df, unit)


elev_csv <- read_csv("elevft_newbasins.csv")
t13_csv <- read_csv("t13.csv")
p13_csv <- read_csv("p13.csv")
t19_csv <- read_csv("t19.csv")
p19_csv <- read_csv("p19.csv")

#unit_m <- c("mm", "cels", "m")
#unit_e <- c("in", "fahr", "ft")
#unit <- data.frame(unit_e, unit_m)

t13 <- t13_csv %>%  mutate(param = "t", dsrce = "prsm13")
p13 <- p13_csv %>%  mutate(param = "p", dsrce = "prsm13")
t19 <- t19_csv %>%  mutate(param = "t", dsrce = "prsm19")
p19 <- p19_csv %>%  mutate(param = "p", dsrce = "prsm19")

elev <- elev_csv  %>% filter(zone == "CEGC1LLF") %>% uncount(count) %>% 
  mutate(unit_e = "ft", param = "elev", dsrce = "ned2010")

df <- rbind (elev, t13, p13, t19, p19)
head(df)
df <- inner_join(df, unit) %>%
head(df)

elev_csv <- read_csv("elevft_newbasins.csv")
t13_csv <- read_csv("t13.csv")
p13_csv <- read_csv("p13.csv")
t19_csv <- read_csv("t19.csv")
p19_csv <- read_csv("p19.csv")

t13 <- t13_csv  %>% mutate(param = "t", dsrce = "prsm13")
p13 <- p13_csv  %>% mutate(param = "p", dsrce = "prsm13")
t19 <- t19_csv  %>% mutate(param = "t", dsrce = "prsm19")
p19 <- p19_csv  %>% mutate(param = "p", dsrce = "prsm19")


elev <- elev_csv  %>% uncount(count) %>% 
  mutate(unit = "ft", param = "elev", dsrce = "ned2010")

df <- rbind(elev, t13, p13, t19, p19)

write_csv(df, "df.csv")

