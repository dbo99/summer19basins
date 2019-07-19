
library(tidyverse)
library(plotly)


{
rm(list = ls())
gridft <- rnorm(200000, 2750, 30) %>% as.integer()
df <- data.frame(gridft) #%>% mutate(zone = "a") 
tccc1hlf <- as.data.frame(table(unlist(df))) 
tccc1hlf <- tccc1hlf %>% transmute(gridft = as.character(Var1),
                                   gridft = as.integer(gridft),
                                   count = Freq)
as_tibble(tccc1hlf)
tccc1hlf <- rep(tccc1hlf$gridft, tccc1hlf$count)
}


f <- ecdf(tccc1hlf)
p <- plot(f, main="Default Plot")
p



h <- hist(tccc1hlf, breaks = 20)
cum.prob <- data.frame(value=h$mids, prob=cumsum(h$counts)/sum(h$counts))
cum.prob
