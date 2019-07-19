
library(tidyverse)
library(plotly)


{
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
}

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

ggplot(tccc1, aes(gridft)) + stat_ecdf() + coord_flip() +facet_wrap(~zone, nrow = 1, scales = "free")

