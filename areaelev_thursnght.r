
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

######### base R hist() and table ####
f <- ecdf(tccc1hlf)
p <- plot(f, main="Default Plot")
p

h <- hist(tccc1hlf, breaks = 20)
cum.prob <- data.frame(value=h$mids, prob=cumsum(h$counts)/sum(h$counts))
cum.prob

############# ggplot ##########

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

