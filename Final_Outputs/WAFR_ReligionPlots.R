relig <- read.csv("~/WAFR_HealthPAD/2.5_Religion_CIA.csv")
setwd("~/WAFR_HealthPAD")

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)

relig2 = relig %>% mutate(ord = paste0(Religion,Country))

orderRelig = relig2 %>%
  arrange(Percent)

relig2$ord = factor(relig2$ord,
                     levels = orderRelig$ord)

ggplot(relig2, aes(x = Percent, y = ord, colour = Percent))+
  geom_point(size = 6)+
  facet_wrap(~Country, scales = "free", nrow = 3)+
  scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
  #scale_x_continuous(limits= c(0,100))+
  theme_bw()

ggsave("WAFR_ReligionPlot.pdf",width = 15, height = 10, units = c("in"),
       dpi = 300, limitsize = TRUE)