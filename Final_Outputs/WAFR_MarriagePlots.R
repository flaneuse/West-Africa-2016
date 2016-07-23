MarriageAge <- read.csv("~/WAFR_HealthPAD/2.3_FirstMarriageAge_UNPopulation.csv")
setwd("~/WAFR_HealthPAD")
install.packages('tidyr')

library(ggplot2)
library(dplyr)
library(tidyr)

marriage = MarriageAge %>%
  gather(gender, avgAge, -Country, -`Legal.Age.of.Marriage`)

ggplot(marriage, aes(x = avgAge, y = gender, colour = gender))+
  geom_segment(aes(x = 10, xend = avgAge, y = gender, yend = gender), size = 1, color = "dodgerblue")+
  geom_segment(aes(x = `Legal.Age.of.Marriage`, xend = `Legal.Age.of.Marriage`, 
                   y = 0, yend = 3)) +
  geom_point(size = 6)+
  facet_wrap(~Country)+
  scale_x_continuous(breaks= seq(10, 30, by = 5))+
  theme_bw()

ggsave("WAFR_MarriageAge.pdf",width = 20, height = 10, units = c("in"),
       dpi = 300, limitsize = TRUE)