Edu <- read.csv("~/WAFR_HealthPAD/2.4_SecondaryCompletionRate_Ages15_24_DHS.csv")
setwd("~/WAFR_HealthPAD")

library(ggplot2)
library(dplyr)
library(tidyr)

edufil = Edu  %>%filter(category %in% c("Region and Sex", 'Urban/Rural and Sex')) %>%
  mutate(comb = paste0(region, country))

orderEdu = edufil %>%
  arrange(mean)

edufil$comb = factor(edufil$comb,
                       levels = orderEdu$comb)

ggplot(edufil, aes(x = mean, y = comb, colour = sex))+
  geom_point(size = 6)+
  facet_wrap(~country, scales = "free", nrow = 3)+
  scale_x_continuous(breaks= seq(0, 0.7, by = 0.05),
                     labels = scales::percent) +
  theme_bw()

ggsave("WAFR_EducationPlot.pdf",width = 20, height = 10, units = c("in"),
       dpi = 300, limitsize = TRUE)