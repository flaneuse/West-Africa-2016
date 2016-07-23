hlthwf <- read.csv("~/WAFR_HealthPAD/4.1_HealthWorkForce_WHO.csv")
setwd("~/WAFR_HealthPAD")
install.packages('tidyr')
install.packages('library(RColorBrewer')
install.packages("ggplot2")
install.packages('dplyr')

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

#ggplot(hlthwf, aes(x = country, y = value, fill = factor(indicator)))+
#  geom_text(aes(label=value))+
#  geom_bar(stat="identity", position='fill')+
#  scale_fill_brewer(type = "div", palette = "Spectral")+
#  facet_wrap(~country)+
#  theme_bw()

hlthwf2 = hlthwf%>% mutate(comb = paste0(country, indicator))

hlthorder = hlthwf2 %>% 
  arrange(value)

hlthwf2$comb = factor(hlthwf2$comb,
                    levels = hlthorder$comb)

ggplot(hlthwf2, aes(x = value, y = comb, colour = value))+
  geom_point(size = 6)+
  facet_wrap(~country, scales = "free", nrow = 3)+
  scale_x_continuous(breaks= seq(0, 0.8, by = 0.05))+
  scale_colour_gradientn(colours = brewer.pal(6, 'YlGnBu')) +
  theme_bw()

ggsave("WAFR_HealthWorkforce.pdf",width = 20, height = 5, units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)