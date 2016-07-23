hlthFac <- read.csv("~/WAFR_HealthPAD/4.1_HealthCareFacilities.csv")
setwd("~/WAFR_HealthPAD")
install.packages('tidyr')
install.packages('dplyr')
install.packages('RColorBrewer')
install.packages('ggplot2')

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Sort by density of health facilities
hlthFac2 = hlthFac %>%

filter(facType != 'Health Post',
       facType != 'Health Centre')%>%

mutate(comb = paste0(country, facType))

hlthFacOrder = hlthFac2 %>% 
  arrange(density100k)

hlthFac2$comb = factor(hlthFac2$comb,
                      levels = hlthFacOrder$comb) 

#GGPlot facility density
ggplot(hlthFac2, aes(x = density100k, y = comb, colour = density100k))+
  geom_point(size = 6)+
  facet_wrap(~country, scales = "free", nrow = 3)+
  scale_x_continuous(breaks= seq(0, 6, by = 0.1))+
  scale_colour_gradientn(colours = brewer.pal(6, 'YlGnBu')) +
  theme_bw()

ggsave("WAFR_HealthFacilityDensity.pdf",width = 20, height = 5, units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)