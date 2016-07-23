mortRate <- read.csv("~/WAFR_HealthPAD/1.5_MMR_U5MR_WorldBank.csv")
setwd("~/WAFR_HealthPAD")

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)
#devtools::install_github("flaneuse/llamar")
library(llamar)
library(extrafont)
loadfonts()

# Maternal Mortality Rate Sparkline -------------------------------------------------
mortRate2 = mortRate %>% filter(year>2005)

mmrOrder = mortRate2 %>% 
  filter(year == 2015) %>% 
  ungroup() %>% 
  arrange(desc(mmr))

mortRate2$country = factor(mortRate2$country, 
                          levels = mmrOrder$country)

ggplot(mortRate2, aes(x = year, y = mmr, label=mmr))+
  geom_line()+
  geom_point(size=3, color=grey90K, stroke=.15, shape=21,
             fill="white", data = mortRate2 %>% filter(year == 2006 | year == 2015))+
  geom_text(family="Segoe UI", size = 3, nudge_x=.2, nudge_y=10, data = mortRate2 %>% filter(year == 2006 | year == 2015))+
  facet_wrap(~country, scales= "free_y", ncol=1)+
  theme_blank()+
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'),
        strip.background = element_blank())

ggsave("mmrSpark.pdf",
       width = 2, height = 8,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Under 5 Mortality Rate Sparkline --------------------------------------------------

ggplot(mortRate2, aes(x = year, y = mmr, label= round(u5mr,0))) +
  geom_line()+
  geom_point(size=3, color=grey90K, stroke=.15, shape=21,
             fill="white", data=mortRate2 %>% filter(year %in% c(2006,2015)))+
  geom_text(family="Segoe UI", size = 3, nudge_x=.2, nudge_y=.5,data = mortRate2 %>% filter(year == 2006 | year == 2015))+
  facet_wrap(~country, scales= "free_y", ncol=1) +
  theme_blank()+
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'),
        strip.background = element_blank())

ggsave("U5mrSpark.pdf",
       width = 2, height = 8,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Maternal Mortality Heat Map ---------------------------------------------------------------

## MMR Heat Map
mmrOrder = mortRate2 %>% 
  filter(year == 2015) %>% 
  mutate(textColor=ifelse(mmr > 1.5*mean(mmr), grey10K, grey90K)) %>%
  mutate(textColor2=ifelse(u5mr > 70, grey10K, grey90K)) %>%
  ungroup() %>% 
  arrange(desc(mmr))

mmrOrder$country = factor(mmrOrder$country, 
                          levels = rev(mmrOrder$country))


ggplot(mmrOrder, aes(x=1, y=country, fill=mmr, label=mmr, color=textColor))+
  geom_tile(color="white", size=.15)+
  theme_labelsOnly()+
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'))+
  scale_color_identity()+
  geom_text(family="Segoe UI", size = 3)+
  theme(strip.background = element_blank())

ggsave("mmrHeat.pdf",
       width = 2, height = 8,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Under 5 Mortality Heat Map ----------------------------------------------
ggplot(mmrOrder, aes(x=1, y=country, fill=u5mr, label=u5mr, color=textColor2))+
  geom_tile(color="white", size=.15)+
  theme_labelsOnly()+
  scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu'))+
  scale_color_identity()+
  geom_text(family="Segoe UI", size = 3)+
  theme(strip.background = element_blank())

ggsave("u5mrHeat.pdf",
       width = 2, height = 8,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

