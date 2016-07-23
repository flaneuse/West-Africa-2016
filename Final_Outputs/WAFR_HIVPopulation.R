setwd("~/WAFR_HealthPAD")
install.packages('tidyr')
install.packages('library(RColorBrewer')
install.packages("ggplot2")
install.packages('dplyr')

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# People Living with HIV --------------------------------------------------
hivPop <- read.csv("~/WAFR_HealthPAD/3.10_PeopleWithHIV_BySexAge_UNAIDS.csv")

hivPop2 = hivPop%>% mutate(comb = paste0(country, indicator))

hivPopOrder = hivPop2 %>% 
  arrange(value)

hivPop2$comb = factor(hivPop2$comb,
                      levels = hivPopOrder$comb)

ggplot(data=hivPop2, aes(x = comb, y = value, fill = indicator))+
  geom_bar(stat="identity", width=.15)+
  #coord_flip()+
  facet_wrap(~country, scales="free", nrow = 3)+
  scale_y_continuous(breaks= seq(0, 350000, by = 10000))+
  scale_fill_brewer(palette="Spectral")+
  theme_bw()

ggsave("WAFR_HIVPopulation.pdf",width = 20, height = 10, units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)


# Key Population Size Estimate --------------------------------------------
keyPopSize <- read.csv("~/WAFR_HealthPAD/3.4_5_SizeEstimate_KeyPop_UNAIDS.csv")

keyPopSize2 = keyPopSize%>% mutate(comb = paste0(country, indicator))

keyPopSizeOrder = keyPopSize2 %>% 
  arrange(value)

keyPopSize2$comb = factor(keyPopSize2$comb,
                      levels = keyPopSizeOrder$comb)

ggplot(data=keyPopSize2, aes(x = comb, y = value, fill = indicator))+
  geom_bar(stat="identity", width=.15)+
  #coord_flip()+
  facet_wrap(~country, scales="free", nrow = 2)+
  scale_y_continuous(breaks= seq(0, 40000, by = 2000))+
  scale_fill_brewer(palette="Spectral")+
  theme_bw()

ggsave("WAFR_KeyPopulationSize.pdf",width = 20, height = 10 , units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)


# Key Population HIV Prevalence -------------------------------------------
keyPopPrev <- read.csv("~/WAFR_HealthPAD/3.4_5_HIVPrevalence_KeyPop_UNAIDS.csv")

keyPopPrev2 = keyPopPrev%>% mutate(comb = paste0(country, indicator))

keyPopPrevOrder = keyPopPrev2 %>% 
  arrange(value)

keyPopPrev2$comb = factor(keyPopPrev2$comb,
                          levels = keyPopPrevOrder$comb)

ggplot(data=keyPopPrev2, aes(x = comb, y = value, fill = indicator))+
  geom_bar(stat="identity", width=.15)+
  #coord_flip()+
  facet_wrap(~country, scales="free", nrow = 2)+
  scale_y_continuous(breaks= seq(0, 45, by = 5))+
  scale_fill_brewer(palette="Spectral")+
  theme_bw()

ggsave("WAFR_KeyPopulationHIVPrevalence.pdf",width = 20, height = 10 , units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)


# ART Coverage Rate -------------------------------------------------------
artCov <- read.csv("~/WAFR_HealthPAD/3.6_ARTCoverage_ByAgeSex_UNAIDS.csv")

artCov2 = artCov%>% mutate(comb = paste0(country, indicator))

artCovOrder = artCov2 %>% 
  arrange(value)

artCov2$comb = factor(artCov2$comb,
                          levels = artCovOrder$comb)

ggplot(data=artCov2, aes(x = comb, y = value, fill = indicator))+
  geom_bar(stat="identity", width=.15)+
  #coord_flip()+
  facet_wrap(~country, scales="free", nrow = 2)+
  scale_y_continuous(breaks= seq(5, 60, by = 5))+
  scale_fill_brewer(palette="Spectral")+
  theme_bw()

ggsave("WAFR_ARTCoverage.pdf",width = 20, height = 10 , units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)


# ART Retention Rate ------------------------------------------------------
artRet <- read.csv("~/WAFR_HealthPAD/3.6_ARTRetentionRate.csv")

ggplot(data=artRet, aes(x = country, y = value, fill = indicator))+
  geom_bar(stat="identity", width=.15)+
  #coord_flip()+
  facet_wrap(~country, scales="free", nrow = 2)+
  scale_y_continuous(breaks= seq(0, 90, by = 10))+
  scale_fill_brewer(palette="Spectral")+
  theme_bw()

ggsave("WAFR_WAFR_ARTRetention.pdf",width = 20, height = 10 , units = c("in"),
       dpi=300, limitsize = TRUE, useDingbats=FALSE, compress=FALSE)

