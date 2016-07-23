# Import Data and Install Packages ----------------------------------------
## Data
healthEcon <- read.csv("~/Documents/GitHub/West-Africa-Health/1.2_HealthEconomics_GDP_TotalHE_ExternalResources.csv")
setwd("~/Documents/GitHub/West-Africa-Health")

## Packages
library(extrafont)
loadfonts()
library(llamar)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(devtools)


## Reshaping Data

# Health Economics 1: GDP + Public ealth expend. + Ext expend -------------------------------
## Order the data
order = healthEcon %>%
  filter(year == 2014, indicator == "Health expenditure, public (% of government expenditure)") %>%
  ungroup() %>%
  arrange(desc(value))

healthEcon$country = factor(healthEcon$country,
                    levels = order$country)

## Plot data
ggplot(healthEcon, aes(x = year, y = value, colour = indicator, group = indicator)) +
  geom_line() +
  ## These help scale the data because the min-max y values vary a bunch
  annotate(geom = 'point', x= 2014, y = 60, size = 0, colour = 'white') +
  annotate(geom = 'point', x= 2014, y = -10, size = 0, colour = 'white') +
  facet_wrap(~country, scales = 'free_y') +
  theme_basic() +
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'))

ggsave("WAFR_HealthEconomics_GDPandHealthExpenditure_revised.pdf",
       width = 7, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Health Economics 2: Public expend. and Out of Pocket expend. ------------
ggplot(he %>% filter(var %in% c('OutOfPocketHlthExpend', 'GovHlthExpendOfTotalExpend')),
       aes(x = Year, y = rate,
           colour = var,
           group = var)) +
  geom_line() +
  facet_wrap(~Country) +
  theme_basic() +
  theme(legend.position = c(0.8, 0.1),
        panel.margin = unit(0.5, 'lines'))+
  theme(text=element_text(size=16, family="Arial"))

ggsave("WAFR_HealthEconomics_PublicandOutofPocketHealthExpenditure.pdf",
       width = 7, height = 4.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)
