
# Quick plot of Ghana data to show the power of disaggregation ------------


# import data -------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(data.table)
library(llamar)
loadPkgs()

# Data from http://www.education-inequalities.org/countries/ghana/indicators/comp_lowsec/sexes#?dimension=sex&group=|Female|Male&dimension2=community&group2=|Rural|Urban&age_group=comp_lowsec_1524&year=2011
ed = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/UNESCO_Ghana_ComplLowerSec.xlsx')



# globals -----------------------------------------------------------------

femColor = '#9483BD'

maleColor = '#6AB2E2'

# Country average
countryTotal = data.frame(mean = 0.46)

# limits
xMax = 0.75
xMin = 0.15
sizeDot = 4

# save size
widthPlot = 8

# initial plot: avg -------------------------------------------------------


ggplot(countryTotal, aes(x = mean, y = 'total')) +
  geom_vline(xintercept = countryTotal$mean, colour = grey90K, size = 0.15) +
  geom_point(size = sizeDot, shape = 1, colour = grey90K) +
  scale_x_continuous(labels = scales::percent, limits = c(xMin, xMax)) +
  theme_xylab() +
  ylab('')

ggsave('Ghana_disag_total.pdf', width = widthPlot,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Prelim breakdown: sex --------------------------------------------------------

sex = ed %>% filter(category %like% 'Country Total Sex')

ggplot(sex, aes(x = mean, y = 'sex', 
                colour = sex, shape = sex)) +
  geom_vline(xintercept = countryTotal$mean, colour = grey90K, size = 0.15) +
  geom_point(size = sizeDot) +
  scale_colour_manual(values = c('Male' = maleColor, 'Female' = femColor)) +
  scale_x_continuous(labels = scales::percent, limits = c(xMin, xMax)) +
  scale_shape_manual(values = c('Male' = 16, 'Female' = 15)) +
  theme_xylab() +
  ylab('')

ggsave('Ghana_disag_sex.pdf', width = widthPlot,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# Secondary breakdown: region + sex ---------------------------------------

regions = ed %>% 
  filter(category == 'Region and Sex') 

regionOrder = regions %>% 
  select(category, sex, mean, region) %>% 
  spread(sex, mean) %>% 
  arrange((Male))

regionOrder$region = factor(regionOrder$region, levels = regionOrder$region)

ggplot(regions, aes(x = mean, y = region)) +
  geom_vline(xintercept = countryTotal$mean, colour = grey90K, size = 0.15) +
  geom_segment(aes(x = Male, xend = Female, y = region, yend = region), 
               colour = grey90K, size = 0.2, data = regionOrder) +
  geom_point(aes(colour = sex, shape = sex), size = sizeDot) +
  scale_colour_manual(values = c('Male' = maleColor, 'Female' = femColor)) +
  scale_x_continuous(labels = scales::percent, limits = c(xMin, xMax)) +
  scale_shape_manual(values = c('Male' = 16, 'Female' = 15)) +
  theme_xylab() +
  ylab('')

ggsave('Ghana_disag_geo.pdf', width = widthPlot,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# combine with DHS --------------------------------------------------------

dhs = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/Ghana_DHS_stunting_2014.xlsx') %>% 
  mutate(stunted = Value / 100)

  dhs_edu = left_join(regionOrder, dhs, by = c("region" = "Characteristic Label")) %>% 
    dplyr::select(region, Female, Male, stunted) %>% 
    gather(indicator, value, -region)
  
  dhs_edu$region = factor(dhs_edu$region, levels = regionOrder$region)

  
  ggplot(dhs_edu, aes(x = indicator, y = region, fill = value, label = percent(value, 0))) +
    geom_tile(colour= 'white') +
    scale_fill_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
    geom_text(colour = 'white', family = 'Segoe UI', size = 4) +
    theme_xylab()
  
  ggplot(dhs_edu, aes(x = indicator, y = region, fill = value, label = percent(value, 0))) +
    geom_tile(colour= 'white') +
    scale_fill_gradientn(colours = (brewer.pal(9, 'RdPu')[3:9])) +
    geom_text(colour = 'white', family = 'Segoe UI', size = 4) +
    theme_xylab()
  