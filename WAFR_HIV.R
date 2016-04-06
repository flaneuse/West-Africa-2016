library(llamar)
loadPkgs()

natl_wide = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl_wide.csv')
natl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')
subnatl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_subnatl.csv')

# HIV prevalence ----------------------------------------------------------

hivBySex = natl_wide %>% 
  ungroup() %>% 
  filter(IndicatorId %in% c('HA_HIVP_M_HIV', 'HA_HIVP_W_HIV')) %>% 
  select(group = IndicatorId, country = CountryName, `HIV rate` = value2, year = year2) %>% 
  group_by(country) %>%
  mutate(`HIV rate` = `HIV rate` /100,
         diff = `HIV rate` - lag(`HIV rate`),
         diff = ifelse(is.na(diff), 0, diff),
         group = ifelse(group == 'HA_HIVP_M_HIV', 'men',
                        ifelse(group == 'HA_HIVP_W_HIV', 'women', NA))
  )
# 
# hivBySex = natl_wide %>%
#   ungroup() %>%
#   filter(IndicatorId %in% c('HA_HIVP_M_HIV', 'HA_HIVP_W_HIV')) %>%
#   select(IndicatorId, CountryName, value2, year2) %>%
#   spread(IndicatorId, value2) %>%
#   mutate(diff = HA_HIVP_W_HIV - HA_HIVP_M_HIV)
# 
order = hivBySex %>%
  ungroup() %>%
  arrange(`HIV rate`) %>%
  select(country)
# 
order = unique(order$country)
# 
# hivBySex$CountryName = factor(hivBySex$CountryName, levels = order)
# 
colourM = '#4575b4'
colourF = '#ce1256'

# write.csv(hivBySex, '~/Documents/USAID/West Africa Regional 2016/dataout/HIVbySex.csv')

# -- Difference in most recent HIV rates by M/F --
ggplot(hivBySex, aes(x = diff, y = CountryName, 
                     xend = 0, yend = CountryName,
                     size = HA_HIVP_W_HIV)) +
  geom_segment(size = 0.5, colour = grey50K) +
  geom_point(colour = colourF) +
  geom_text(aes(label = year2, x = -0.2),
            vjust = 0, nudge_y = -0.1,
            colour = grey50K, size = 4, family = 'Segoe UI Light') +
  geom_text(aes(label = paste0(HA_HIVP_W_HIV, '%')),
            colour = colourF, size = 4, family = 'Segoe UI Light', nudge_y = 0.3) +
  geom_text(aes(x = 0,
                label = paste0(HA_HIVP_M_HIV, '%')),
            colour = colourM, size = 4, family = 'Segoe UI Light', nudge_y = 0.3) +
  geom_point(aes(x = 0, size = HA_HIVP_M_HIV), colour = colourM) +
  theme_labelsOnly()


# -- HIV trends over time --

smMult = data.frame(CountryName = c("Burkina Faso",  "Cote d'Ivoire",
                                    "Cameroon",      "Ghana",         "Guinea",
                                    "Liberia", "Mali",  
                                    "Niger",
                                    "Sierra Leone",  "Senegal", "Togo"),
                    smCols = c(1, 2, 1, 1, 2, 4, 4, 3, 3, 2, 3),
                    smRows = c(3, 1,1,2,2,1,2,3,2, 3, 1))

hivTrends = natl %>% 
  filter(IndicatorId %in% c('HA_HIVP_M_HIV', 'HA_HIVP_W_HIV')) %>% 
  mutate(`HIV rate` = Value /100,
         IndicatorId = ifelse(IndicatorId == 'HA_HIVP_M_HIV', 'men',
                        ifelse(IndicatorId == 'HA_HIVP_W_HIV', 'women', NA)))

hivTrends = full_join(hivTrends, smMult, by = 'CountryName')

write.csv(hivTrends, '~/Documents/USAID/West Africa Regional 2016/dataout/HIVtrends.csv')


hivTrends$CountryName = factor(hivTrends$CountryName, levels = rev(order))


ggplot(hivTrends, aes(x = SurveyYear, y = Value, 
                      colour = Indicator)) +
  geom_line() +
  geom_point(size = 6, colour = 'white') +
  geom_point(size = 3) +
  geom_text(aes(label = SurveyYear), 
            data = hivTrends %>% filter(IndicatorId == 'HA_HIVP_M_HIV'),
            colour = grey50K, nudge_y = -0.5) +
  geom_linerange(aes(ymin = CILow, ymax = CIHigh), alpha = 0.5, size = 2) +
  scale_colour_manual(values = c(colourM, colourF)) +
  facet_wrap(~CountryName, ncol = 3) +
  theme_ygrid() +
  coord_cartesian(ylim = c(0, 7)) +
  scale_x_continuous(limits = c(2003, 2014),
                     breaks = c(2003, 2008, 2013))

# -- HIV disaggregation --
hivDisag = subnatl %>% 
  filter(IndicatorId %in% c('HA_HIVP_M_HIV', 'HA_HIVP_W_HIV'))

hivDisag$CountryName = factor(hivDisag$CountryName, levels = rev(order))

cats = hivDisag %>%
  group_by(CharacteristicCategory) %>% 
  summarise(num = n()) %>% 
  select(CharacteristicCategory)

cats = cats$CharacteristicCategory

for (i in seq_along(cats)){
  print(i)
  print(ggplot(hivDisag%>% filter(
                            CharacteristicCategory == cats[i]), 
         aes(x = SurveyYear, y = Value, 
             colour = CharacteristicLabel,
             group = CharacteristicLabel)) +
    geom_line() +
    geom_point(size = 6, colour = 'white') +
    geom_point(size = 3) +
    # geom_text(aes(label = SurveyYear), 
    # data = hivTrends %>% filter(IndicatorId == 'HA_HIVP_W_HIV'),
    # colour = grey50K, nudge_y = -0.5) +
    geom_linerange(aes(ymin = CILow, ymax = CIHigh), alpha = 0.5, size = 2) +
    # scale_colour_manual(values = c(colourM, colourF)) +
    facet_wrap(~CountryName + IndicatorId) +
    theme_ygrid() +
    theme(legend.position = 'bottom')+
    coord_cartesian(ylim = c(0, 10)) +
    scale_x_continuous(limits = c(2003, 2014),
                       breaks = c(2003, 2008, 2013))
  
  )
}

# write.csv(hivBySex, '~/Documents/USAID/West Africa Regional 2016/dataout/HIVbySex.csv')

