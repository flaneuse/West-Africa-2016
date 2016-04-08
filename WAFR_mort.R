library(llamar)
loadPkgs()

natl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')

df = natl %>% 
  filter(Indicator %in% c('Infant mortality rate', 'Under-five mortality rate (5 year periods)')) %>% 
  select(country = CountryName, year = SurveyYear, IndicatorId, Value) %>% 
  spread(IndicatorId, Value) %>% 
  rename(infant = CM_ECMR_C_IMR, under5 = CM_ECMT_C_U5M)

order = df %>% 
  arrange(desc(infant))

order = unique(order$country)

df$country= factor(df$country, levels = order)

  

ggplot(df, aes(x = infant, y = under5, 
               alpha = year, group = country)) +
  geom_point(size = 4) +
  geom_line() + 
  # scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
  facet_wrap(~country) +
  theme_xygrid()