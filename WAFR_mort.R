library(llamar)
loadPkgs()

natl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')

df = natl %>% 
  filter(Indicator %in% c('Infant mortality rate', 'Under-five mortality rate (5 year periods)',
                          'Maternal mortality rate')) %>% 
  select(country = CountryName, year = SurveyYear, IndicatorId, Value) %>% 
  spread(IndicatorId, Value) %>% 
  rename(infant = CM_ECMR_C_IMR, 
         under5 = CM_ECMT_C_U5M,
         maternal = MM_MMRT_W_MRT) %>% 
  group_by(country) %>% 
  mutate(yearNum = dense_rank(year))

order = df %>% 
  arrange(desc(infant))

order = unique(order$country)

df$country= factor(df$country, levels = order)

  

ggplot(df, aes(x = infant, y = under5, 
               alpha = year, group = country)) +
  geom_line(colour = 'dodgerblue') + 
  geom_point(size = 4, colour = 'white', alpha = 1) +
  geom_point(size = 4, colour = 'dodgerblue') +
  # scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
  facet_wrap(~country) +
  theme_xygrid()

ggplot(df, aes(x = maternal, y = under5, 
               alpha = year, group = country)) +
  geom_line(colour = 'dodgerblue') + 
  geom_point(size = 4, colour = 'white', alpha = 1) +
  geom_point(size = 4, colour = 'dodgerblue') +
  # scale_colour_gradientn(colours = brewer.pal(9, 'YlGnBu')) +
  facet_wrap(~country) +
  theme_xygrid()