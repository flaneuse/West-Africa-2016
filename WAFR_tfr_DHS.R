tfr = read.csv('data/WFAR_tfr.csv') %>% 
  filter(year == '2010-2015') %>% 
  select(country, tfr) %>% 
  mutate(country = as.character(country),
         region = ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire",
                         country))
  
natl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')

natl = left_join(natl, tfr, by = c('CountryName' = 'region'))


df = natl %>% 
  filter(IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                            'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  select(CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)


# desires -----------------------------------------------------------------


ggplot(df, aes(x = FP_CUSA_W_MOD, y = 100- PR_DESL_W_WNM,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 2, size = 5) + 
  geom_point() +
  xlab('modern contraception use in all women') +
  ggtitle('')+
  ylab('percent of women desiring more children')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(fill = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.1, 0.1))



# tfr ---------------------------------------------------------------------


ggplot(df, aes(x = FP_CUSA_W_MOD, y = 100- PR_DESL_W_WNM,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 2, size = 5) + 
  geom_point() +
  xlab('modern contraception use in all women') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(fill = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.1, 0.1))
