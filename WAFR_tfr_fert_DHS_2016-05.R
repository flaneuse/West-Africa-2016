library(llamar)
loadPkgs()


natl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')



df = natl %>% 
  filter(IndicatorId %in% c('FP_NADM_W_UNT', 'FP_CUSM_W_MOD',
                            'FE_FRTR_W_TFR'),
         SurveyYear >= 2010 | SurveyYear == 2000,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  select(CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value) %>% 
  filter(SurveyYear != 2012 | CountryName != 'Senegal',
         SurveyYear != 2010 | CountryName != 'Senegal',
         CountryName != 'Mauritania') %>% 
  rename(contra = FP_CUSM_W_MOD,
         unmet = FP_NADM_W_UNT,
         tfr = FE_FRTR_W_TFR) %>% 
  select(-SurveyYear)

newData = data.frame(CountryName = c('Mauritania', 'Guinea-Bissau', 'Cape Verde'), 
                     tfr = c(4.7, 4.9, 2.3),
                     contra =c(10, 10.3, 57.1),
                     unmet = c(31.1, 6, 16.7)
)

df = rbind(df, newData)


# Plot TFR ---------------------------------------------------------------


ggplot(df, aes(x = contra, y = tfr,
               colour = unmet,
               size = unmet,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 5,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('modern contraception use in married women') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(4, 35)) +
  scale_y_continuous(limits = c(4, 8),
                     breaks = c(4, 6,8)) +
  theme_xygrid() +
  theme(panel.grid.major.x = element_line(size = 0.1),
        panel.grid.major.y = element_line(size = 0.1))+
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.9, 0.9))+
  coord_cartesian(xlim = c(4, 24))

ggsave("~/GitHub/West Africa 2016/plots/2016-05-05_TFR.pdf",
       width = 4*1.5, height = 4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)