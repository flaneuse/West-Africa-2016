library(llamar)
loadPkgs()

tfr = read.csv('~/GitHub/West Africa 2016/WestAfrica/data/WFAR_tfr.csv') %>% 
  filter(year == '2010-2015') %>% 
  select(country, tfr) %>% 
  mutate(country = as.character(country),
         region = ifelse(country == "Côte d'Ivoire", "Cote d'Ivoire",
                         country))

natl = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')



df = natl %>% 
  filter(IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                            'FP_CUSA_W_MOD', 'PR_DESL_M_WNM'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  select(CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value) %>% 
  filter(SurveyYear != 2012 | CountryName != 'Senegal') %>% 
  mutate(diffBabies = - PR_DESL_M_WNM + PR_DESL_W_WNM)

df = left_join(df, tfr, by = c('CountryName' = 'region'))


# desires -----------------------------------------------------------------


ggplot(df, aes(x = FP_CUSA_W_MOD, y = 100- PR_DESL_W_WNM,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 2, size = 5,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('modern contraception use in all women') +
  ggtitle('')+
  ylab('percent of women desiring more children')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.1, 0.1))

ggsave("~/GitHub/babies_contraception_unmet.pdf",
       width = 6, height = 4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)



# tfr ---------------------------------------------------------------------


ggplot(df, aes(x = FP_CUSA_W_MOD, y = tfr,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 5,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('modern contraception use in all women') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.9, 0.9))

ggsave("~/GitHub/tfr_contraception_unmet.pdf",
       width = 6, height = 4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# TFR babies --------------------------------------------------------------

ggplot(df, aes(x = 100- PR_DESL_W_WNM, y = tfr,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 3,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('percent of women desiring more children') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.1, 0.9))

ggsave("~/GitHub/tfr_babies_unmet.pdf",
       width = 6, height = 4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

# tfr unmet -----------------------------------------------------------


ggplot(df, aes(x = 100- FP_NADM_W_UNT, y = tfr,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 3,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('unmet neet') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = 'none')

ggsave("~/GitHub/tfr_unmet.pdf",
       width = 6, height = 4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# gap: men/women ----------------------------------------------------------
ggplot(df, aes(x = diffBabies, y = tfr,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 3,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('difference between male and female desires for more children') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(guide = 'none') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         name = 'unmet need',
                         limits = c(7, 35)) +
  theme_xygrid() +
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.9, 0.9))




# babies, tfr, colored by women -------------------------------------------


ggplot(df, aes(x = diffBabies, y = tfr,
               colour = PR_DESL_W_WNM,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 3,
             family = 'Segoe UI Semilight') + 
  geom_point() +
  xlab('difference between male and female desires for more children') +
  ggtitle('')+
  ylab('total fertility rate')+
  scale_size(name = 'unmet need') +
  scale_colour_gradientn(colours = rev(brewer.pal(9, 'RdPu')),
                         name = 'W. baby wants',
                         limits = c(7, 40)) +
  facet_wrap(~CountryName) + 
  theme_xygrid() +
  guides(colour = guide_colorbar(ticks = FALSE))+ 
  theme(legend.position = c(0.9, 0.9))


plot_tooltip = function(x) {
  if(is.null(x)) return(NULL)
  paste0(x$diffBabies, ';', x$tfr)
}

yLim = c(4, 8)
yVar = 'total fertility rate'
xVar = 'difference in baby desires'

widthPlot = 'auto'
heightPlot = 'auto'

df %>% 
  ggvis(x = ~diffBabies, y = ~tfr,
        fill = ~PR_DESL_W_WNM) %>% 
  set_options(height = heightPlot, width = widthPlot) %>%
  layer_points(size = ~PR_DESL_W_WNM,
               stroke := grey60K, stroke.hover := 'red',
               fill.hover := "red",
               fillOpacity := 1, fillOpacity.hover := 0.5) %>%
  layer_text(text := ~CountryName,
             fontSize := 16,
             fontWeight := 300,
             align := 'center',
             baseline:="top",
             dy := 10,
             font := 'Segoe UI') %>%
  add_axis(type = 'y',
           title = yVar,
           tick_size_major = 0,
           properties = axis_props(
             axis = list(strokeWidth = 0),
             title = list(fontSize = 18,
                          fill = grey70K,
                          fontWeight = 300, 
                          font = 'Segoe UI'),
             labels = list(fontSize = 16,
                           fill = grey60K,
                           fontWeight = 200,
                           font = 'Segoe UI'))) %>% 
             # grid = list(
               # strokeWidth = 0)
           # )) %>% 
  add_axis(type = 'x', 
           tick_size_major = 0,
           title = xVar,
           properties = axis_props(
             axis = list(strokeWidth = 0),
             title = list(fontSize = 18,
                          fill = grey70K,
                          fontWeight = 300, 
                          font = 'Segoe UI'),
             labels = list(fontSize = 16,
                           fill = grey60K,
                           fontWeight = 200,
                           font = 'Segoe UI'))) %>% 
             # grid = list(strokeWidth = 0))) %>% 
  scale_numeric("y", domain = yLim) %>% 
  add_tooltip(plot_tooltip, "hover") 


