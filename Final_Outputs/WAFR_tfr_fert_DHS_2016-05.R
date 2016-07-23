setwd("~/WAFR_HealthPAD")
install.packages("devtools")
library(devtools)
# devtools::install_github("flaneuse/llamar")
library(llamar)
loadPkgs()

# install.packages('tidyr')
# install.packages('RColorBrewer')
# install.packages('ggplot2')

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

natl = read.csv("~/WAFR_HealthPAD/WAFR_DHS_natl.csv")

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
               size = unmet,
               label = CountryName)) +
  geom_label(nudge_y = 0.3, size = 5,
             family = 'Calibri') + 
  geom_point(colour='grey') +
  geom_point(aes(colour = contra), data =df %>% filter(CountryName == 'Togo')) +
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

ggsave("WAFR_FamilyPlanning.pdf",width = 20, height = 10, units = c("in"),
       dpi=300, limitsize = TRUE)
