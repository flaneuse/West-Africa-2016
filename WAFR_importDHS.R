
# Import DHS data ---------------------------------------------------------



# Common indicators
# Unmet need total (iCE37FE2)
# Total fertility rate (i133C8F8)
# Median duration of exclusive breastfeeding (i5716309)
# Women receiving an HIV test and receiving test results in the last 12 months (i42759492)
# Children underweight (iCC97002)
# Households with electricity (i7C3CB1)
# Children under 5 who slept under an insecticide treated net (ITN) (iC6D4E13)
# Infant mortality rate (i42FFDB2)
# Men receiving an HIV test and receiving test results in the last 12 months (i42759491)
# HIV prevalence among general population (i41075E93)
# HIV prevalence among women (i41075E92)
# HIV prevalence among men (i41075E91)
# Under-five mortality rate (i42FFDB4)
# Received all vaccinations (i4E75F0A)
# Treatment of diarrhea: Either ORS or RHS (i533BDCB)
# Median age at first marriage: 25-49 (i349C430)
# Children wasted (iCC973EA)
# Women with secondary or higher education (iA999FA)
# Place of delivery: Health facility (i49B3AD0)
# Children stunted (iCC96C1A)
# Married women currently using any modern method of contraception (i209E191)
# Married women currently using any method of contraception (i209E190)
# Median age at first sexual intercourse: 25-49 (i36848B0)
# Women who are literate (i797B5DA)


library(llamar)
loadPkgs()


# DHS ---------------------------------------------------------------------


# set up the queries ------------------------------------------------------

apiKey = 'USAAID-405632'
indicator = c('FP_NADM_W_UNT','FE_FRTR_W_TFR','CN_BFDR_C_MDE','HA_CPHT_W_T1R','HC_ELEC_H_ELC',
              'CN_NUTS_C_WA2','HA_HIVP_W_HIV','HA_HIVP_W_HVE',
              'HA_HIVP_B_HIV','HA_HIVP_B_HVE','CM_ECMT_C_U5M','CM_ECMR_C_IMR',
              'CM_ECMR_C_NNR','CH_VACS_C_BAS','CH_VACS_C_NON','CH_VACC_C_BAS','CH_VACC_C_NON',
              'CH_VAC1_C_BAS','CH_VAC1_C_NON','CH_DIAT_C_ORT','CH_DIAR_C_DIA','MA_AAFM_W_M20',
              'MA_AAFM_W','MA_AAFM_W','MA_AAFM_W','MA_AAFM_W','MA_AAFM_W','MA_AAFM_W','MA_AAFM_W_M2','SX_AAFS_W_M20',
              'HA_HIVP_M_HVE','HA_HIVP_M_HIV',
              'CN_NUTS_C_WH2','CN_NUTS_C_HA2','ED_EDUC_W_SEP',
              'CM_ECMR_C_PNR','FP_CUSA_W_MOD','FP_CUSM_W_ANY','FP_CUSM_W_MOD',
              'SX_AAFS_W_M25','SX_AAFS_W_M30','SX_AAFS_W_M35','SX_AAFS_W_M40','SX_AAFS_W_M45',
              'SX_AAFS_W_M2A','SX_AAFS_W_M2B','ED_LITR_W_LIT','RH_PAHC_W_KNW','RH_PAHC_W_PRM',
              'RH_PAHC_W_MON','RH_PAHC_W_DIS','RH_PAHC_W_TRN','RH_PAHC_W_ALN','RH_PAHC_W_FEM',
              'MM_MMRT_W_MRT','MM_MMRO_W_GFR','MM_MMRO_W_MMR','MM_MMRO_W_CIL','MM_MMRO_W_CIH',
              'RH_DELP_C_DHF','FP_CUSA_W_ANY', 'PR_DESL_W_WNM', 'PR_DESL_M_WNM')

indicators = paste0(indicator, collapse=',')

countries = 'BJ,BF,CM,CI,CV,GH,GN,GM,LB,ML,MR,NI,NG,SN,SL,TG'

# years = paste0(seq(1980,2016), collapse = ',')
years = paste0(seq(2000,2016), collapse = ',')


# Testing year limits -----------------------------------------------------


# Figure out where to filter the years of DHS data
json_file <- fromJSON(paste0('http://api.dhsprogram.com/rest/dhs/data?breakdown=national&indicatorIds=',indicators, 
                             '&countryIds=', countries, 
                             '&SurveyYear=', years,
                             '&apiKey=', apiKey,
                             '&perpage=5000'))

# Unlist the JSON file entries
json_data <- lapply(json_file$Data, function(x) { unlist(x) })

# Convert JSON input to a data frame
allData <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE)



svyYrs = allData %>% 
  group_by(SurveyYear, CountryName, SurveyType) %>% 
  summarise(num  = n()) %>% 
  mutate(year = as.integer(SurveyYear))

ggplot(svyYrs, aes(x = year, y = CountryName, size = num)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1986, 2016, by = 4)) +
  theme_xgrid()



svyYrs$CountryName = factor(svyYrs$CountryName,
                            levels = rev(unique(sort(svyYrs$CountryName))))

ggplot(svyYrs, aes(x = year, 
                   y = CountryName,
                   label= year,
                   colour = SurveyType)) +
  geom_label(size = 5)+
  scale_x_continuous(limits = c(2000, 2016),
                     breaks = seq(2000, 2016, by = 2)) +
  theme_ygrid() +
  # scale_y_reverse()+
  xlab('') + ylab('') +
  ggtitle('Available Demographic and Health Surveys data')

ggsave("~/GitHub/WAfrica_DHSdata.pdf",
       width = 6, height = 4,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


years = paste0(seq(2000,2016), collapse = ',')


# Import DHS --------------------------------------------------------------


# Import DHS Indicator National-level data

natl = loadDHS(breakdown = 'national', indicators = indicators, 
               countries = countries, years = years, apiKey = apiKey, numResults = 5000)


subnatl = NULL

for (i in seq_along(indicator)) {
  print(i)
  
  temp = loadDHS(breakdown = 'all', indicators = indicator[i], countries = countries, 
                 years = years, apiKey = apiKey, numResults = 5000)
  
  subnatl = rbind(temp, subnatl)  
}



write.csv(natl, '~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')
write.csv(subnatl, '~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_subnatl.csv')

ggplot(natl %>% filter(Indicator %like% 'Maternal mortality ratio'), aes(x = SurveyYear, y = Value)) +
  geom_point(size = 5) +
  facet_wrap(~CountryName) +
  scale_y_continuous(limits = c(0, 1200))


# reshape data ------------------------------------------------------------
natl_wide = natl %>% 
  arrange(SurveyYear) %>% 
  group_by(CountryName, IndicatorId, Indicator, ByVariableLabel) %>%
  summarise(value1 = first(Value),
            value2 = last(Value),
            year1 = first(SurveyYear),
            year2 = last(SurveyYear))
  
write.csv(natl_wide, '~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl_wide.csv')



# basic comparative plot --------------------------------------------------


df = natl %>% filter(Indicator %like% 'Unmet need for family planning')

mostRecent = df %>% 
  group_by(CountryName) %>% 
  summarise(SurveyYear = max(SurveyYear))

avg = left_join(mostRecent, df) 

wAfrAvg = mean(avg$Value)

avg = avg %>% 
  mutate(deviation = Value - wAfrAvg)

df = left_join(df, avg %>% select(CountryName, deviation))

order = df %>% 
  arrange(desc(deviation)) %>% 
  select(CountryName)

order = unique(order$CountryName)

df$CountryName = factor(df$CountryName, levels = order)

ggplot(df, 
       aes(x = SurveyYear, y = Value, 
           group = CountryName,
           colour = deviation)) +
  # geom_hline(yintercept = wAfrAvg, colour = grey80K, size = 0.5) +
  geom_point(size = 3) +
  geom_text(aes(label = paste(round(Value,0),'%')), size = 3.5,
            nudge_y = -7, colour = grey90K, check_overlap = TRUE) + 
  geom_line() +
  scale_colour_gradientn(colours = rev(brewer.pal(11, 'RdYlBu'))) +
  facet_wrap(~CountryName) +
  scale_y_continuous(limits = c(0, 36), breaks = c(0, 15, 30)) +
  ggtitle('Unmet need for family planning has been fairly constant, aside from in Benin') +
  theme_xygrid() +
  ylab('')+
  theme(title = element_text(size = 14),
    rect = element_rect(fill = grey10K, colour = grey10K, linetype = 1, size =0),
        panel.background = element_rect(fill= grey10K),
        panel.margin = unit(1, 'lines'))



# contraception -----------------------------------------------------------


df = natl %>% filter(Indicator %like% 'Current use of any modern method')

mostRecent = df %>% 
  group_by(CountryName) %>% 
  summarise(SurveyYear = max(SurveyYear))

avg = left_join(mostRecent, df) 

wAfrAvg = mean(avg$Value)

avg = avg %>% 
  mutate(deviation = Value - wAfrAvg)

df = left_join(df, avg %>% select(CountryName, deviation))

order = df %>% 
  arrange(desc(deviation)) %>% 
  select(CountryName)

order = unique(order$CountryName)

df$CountryName = factor(df$CountryName, levels = order)

ggplot(df, 
       aes(x = SurveyYear, y = Value, 
           group = CountryName,
           colour = deviation)) +
  # geom_hline(yintercept = wAfrAvg, colour = grey80K, size = 0.5) +
  geom_point(size = 3) +
  geom_line() +
  scale_colour_gradientn(colours = (brewer.pal(11, 'RdYlBu'))) +
  facet_wrap(~CountryName) +
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 10, 20)) +
  theme_xygrid() +
  geom_text(aes(label = paste(round(Value,0),'%')), size = 3.5,
            nudge_y = -4, colour = grey90K, check_overlap = TRUE) + 
  theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
        panel.background = element_rect(fill= grey15K)) +
  ylab('')+
  ggtitle('Modern contraception use in all women is low but increasing') +
  theme(title = element_text(size = 14),
        rect = element_rect(fill = grey10K, colour = grey10K, linetype = 1, size =0),
        panel.background = element_rect(fill= grey10K),
        panel.margin = unit(1, 'lines'))




ggplot(df, 
       aes(y = CountryName, x = Value, 
           group = CountryName,
           colour = SurveyYear)) +
  geom_vline(xintercept = wAfrAvg, colour = grey80K, size = 0.5) +
  geom_point(size = 3) +
  geom_line() +
  scale_colour_gradientn(colours = rev(brewer.pal(11, 'RdYlBu'))) +
  theme_xgrid() +
  scale_x_continuous(limits = c(0, 40)) +
  theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
        panel.background = element_rect(fill= grey10K))

# baby desires -----------------------------------------------------------


df = natl %>% filter(Indicator %like% 'want no more',
                     ByVariableLabel == 'Total')

mostRecent = df %>% 
  group_by(CountryName) %>% 
  summarise(SurveyYear = max(SurveyYear))

avg = left_join(mostRecent, df) 

wAfrAvg = mean(avg$Value)

avg = avg %>% 
  mutate(deviation = Value - wAfrAvg)

df = left_join(df, avg %>% select(CountryName, deviation))

order = df %>% 
  arrange(desc(deviation)) %>% 
  select(CountryName)

order = unique(order$CountryName)

df$CountryName = factor(df$CountryName, levels = order)

ggplot(df, 
       aes(x = SurveyYear, y = Value, 
           group = Indicator,
           colour = deviation)) +
  geom_hline(yintercept = wAfrAvg, colour = grey80K, size = 0.5) +
  geom_point(size = 3) +
  geom_line() +
  scale_colour_gradientn(colours = (brewer.pal(11, 'RdYlBu'))) +
  # geom_text(aes(label = IndicatorId)) +
  facet_wrap(~CountryName) +
  scale_y_continuous(limits = c(0, 70)) +
  theme_xygrid() +
  theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
        panel.background = element_rect(fill= grey15K))

df = natl %>% filter(Indicator %like% 'want no more')

mostRecent = df %>% 
  group_by(CountryName) %>% 
  summarise(SurveyYear = max(SurveyYear))

avg = left_join(mostRecent, df) 

wAfrAvg = mean(avg$Value)

avg = avg %>% 
  mutate(deviation = Value - wAfrAvg)

df = left_join(df, avg %>% select(CountryName, deviation))

order = df %>% 
  arrange(desc(deviation)) %>% 
  select(CountryName)

order = unique(order$CountryName)

df$CountryName = factor(df$CountryName, levels = order)

ggplot(df, 
       aes(x = SurveyYear, y = Value, 
           group = ByVariableLabel,
           colour = ByVariableLabel)) +
  geom_hline(yintercept = wAfrAvg, colour = grey80K, size = 0.5) +
  geom_point(size = 3) +
  geom_line() +
  # scale_colour_gradientn(colours = (brewer.pal(11, 'RdYlBu'))) +
  # geom_text(aes(label = IndicatorId)) +
  facet_wrap(~CountryName + Indicator, scales = 'free_y')
  theme_xygrid() +
  theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
        panel.background = element_rect(fill= grey15K))

#----
  
  df = natl %>% filter(Indicator %like% 'want no more')
  
  mostRecent = df %>% 
    group_by(CountryName) %>% 
    summarise(SurveyYear = max(SurveyYear))
  
  avg = left_join(mostRecent, df) 
  
  
  
  order = df %>% 
    arrange(desc(deviation)) %>% 
    select(CountryName)
  
  order = unique(order$CountryName)
  
  df$CountryName = factor(df$CountryName, levels = order)
  
  ggplot(df, 
         aes(x = SurveyYear, y = Value, 
             group = ByVariableLabel,
             colour = ByVariableLabel,
             shape = IndicatorId)) +
    geom_point(size = 3) +
    geom_line() +
    facet_wrap(~CountryName, scales = 'free_y') +
  theme_xygrid() +
    theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
          panel.background = element_rect(fill= grey15K))
  
  df2 = df %>% 
    select(Value, CountryName, SurveyYear, IndicatorId, ByVariableLabel) %>% 
    spread(IndicatorId, Value) %>% 
    mutate(diffVal = - PR_DESL_M_WNM + PR_DESL_W_WNM)
  
  
  order = df2 %>% 
    filter(ByVariableLabel=='Total') %>% 
    arrange(desc(PR_DESL_W_WNM)) %>% 
    select(CountryName)
  
  order = unique(order$CountryName)
  
  df2$CountryName = factor(df2$CountryName, levels = order)
  
  
  ggplot(df2, 
         aes(x = SurveyYear, y = diffVal, 
             group = ByVariableLabel,
             colour = ByVariableLabel)) +
    geom_point(size = 3) +
    geom_line() +
    facet_wrap(~CountryName) +
    theme_basic() +
    theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
          panel.background = element_rect(fill= grey15K))

  
  ggplot(df2 %>% filter(ByVariableLabel != 'Total'), 
         aes(x = 100 - PR_DESL_M_WNM, y = diffVal, 
             group = ByVariableLabel,
             colour = ByVariableLabel)) +
    geom_point(size = 3) +
    facet_wrap(~CountryName) +
    theme_basic() +
    theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
          panel.background = element_rect(fill= grey15K))
  
  


  #!!
  ggplot(df2 %>% filter(ByVariableLabel != 'Total'), 
         aes(x = 100 - PR_DESL_M_WNM, y = 100 - PR_DESL_W_WNM, 
             group = ByVariableLabel,
             colour = ByVariableLabel)) +
    geom_abline(slope = 1, intercept = 0,
                colour = grey40K, size = 0.5) +
    geom_point(size = 3) +
    facet_wrap(~CountryName) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))+
    theme_xygrid() + 
    ggtitle('Men and women have different desires for more children') +
    xlab('percent of men wanting more children') +
    ylab('percent of women wanting more children') +
    theme(legend.position = c(0.8, 0.1),
          legend.direction = 'horizontal',
          panel.margin = unit(1, 'lines'))
  
  ggplot(df2 %>% filter(CountryName == 'Ghana',
    ByVariableLabel != 'Total'), 
         aes(x = PR_DESL_M_WNM, y = diffVal, 
             group = ByVariableLabel,
             colour = ByVariableLabel)) +
    geom_point(size = 3) +
    facet_wrap(~SurveyYear) +
    theme_xygrid() +
    theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
          panel.background = element_rect(fill= grey15K))

  
  ggplot(df2 %>% filter(CountryName == 'Senegal',ByVariableLabel != 'Total'), 
         aes(x = PR_DESL_M_WNM, y = diffVal, 
             group = ByVariableLabel,
             colour = ByVariableLabel)) +
    geom_point(size = 3) +
    facet_wrap(~SurveyYear) +
    theme_xygrid() +
    theme(rect = element_rect(fill = grey15K, colour = grey15K, linetype = 1, size =0),
          panel.background = element_rect(fill= grey15K))
  
  
  order = df2 %>% 
    filter(ByVariableLabel=='Total') %>% 
    arrange(desc(diffVal)) %>% 
    select(CountryName)
  
  order = unique(order$CountryName)
  
  df2$CountryName = factor(df2$CountryName, levels = order)
  
  ggplot(df2 %>% filter(ByVariableLabel == 'Total'), 
         aes(x = SurveyYear, y = diffVal, 
             group = CountryName)) +
    geom_area(fill = 'dodgerblue') +
    geom_bar(fill = 'dodgerblue', stat='identity') +
    facet_wrap(~CountryName) +
    theme_xygrid()
  

# WHO Global Health Observatory -------------------------------------------
x = fromJSON(paste0('http://apps.who.int/gho/athena/api/GHO/WHOSIS_000001.json'))
json_data <- lapply(x$dataset, function(x) { unlist(x) })

# Convert JSON input to a data frame
allData <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE)

library(XML)
# df <- xmlParse("http://apps.who.int/gho/athena/api/GHO/WHOSIS_000001?filter=COUNTRY:*;REGION:EUR")

# xml_data <- xmlToList(df)


# baby desires + contraception + unmet ------------------------------------


df = natl %>% 
  filter(IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                                        'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
                     ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  select(CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)

ggplot(df, aes(x = FP_CUSA_W_MOD, y = 100- PR_DESL_W_WNM,
               colour = FP_NADM_W_UNT,
               size = FP_NADM_W_UNT,
               label = CountryName)) +
  geom_label(nudge_y = 2, size = 5) + 
  geom_point() +
  xlab('modern contraception use in all women') +
  ggtitle('')+
  # coord_cartesian(xlim = c(0, 25), ylim = c(0, 100))+
  ylab('percent of women desiring more children')+
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         limits = c(7, 35)) +
  theme_xygrid() +
  theme(legend.position = c(0.1, 0.1))



df = subnatl %>% 
  filter(CountryName %in% c('Cameroon', "Cote d'Ivoire"),
    IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                       'FP_CUSM_W_MOD', 'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  mutate(id = paste0(DHS_CountryCode,CharacteristicId)) %>% 
  select(id, CharacteristicLabel, CharacteristicCategory, CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)

df = subnatl %>% 
  filter(CountryName %in% c('Benin', "Ghana"),
         IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                            'FP_CUSM_W_MOD', 'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  mutate(id = paste0(DHS_CountryCode,CharacteristicId)) %>% 
  select(id, CharacteristicLabel, CharacteristicCategory, CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)

df = subnatl %>% 
  filter(CountryName %in% c('Guinea', "Liberia"),
         IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                            'FP_CUSM_W_MOD', 'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  mutate(id = paste0(DHS_CountryCode,CharacteristicId)) %>% 
  select(id, CharacteristicLabel, CharacteristicCategory, CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)

df = subnatl %>% 
  filter(CountryName %in% c('Mali', "Sierra Leone"),
         IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                            'FP_CUSM_W_MOD', 'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  mutate(id = paste0(DHS_CountryCode,CharacteristicId)) %>% 
  select(id, CharacteristicLabel, CharacteristicCategory, CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)


df = subnatl %>% 
  filter(CountryName %in% c('Niger', "Nigeria"),
         IndicatorId %in% c('FP_NADM_W_UNT', 'PR_DESL_W_WNM',
                            'FP_CUSM_W_MOD', 'FP_CUSA_W_MOD'),
         SurveyYear > 2010,
         ByVariableLabel == 'Total' | ByVariableLabel == "") %>% 
  mutate(id = paste0(DHS_CountryCode,CharacteristicId)) %>% 
  select(id, CharacteristicLabel, CharacteristicCategory, CountryName, IndicatorId, Value, SurveyYear) %>% 
  spread(IndicatorId, Value)



ggplot(df %>% filter(!is.na(FP_NADM_W_UNT),
                     !is.na(FP_CUSM_W_MOD),
                     !is.na(PR_DESL_W_WNM)
                       ), aes(x = FP_CUSM_W_MOD, y = 100 - PR_DESL_W_WNM,
               colour = FP_NADM_W_UNT,
               label = CharacteristicLabel)) +
  geom_label(nudge_y = 2) + 
  geom_point(size = 5) +
  xlab('modern contraception use') +
  ylab('desires for more children')+
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu'),
                         limits = c(10, 50)) +
  facet_wrap(~CountryName + CharacteristicCategory) +
  theme_xygrid()



