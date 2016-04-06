library(llamar)
loadPkgs()


# Static things -----------------------------------------------------------

accentColor = '#b2182b'

refYr = '1980-1985'

# Load Data ---------------------------------------------------------------


tfr = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WPP_TFR.xlsx',
                    sheet = 1)

prb = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WAFR_PRB_TFR.xlsx') %>% 
  mutate(prb = as.numeric(Data)
         # country = rem
         # country = ifelse(country == 'Cape Verde', 'Cabo Verde', country)
         ) %>% 
  filter(TimeFrame == 2014) %>% 
  select(Location, prb)



# Wrangle -----------------------------------------------------------------
# Pull out the regional numbers
regionalTFR = tfr %>% 
  select(year,
         tfrWAfr = `Western Africa`) %>% 
  mutate(laggedWAfr = lag(tfrWAfr),
         rateWAfr = (tfrWAfr - laggedWAfr)/laggedWAfr)


# Tidy
tfr = tfr %>% 
  gather(country, tfr, -year) %>% 
  filter(!(country %in% c('Sub-Saharan Africa', 'AFRICA', 'Western Africa'))) %>% 
  group_by(country) %>% 
  mutate(lagged = lag(tfr),
         rate = (tfr - lagged)/lagged
         )


# Pull out the reference number
refTFR = tfr %>% 
  filter(year == refYr) %>% 
  select(country, refTFR = tfr)


tfr = full_join(tfr, regionalTFR) %>% 
  mutate(diffRate = rate-rateWAfr)

tfr = left_join(tfr, prb, by = c("country" = "Location"))

tfr = full_join(tfr, refTFR, by = 'country')


order = tfr %>% 
  ungroup() %>% 
  filter(year =='2010-2015') %>% 
  arrange(desc(tfr))

tfr$country = factor(tfr$country,
                     levels  = order$country)


# save --------------------------------------------------------------------
tfr = tfr %>%
  select(-lagged,-laggedWAfr) %>%
  mutate(refRate = ifelse(year == '2010-2015',
                          (tfr - refTFR) / refTFR, NA))

write.csv(tfr, '~/Documents/USAID/West Africa Regional 2016/dataout/WFAR_tfr.csv')


# TFR over time -----------------------------------------------------------

ggplot(tfr, aes(x = year, y = tfr, group = country)) +
  geom_line(colour = grey50K,
            aes(y = tfrWAfr)) +
  geom_line(colour = accentColor) +
  geom_point(colour = accentColor,
             data = tfr %>% filter(year == '2010-2015'))+
  geom_point(aes(y = prb),
             data = tfr %>% filter(year == '2010-2015'),
             colour = 'dodgerblue')+
  geom_text(aes(label = round(tfr,1)), 
            colour = accentColor,
            nudge_y = -1,
            hjust = 1,
            data = tfr %>% filter(year == '2010-2015')) +
  geom_text(aes(label = round(prb,1)), 
            colour = 'dodgerblue',
            nudge_y = -1,
            hjust = 1,
            data = tfr %>% filter(year == '2010-2015')) +
  facet_wrap(~country) +
  scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                              '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                   labels = c('1950-1955','',  '',
                              '1980-1985', 
                              '','','2010-2015')) +
  theme_xygrid() + 
  scale_y_continuous(limits = c(0,8)) +
  ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
  theme(axis.text.x = element_text(size = 10),
        panel.margin = unit(1, 'lines'))


# TFR rate over time ------------------------------------------------------

ggplot(tfr, aes(x = year, y = rate, group = country)) +
  geom_line(colour = grey50K,
            aes(y = rateWAfr)) +
  geom_line(colour = accentColor) +
  geom_ribbon(aes(ymin = rateWAfr, ymax = rate),
              fill = 'blue',
              alpha = 0.15) +
  geom_point(colour = accentColor,
             data = tfr %>% filter(year == '2010-2015'))+
  facet_wrap(~country) +
  scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                              '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                   labels = c('1950-1955','',  '',
                              '1980-1985', 
                              '','','2010-2015')) +
  theme_xygrid() + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
  theme(axis.text.x = element_text(size = 10),
        panel.margin = unit(1, 'lines'))

