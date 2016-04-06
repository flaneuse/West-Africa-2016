library(llamar)
loadPkgs()

tfr = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WPP_TFR.xlsx',
                    sheet = 1)

prb = read.csv('~/Documents/USAID/West Africa Regional 2016/datain/WAFR_PRB_TFR.csv') %>% 
  mutate(prb = as.numeric(Data)
         # country = rem
         # country = ifelse(country == 'Cape Verde', 'Cabo Verde', country)
         )

accentColor = '#b2182b'

# Wrangle -----------------------------------------------------------------
# Pull out the regional numbers
regionalTFR = tfr %>% 
  select(year,
         tfrWAfr = `Western Africa`)

tfr = tfr %>% 
  gather(country, tfr, -year) %>% 
  filter(!(country %in% c('Sub-Saharan Africa', 'AFRICA', 'Western Africa')))

tfr = full_join(tfr, regionalTFR)

tfr = left_join(tfr, prb %>% filter(TimeFrame == 2014), by = c("country" = "Location"))


order = tfr %>% 
  filter(year =='2010-2015') %>% 
  arrange(desc(tfr))

tfr$country = factor(tfr$country,
                     levels  = order$country)

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
  scale_x_discrete(labels = c('1950-1955','', '', '', '',
                              '', '1980-1985', '', '','',
                              '','','2010-2015')) +
  theme_xygrid() + 
  scale_y_continuous(limits = c(0,8)) +
  ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
  theme(axis.text.x = element_text(size = 10),
        panel.margin = unit(1, 'lines'))
