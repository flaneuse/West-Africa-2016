# Sources -----------------------------------------------------------------

# Import data from the UN's World Urbanization Prospects, 2014
# http://esa.un.org/unpd/wup/

# -- Population --
# Population data were downloaded as Excel files and lightly manipulated (cut out relevant countries, transposed in Excel)
# Pop. is listed in combined male + female
# Pop. values are listed in 1000's of people.
# West Africa is all the target countries, MINUS Cameroon (= Middle Africa) and PLUS St. Helena.


# Import data -------------------------------------------------------------

library(llamar)
loadPkgs()

accentColor = '#b2182b'

pctUrb = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WUP2014_combined.xlsx',
                    sheet = 1)

rurPop = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WUP2014_combined.xlsx',
                    sheet = 2)

urbPop = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WUP2014_combined.xlsx',
                    sheet = 3)

totPop = read_excel('~/Documents/USAID/West Africa Regional 2016/datain/WUP2014_combined.xlsx',
                    sheet = 4)

# Wrangle -----------------------------------------------------------------
# Pull out the regional numbers
regionalR = rurPop %>% 
  mutate(year = as.numeric(year)) %>% 
  select(year,
         ruralSubsaharan = `Sub-Saharan Africa`, 
         ruralAfrica = AFRICA, 
         ruralWestAfrica = `Western Africa`)

regionalU = urbPop %>% 
  mutate(year = as.numeric(year)) %>% 
  select(year,
         urbanSubsaharan = `Sub-Saharan Africa`, 
         urbanAfrica = AFRICA, 
         urbanWestAfrica = `Western Africa`)

regionalT = totPop %>% 
  mutate(year = as.numeric(year)) %>% 
  select(year,
         totalSubsaharan = `Sub-Saharan Africa`, 
         totalAfrica = AFRICA, 
         totalWestAfrica = `Western Africa`)


rurPop = rurPop %>% 
  gather(country, rural, -year) %>% 
  mutate(rural = rural/1000, # convert to millions
         year = as.numeric(year)) %>%  # convert to number
  filter(!(country %in% c('Sub-Saharan Africa', 'AFRICA', 'Western Africa')))


urbPop = urbPop %>% 
  gather(country, urban, -year) %>% 
  mutate(urban = urban/1000, # convert to millions
         year = as.numeric(year)) %>%  # convert to number
  filter(!(country %in% c('Sub-Saharan Africa', 'AFRICA', 'Western Africa')))

totPop = totPop %>% 
  gather(country, total, -year) %>% 
  mutate(total = total/1000, # convert to millions
         year = as.numeric(year)) %>%  # convert to number
  filter(!(country %in% c('Sub-Saharan Africa', 'AFRICA', 'Western Africa')))

urbRural = full_join(rurPop, urbPop, 
                     by = c('country', 'year')) 


urbRural = full_join(urbRural, totPop, 
                     by = c('country', 'year')) 

# Check numbers make sense
all(urbRural$rural + urbRural$urban - urbRural$total < 1e-10)



# Calc % urban
urbRural = urbRural %>% 
  filter(country != 'Saint Helena') %>% 
  mutate(pctUrb = urban/total)

countryOrder = urbRural %>% 
  filter(year == 2015) %>% 
  arrange((pctUrb))

urbRural$country = factor(urbRural$country,
                          countryOrder$country)

# Pull out the regional numbers
regional = full_join(regionalR, regionalU, by = 'year')
regional = full_join(regional, regionalT, by = 'year')


urbRural = full_join(urbRural, regional, by = 'year') %>% 
  mutate(pctUrb_WestAfrica = urbanWestAfrica / totalWestAfrica,
         pctUrb_Africa = urbanAfrica / totalAfrica,
         pctUrb_Subsaharan = urbanSubsaharan / totalSubsaharan)


write.csv(urbRural, '~/Documents/USAID/West Africa Regional 2016/dataout/urbanRural_pop.csv')
# plot --------------------------------------------------------------------
ggplot(urbRural, aes(x = year, y = pctUrb,
                     group = country)) +
  geom_hline(yintercept = 0.5,
             colour = 'blue') +
  geom_line(aes(y = pctUrb_WestAfrica), 
            colour = grey50K) + 
  # geom_area(aes(y = pctUrb_WestAfrica), 
  #           alpha = 0.4,
  #           fill = 'yellow') +
  geom_ribbon(aes(ymin = pctUrb_WestAfrica, 
                  ymax = pctUrb),
              fill = 'grey',
            alpha = 0.4) + 
  geom_line(colour = accentColor) +
  facet_wrap(~country) +
  theme_xygrid()

ggplot(urbRural, aes(x = year, 
                     group = country)) +
  # geom_line(aes(y = urbanWestAfrica), 
            # colour = grey50K) + 
  # geom_line(aes(y = ruralWestAfrica), 
            # colour = grey50K,
            # linetype = 2) +
  geom_line(aes(y = urban),
            colour = accentColor) +
  geom_line(aes(y = rural),
            colour = accentColor, linetype = 2) +
  facet_wrap(~country, scales = 'free_y') +
  theme_xygrid()


# rate change urbanization ------------------------------------------------
urbRural = urbRural %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(laggedUrb = lag(urban),
         rateUrb = (urban - laggedUrb)/urban)



# current pop vs. % urbanized ---------------------------------------------
urbRural2 = full_join(urbRural, popRate %>% select(year, country, avgRate)) %>% 
  filter(year == 2015)

ggplot(urbRural2 %>% filter(year == 2015), 
       aes(x = total, y = pctUrb, 
           label = country,
           colour = avgRate,
           size = avgRate)) +
  geom_point() +
  scale_x_continuous(trans = 'log') +
  scale_colour_gradientn(colours = brewer.pal(9, 'RdPu')) +
  geom_label(size = 2, nudge_y = 0.05) +
  theme_xygrid()
