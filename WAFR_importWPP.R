# Sources -----------------------------------------------------------------

# Import data from the UN's World Population 
# http://esa.un.org/unpd/wpp/

# -- Population --
# Population data were downloaded as Excel files and lightly manipulated (cut out relevant countries, transposed in Excel)
# Pop. is listed in combined male + female
# Pop. values are listed in 1000's of people.
# West Africa is all the target countries, MINUS Cameroon (= Middle Africa) and PLUS St. Helena.
# Pop. estimates are for the middle variant model.


# Import data -------------------------------------------------------------

library(llamar)
loadPkgs()

obsPop = read_excel('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_UN-WPP.xlsx',
                    sheet = 1)

predPop = read_excel('~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_UN-WPP.xlsx',
                     sheet = 2)



# Wrangle -----------------------------------------------------------------
obsPop = obsPop %>% 
  gather(country, pop, -year) %>% 
  mutate(pop = pop/1000, # convert to millions
         year = as.numeric(year), # convert to number
         isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
                            0, 1)
  ) 

predPop = predPop %>% 
  gather(country, proj, -year) %>% 
  # filter(year != 2015) %>% 
  mutate(proj = proj/1000, # convert to millions
         year = as.numeric(year), # convert to number
         isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
                            0, 1)
  ) 


pop = full_join(obsPop, predPop,
                by = c("year", "country", "isCountry")) 


# obsPop = obsPop %>% 
#   gather(country, pop, -year) %>% 
#   mutate(pop = pop/1000, # convert to millions
#          year = as.numeric(year), # convert to number
#          isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
#                             0, 1),
#          proj = 0
#   ) 
# 
# predPop = predPop %>% 
#   gather(country, pop, -year) %>% 
#   filter(year != 2015) %>% 
#   mutate(pop = pop/1000, # convert to millions
#          year = as.numeric(year), # convert to number
#          isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
#                             0, 1),
#          proj = 1
#   ) 
# 
# pop = rbind(obsPop, predPop) 


write.csv(pop, '~/Documents/USAID/West Africa Regional 2016/dataout/allPop.csv')
write.csv(obsPop, '~/Documents/USAID/West Africa Regional 2016/dataout/obsPop.csv')
write.csv(predPop, '~/Documents/USAID/West Africa Regional 2016/dataout/predPop.csv')

# calc derivative ---------------------------------------------------------
popRate = pop %>% 
  group_by(country) %>% 
  arrange(country, year) %>%
  mutate(lagged = lag(pop),
         lagged = ifelse(year == 1950, NA, lagged),
         rateYr = (pop - lagged)/lagged,
         avgRate = rollapplyr(rateYr, width = 5, FUN = mean, fill = NA),
         yearFacet = as.character(year))

popRate_region = popRate %>% 
  filter(country %in% c('Africa', 'Western Africa')) %>% 
  select(country, year, rateYr) %>% 
  spread(country, rateYr)

popRate = full_join(popRate, popRate_region,
                    by = c('year')) %>% 
  filter(country != 'Saint Helena')

write.csv(popRate, '~/Documents/USAID/West Africa Regional 2016/dataout/popRate.csv')


wAfrRate = popRate %>% 
  filter(year %in% c(2015),
         country == 'Western Africa')
  
popRate_wide = popRate %>% 
  filter(year %in% c(2005, 2015),
         isCountry == 1,
         country != 'Saint Helena') %>% 
  select(year, country, avgRate) %>% 
  mutate(yearStr = ifelse(year == 2005,
                       '2001-2005',
                       ifelse(year == 2015, '2011-2015',NA)))

rateSign = popRate_wide %>% 
  group_by(country) %>% 
  mutate(`2011-2015` = lead(avgRate),
         chg = `2011-2015` - avgRate) %>% 
  filter(!is.na(chg)) %>% 
  select(country, chg, `2011-2015`)

popRate_wide = full_join(popRate_wide, rateSign, by = 'country')


  # spread(year, avgRate) %>% 
  # rename(`2001-2005` = `2005`,
         # `2011-2015` = `2015`)

write.csv(popRate_wide, '~/Documents/USAID/West Africa Regional 2016/dataout/popRate_filtered.csv')

# plots -------------------------------------------------------------------


accentColor = '#b2182b'

obsPop2 = obsPop %>% filter(isCountry == 1, year <= 2050)

ggplot(obsPop2, aes(x = year, y = pop, 
                   label = paste0(round(pop, 1), ' M'),
                group = country)) +
  geom_line(colour = accentColor) +
  geom_line(colour = accentColor, linetype = 2,
            data = predPop %>% filter(isCountry ==1)) +
  geom_point(size = 2, colour = accentColor,
             fill = 'white', shape = 21,
             data = obsPop2 %>% filter(year == 2015)) +
  geom_text(size = 3, colour = accentColor,
            nudge_y = 1,
             data = obsPop2 %>% filter(year == 2015)) +
  facet_wrap(~country, scales='free_y') 

popRate = popRate %>% filter(year <= 2050)
ggplot(popRate, aes(x = year, y = rate, 
                    label = percent(rate, 1),
                    group = country)) +
  geom_hline(colour = grey90K, yintercept = 0) +
  geom_line(colour = accentColor, linetype = 2) +
  geom_line(colour = accentColor, data = popRate %>% filter(proj == 0)) +
  geom_point(size = 2, colour = accentColor,
             fill = 'white', shape = 21,
             data = popRate %>% filter(year == 2015)) +
  geom_text(size = 3, colour = accentColor,
            nudge_y = 0.01,
            data = popRate %>% filter(year == 2015)) +
  facet_wrap(~country)

popRate$yearFacet = factor(popRate$yearFacet,
                           levels = c('2015', '2010', '2005',
                                      '2000', '1995', '1990'))

countryOrder = popRate %>% 
  filter(year == 2015) %>% 
  arrange((avgRate))

popRate$country = factor(popRate$country,
                         levels = countryOrder$country)

ggplot(popRate %>% filter(year %in% seq(2005, 2015, by = 10),
                          isCountry == 1), aes(x = country, y = avgRate, 
                    label = percent(avgRate, 1),
                    group = country)) +
  geom_hline(colour = grey90K, yintercept = 0) +
  coord_flip()+
  # geom_line(colour = accentColor, linetype = 2) +
  # geom_line(colour = accentColor, data = popRate %>% filter(proj == 0)) +
  geom_point(size = 3, colour = accentColor,
             fill = 'white', shape = 21) +
  geom_point(size = 3, colour = accentColor,
             fill = accentColor, shape = 21,
             data = popRate %>% filter(year == 2015, isCountry == 1)) +
  theme_xgrid()


# Probably bad idea -------------------------------------------------------
pop = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/allPop.csv') %>% 
  select(-X) %>% 
  filter(isCountry == 1,
         country != 'Saint Helena')

pop = pop %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(tot = sum(pop, proj), 
         pct = pop/tot)

ggplot(pop, aes(x = year, y = pct, group = country)) +
  geom_line() +
  theme_xygrid() +
  coord_cartesian(xlim = c(1950, 2015)) +
# facet_wrap(~country, scales = 'free_y')
facet_wrap(~country)
