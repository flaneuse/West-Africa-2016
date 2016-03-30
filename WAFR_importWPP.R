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
                            0, 1),
         proj = 0
  ) 

predPop = predPop %>% 
  gather(country, pop, -year) %>% 
  filter(year != 2015) %>% 
  mutate(pop = pop/1000, # convert to millions
         year = as.numeric(year), # convert to number
         isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
                            0, 1),
         proj = 1
  ) 

pop = rbind(obsPop, predPop) 


# calc derivative ---------------------------------------------------------
popRate = pop %>% 
  arrange(country, year) %>% 
  mutate(lagged = lag(pop),
         lagged = ifelse(year == 1950, NA, lagged),
         rate = (pop - lagged)/lagged)


# plots -------------------------------------------------------------------


accentColor = '#b2182b'

obsPop2 = obsPop %>% filter(isCountry == 1)

ggplot(obsPop2, aes(x = year, y = pop, 
                   label = paste0(round(pop, 1), ' M'),
                group = country)) +
  geom_line(colour = accentColor) +
  geom_line(colour = accentColor, linetype = 2,
            data = predPop) +
  geom_point(size = 2, colour = accentColor,
             fill = 'white', shape = 21,
             data = obsPop %>% filter(year == 2015)) +
  geom_text(size = 3, colour = accentColor,
            nudge_y = 1,
             data = obsPop %>% filter(year == 2015)) +
  facet_wrap(~country, scales = 'free_y') 


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
            nudge_y = 0.1,
            data = popRate %>% filter(year == 2015)) +
  facet_wrap(~country)