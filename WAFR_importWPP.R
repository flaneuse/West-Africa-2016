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
         isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
                            0, 1),
         proj = 0
  ) 

predPop = predPop %>% 
  gather(country, pop, -year) %>% 
  filter(year != 2015) %>% 
  mutate(pop = pop/1000, # convert to millions
         isCountry = ifelse(country %in% c('Sub-Saharan Africa', 'Africa', 'Western Africa'), 
                            0, 1),
         proj = 1
  ) 

pop = rbind(obsPop, predPop)

