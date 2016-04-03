
# Assemble together all the component pieces for a Tableau dashboard --------
library(llamar)
loadPkgs()


# health projects ---------------------------------------------------------
proj = data.frame(
  country = c("Benin",
              "Burkina Faso",
              "Cabo Verde",
              "Cameroon",
              "Côte d'Ivoire",
              "Gambia",
              "Ghana",
              "Guinea",
              "Guinea-Bissau",
              "Liberia",
              "Mali",
              "Mauritania",
              "Niger",
              "Nigeria",
              "Senegal",
              "Sierra Leone",
              "Togo"),
  role = c('bilateral', 'non-presence',
           'none', 'non-presence', 'non-presence', 'independent',
           'bilateral', 'independent', 'none', 'independent', 'independent',
           'non-presence', 'non-presence', 'independent', 'independent', 'independent', 'non-presence')
)  

# non-presence = Cote d’Ivoire, Togo, Burkina Faso, Niger, Mauritania, and Cameroon
# bilateral = c('Benin', 'Ghana')

# urban/rural data --------------------------------------------------------

urbRural = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/urbanRural_pop.csv') %>% 
  select(-X)



# population --------------------------------------------------------------

pop = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/allPop.csv') %>% 
  select(-X) %>% 
  filter(isCountry == 1,
         country != 'Saint Helena')

pop2015 = pop %>% 
  filter(year == 2015) %>% 
  select(country, year, pop2015 = pop)

pop = full_join(pop, pop2015, by = c("year", "country"))

# merge-n-save ------------------------------------------------------------

tableau = full_join(urbRural, pop, by = c('country', 'year')) 
tableau = full_join(tableau, proj, by = 'country') 

write.csv(tableau, '~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_allTableau.csv')
