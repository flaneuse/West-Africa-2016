# Load Libraries and data -------------------------------------------------
library(llamar)
loadPkgs()

targetedCountries = c("Niger","Burkina Faso", "Cote d'Ivoire", "Cameroon", "Mauritania", "Togo")


# POPULATION PYRAMIDS -----------------------------------------------------
# Data from the UN URPAS esa.un.org

# import pop pyramids -----------------------------------------------------
# Data initially in 1000's of people.
popPyramid = read.csv('~/Documents/USAID/West Africa Regional 2016/datain/2.1_PopulationPyramid.csv')

popPyramid = popPyramid %>% 
  rename(country = `X.Location`,
         year = `X.Year`,
         sex = `X.Sex`,
         rural = `X.Area`,
         total = `X.Total`) %>% 
  filter(year == 2015) %>% 
  gather(age, pop, -country, -year, -sex, -rural, -total)  # tidy




popPyramid = popPyramid %>% 
  mutate(pop = pop/1000, # Convert to millions of people
         age = str_replace_all(age, 'X.', ''),
         age = str_replace_all(age, '\\.', '-'),
         age = ifelse(age == '80-', '80+', age),
         pop = ifelse(sex == 'Male', pop * -1, pop)) # Hack to make two axes

# Change levels of age to be correct.
popPyramid$age = factor(popPyramid$age,
                        levels = c('0-4','5-9','10-14','15-19','20-24','25-29',
                                   '30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79', '80+'))

# pop pyramid plot --------------------------------------------------------
plotWidth = 6
plotHeight = 6

totalPop = popPyramid %>% 
  filter(rural == 'Total')

ggplot(totalPop %>% filter(sex =='Female'), 
       aes(x = age, y = pop,
           fill = sex)) +
  coord_flip() + 
  geom_bar(stat = 'identity', size = 0) +
  geom_bar(stat = 'identity',
           size = 0,
           data = totalPop %>% filter(sex =='Male')) +
  facet_wrap(~country) +
  theme_xgrid()

fileName = paste0('~/Documents/USAID/West Africa Regional 2016/plots/MF_pop_samescale.pdf')

ggsave(filename = fileName,
       width = plotWidth, height = plotHeight,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

for (i in seq_along(targetedCountries)){
  totalPop = popPyramid %>% 
    filter(rural == 'Total',
           country == targetedCountries[i])
  
  ggplot(totalPop %>% filter(sex =='Female'), 
         aes(x = age, y = pop,
             fill = sex)) +
    coord_flip() + 
    geom_bar(stat = 'identity', size = 0) +
    geom_bar(stat = 'identity',
             size = 0,
             data = totalPop %>% filter(sex =='Male')) +
    facet_wrap(~country) +
    theme_xgrid()
  
  fileName = paste0('~/Documents/USAID/West Africa Regional 2016/plots/MF_pop_', 
                    targetedCountries[i], '.pdf')
  
  ggsave(filename = fileName,
         width = plotWidth, height = plotHeight,
         bg = 'transparent',
         paper = 'special',
         units = 'in',
         useDingbats=FALSE,
         compress = FALSE,
         dpi = 300)
}

# By urban/rural ----------------------------------------------------------

for (i in seq_along(targetedCountries)){
  urbRuralPop = popPyramid %>% 
    filter(rural != 'Total',
           country == targetedCountries[i])
  
  p = ggplot(urbRuralPop %>% filter(sex =='Female'), 
         aes(x = age, y = pop,
             fill = sex)) +
    coord_flip() + 
    geom_bar(stat = 'identity', size = 0) +
    geom_bar(stat = 'identity',
             size = 0,
             data = urbRuralPop %>% filter(sex =='Male')) +
    geom_bar(stat = 'identity',
             fill = 'white',
             size = 0,
             alpha = 0.5,
             data = urbRuralPop %>% 
               filter(sex =='Male',
                      !age %in% c('10-14', '15-19', '20-24','25-29'))) +
    geom_bar(stat = 'identity',
             fill = 'white',
             size = 0,
             alpha = 0.5,
             data = urbRuralPop %>% 
               filter(sex =='Female',
                      !age %in% c('10-14', '15-19', '20-24','25-29'))) +
    facet_wrap(~rural, ncol = 2) +
    theme_xgrid() +
    theme(axis.title = element_blank()) +
    ggtitle(targetedCountries[i])
  
  fileName = paste0('~/Documents/USAID/West Africa Regional 2016/plots/UR_pop_', 
                    targetedCountries[i], '.pdf')
  
  ggsave(filename = fileName,
         width = plotWidth, height = plotHeight,
         bg = 'transparent',
         paper = 'special',
         units = 'in',
         useDingbats=FALSE,
         compress = FALSE,
         dpi = 300)
}