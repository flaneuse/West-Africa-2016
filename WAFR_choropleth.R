choroData = data.frame(country = str_to_lower(countries)) %>% 
  mutate(region = ifelse(country == 'guinea-bissau', 'guinea bissau', 
                         ifelse(country == "côte d'ivoire", "ivory coast",
                                levels(country)))) %>% 
  filter(country != 'cabo verde')

coords = geocode(choroData$region)

choroData = cbind(choroData, coords)
