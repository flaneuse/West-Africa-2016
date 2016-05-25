library(llamar)
loadPkgs()


# Specify order ----------------------------------------------------------

countryOrder = c("Sierra Leone", "Nigeria", "Liberia", "Gambia", "Guinea", "Cote d'Ivoire", "Mauritania", "Cameroon", "Mali", "Niger", "Guinea-Bissau", "Benin", "Burkina Faso", "Togo", "Ghana", "Senegal", "Cabo Verde")

causes = c("Acute lower respiratory infections", "Malaria", "Prematurity", 
           "Birth asphyxia and birth trauma", "Other communicable, perinatal and nutritional conditions",
           "Diarrhoeal diseases", "HIV/AIDS")

colorMort = data.frame(cause = c(causes, 'other'),
  color = c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c",
    "#fdbf6f",
    grey50K
))

# import child mortality cause data ---------------------------------------
# Source: WHO Global Health Observatory

# Find the names of all the data files (to merge together)
allFiles = list.files('~/GitHub/West Africa 2016/ChildMortalityCauses/',
                      full.names = TRUE)

# Create a function to loop over all the data.

readGHOdata = function(fileName){
  # Read in the numeric data
  df <- read.csv(fileName,
                 header = TRUE, skip = 2)
  
  # Read the country name
  countryName <- read.csv(fileName,
                          header = TRUE, skip = 0, nrows = 1)
  
  # Pull out the country name
  countryName = as.character(countryName$Deaths.per.1.000.live.births)
  
  # Clean up the data (throwing away data for children under 5, any time data)
  df = df %>%
    select(cause = Cause.of.death, 
           year = Year,
           mort = X0.4.years) %>% # Selecting just the data for the 
    mutate(country = countryName) %>% 
    filter(year == max(year)) # Select the maximum year
}

# Loop over all the files and clean them up
df = lapply(allFiles, readGHOdata)

# Unlist the data and combine into a single dataframe
df = bind_rows(df)


# reorder the graphs ------------------------------------------------------
# simplify the causes of death
df = df %>% 
  mutate(causeCat = ifelse(cause %in% causes, as.character(cause), 'other'))

# country
df$country = factor(df$country, levels = countryOrder)

# cause of death
causeOrder = df %>% 
  filter(causeCat != 'other') %>% 
  group_by(causeCat) %>% 
  summarise(total = sum(mort)) %>% 
  arrange(desc(total))


# Reorder levels, putting other at the end.
df$causeCat = factor(df$causeCat, levels = c(causeOrder$causeCat, 'other'))

# bar graph ---------------------------------------------------------------

ggplot(df, aes(x = causeCat, 
               y = mort,
               fill = causeCat,
               label = country)) +
  geom_bar(stat = 'identity') +
  # scale_fill_brewer(palette = 'Set1') +
  # scale_fill_manual(values = colorMort$colors,
                    # breaks = colorMort$cause) +
  theme_ygrid() +
  facet_wrap(~ country, ncol = 1) +
  geom_text(aes(x = 6, y = 20), hjust = 0, 
            colour = grey75K,
            family = 'Segoe UI Light') +
  theme(strip.text = element_blank(),
        panel.background = element_rect(fill = NA, colour = grey75K, size = 0.1),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank())

# ggsave()
