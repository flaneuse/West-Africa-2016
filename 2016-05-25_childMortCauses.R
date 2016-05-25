library(llamar)
loadPkgs()


# Specify colors ----------------------------------------------------------
colorMort = data.frame(cause = c(
  "Acute lower respiratory infections",                      
  "Birth asphyxia and birth trauma",                         
  "Congenital anomalies",                                    
  "Diarrhoeal diseases",                                     
  "HIV/AIDS",                                                
  "Injuries",                                                
  "Malaria",                                                 
  "Measles",                                                 
  "Meningitis/encephalitis",                                 
  "Other communicable, perinatal and nutritional conditions",
  "Other noncommunicable diseases",                          
  "Pertussis",                                               
  "Prematurity",                                             
  "Sepsis and other infectious conditions of the newborn",   
  "Tetanus"),
  color = c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c",
    "#fdbf6f",
    "#ff7f00",
    "#cab2d6",
    "#6a3d9a",
    "#ffff99",
    "#b15928",
    "")
)

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

df = lapply(allFiles, readGHOdata)

df2 = do.call(rbind, df)

ggplot(df, aes(x = cause, 
               y = mort,
               fill = cause)) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = 'Paired') +
  # scale_fill_manual(values = colorMort$colors, 
                    # breaks = colorMort$cause) +
  theme_ygrid()

ggsave()
