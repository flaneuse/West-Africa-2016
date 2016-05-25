library(llamar)
loadPkgs()


# Specify colors ----------------------------------------------------------



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
