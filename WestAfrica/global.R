# Dashboard to explore total fertility rates in West Africa
# Laura Hughes, lhughes@usaid.gov, 6 April 2016

# Set up the app ----------------------------------------------------------

library(llamar)
loadPkgs()

accentColor = '#b2182b'

# import data -------------------------------------------------------------

tfr = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WFAR_tfr.csv')

countries = levels(unique(tfr$country))

minChg = round(min(tfr$refRate, na.rm = TRUE)*100, 0)
maxChg = round(max(tfr$refRate, na.rm = TRUE)*100, 0)


# Source files ------------------------------------------------------------

source('indivRate.R')
