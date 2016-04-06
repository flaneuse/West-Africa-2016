# Dashboard to explore total fertility rates in West Africa
# Laura Hughes, lhughes@usaid.gov, 6 April 2016

# Set up the app ----------------------------------------------------------

library(llamar)
loadPkgs()


# import data -------------------------------------------------------------

tfr = read.csv('~/Documents/USAID/West Africa Regional 2016/dataout/WFAR_tfr.csv')
