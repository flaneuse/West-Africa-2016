# Dashboard to explore total fertility rates in West Africa
# Laura Hughes, lhughes@usaid.gov, 6 April 2016

# Set up the app ----------------------------------------------------------


library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(stringr)
library(llamar)

accentColor = '#b2182b'
ltColor  = '#e0a3aa'

# import data -------------------------------------------------------------

tfr = read.csv('data/WFAR_tfr.csv')

countries = levels(unique(tfr$country))

minChg = round(min(tfr$refRate, na.rm = TRUE)*100, 0)
maxChg = round(max(tfr$refRate, na.rm = TRUE)*100, 0)


# Source files ------------------------------------------------------------

source('indivRate.R')
source('footer.R')
