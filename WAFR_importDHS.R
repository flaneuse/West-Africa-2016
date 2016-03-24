
# Import DHS data ---------------------------------------------------------



# Common indicators
# Unmet need total (iCE37FE2)
# Total fertility rate (i133C8F8)
# Median duration of exclusive breastfeeding (i5716309)
# Women receiving an HIV test and receiving test results in the last 12 months (i42759492)
# Children underweight (iCC97002)
# Households with electricity (i7C3CB1)
# Children under 5 who slept under an insecticide treated net (ITN) (iC6D4E13)
# Infant mortality rate (i42FFDB2)
# Men receiving an HIV test and receiving test results in the last 12 months (i42759491)
# HIV prevalence among general population (i41075E93)
# HIV prevalence among women (i41075E92)
# HIV prevalence among men (i41075E91)
# Under-five mortality rate (i42FFDB4)
# Received all vaccinations (i4E75F0A)
# Treatment of diarrhea: Either ORS or RHS (i533BDCB)
# Median age at first marriage: 25-49 (i349C430)
# Children wasted (iCC973EA)
# Women with secondary or higher education (iA999FA)
# Place of delivery: Health facility (i49B3AD0)
# Children stunted (iCC96C1A)
# Married women currently using any modern method of contraception (i209E191)
# Married women currently using any method of contraception (i209E190)
# Median age at first sexual intercourse: 25-49 (i36848B0)
# Women who are literate (i797B5DA)

# require(RJSONIO)




# set up the queries
apiKey = 'USAAID-405632'
indicator1 = 'FP_NADM_W_UNT,FE_FRTR_W_TFR,CN_BFDR_C_MDE,HA_CPHT_W_T1R,HC_ELEC_H_ELC'
indicator2 = 'CN_NUTS_C_WA2,HA_HIVP_W_HIV,HA_HIVP_W_HVE'
indicator3 = 'HA_HIVP_B_HIV,HA_HIVP_B_HVE,CM_ECMT_C_U5M,CM_ECMR_C_IMR'
indicator4 = 'CM_ECMR_C_NNR,CH_VACS_C_BAS,CH_VACS_C_NON,CH_VACC_C_BAS,CH_VACC_C_NON'
indicator5 = 'CH_VAC1_C_BAS,CH_VAC1_C_NON,CH_DIAT_C_ORT,CH_DIAR_C_DIA,MA_AAFM_W_M20'
indicator6 = 'MA_AAFM_W,MA_AAFM_W,MA_AAFM_W,MA_AAFM_W,MA_AAFM_W,MA_AAFM_W,MA_AAFM_W_M2'
indicator7 = 'HA_HIVP_M_HVE,HA_HIVP_M_HIV'
indicator8 = 'CN_NUTS_C_WH2,CN_NUTS_C_HA2,ED_EDUC_W_SEP'
indicator9 = 'CM_ECMR_C_PNR,FP_CUSA_W_MOD,FP_CUSM_W_ANY,FP_CUSM_W_MOD'
indicator10 = 'SX_AAFS_W_M25,SX_AAFS_W_M30,SX_AAFS_W_M35,SX_AAFS_W_M40,SX_AAFS_W_M45'
indicator11 = 'SX_AAFS_W_M2A,SX_AAFS_W_M2B,ED_LITR_W_LIT,RH_PAHC_W_KNW,RH_PAHC_W_PRM'
indicator12 = 'RH_PAHC_W_MON,RH_PAHC_W_DIS,RH_PAHC_W_TRN,RH_PAHC_W_ALN,RH_PAHC_W_FEM'
indicator13 = 'MM_MMRT_W_MRT,MM_MMRO_W_GFR,MM_MMRO_W_MMR,MM_MMRO_W_CIL,MM_MMRO_W_CIH'
indicator14 = 'RH_DELP_C_DHF,FP_CUSA_W_ANY,SX_AAFS_W_M20'

indicators = paste0(indicator1, indicator2, indicator3, indicator4, indicator5, indicator6, indicator7, 
                    indicator8, indicator9, indicator10, indicator11, indicator12, indicator13, indicator14)
countries = 'BJ,BF,CM,CI,GH,GN,LB,ML,MR,NI,NG,SN,SL,TG'
# years = paste0(seq(1980,2016), collapse = ',')
years = paste0(seq(2000,2016), collapse = ',')

# Figure out where to filter the years of DHS data
json_file <- fromJSON(paste0('http://api.dhsprogram.com/rest/dhs/data?breakdown=national&indicatorIds=',indicators, 
                             '&countryIds=', countries, 
                             '&SurveyYear=', years,
                             '&apiKey=', apiKey,
                             '&perpage=5000'))

# Unlist the JSON file entries
json_data <- lapply(json_file$Data, function(x) { unlist(x) })

# Convert JSON input to a data frame
allData <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE)

library('dplyr')
library('ggplot2')
library('llamar')

svyYrs = allData %>% 
  group_by(SurveyYear, CountryName) %>% 
  summarise(num  = n()) %>% 
  mutate(year = as.integer(SurveyYear))

ggplot(svyYrs, aes(x = year, y = CountryName, size = num)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1986, 2016, by = 4)) +
  theme_xgrid()

ggplot(svyYrs, aes(x = year, y = CountryName, size = num)) +
  geom_point() +
  scale_x_continuous(limits = c(2000, 2016),
    breaks = seq(2000, 2016, by = 2)) +
  theme_xgrid()


years = paste0(seq(2000,2016), collapse = ',')

# Import DHS Indicator National-level data
json_file = fromJSON(paste0('http://api.dhsprogram.com/rest/dhs/data?breakdown=national&indicatorIds=',indicators, 
                             '&countryIds=', countries, 
                             '&SurveyYear=', years,
                             '&apiKey=', apiKey,
                             '&perpage=5000'))
                             
json_file_sub1 = fromJSON(paste0('http://api.dhsprogram.com/rest/dhs/data?breakdown=all&indicatorIds=',indicator2, 
                             '&countryIds=', countries, 
'&SurveyYear=', years,
'&apiKey=', apiKey,
'&perpage=5000'))

x = loadDHS(breakdown = 'national', indicators = indicator14, countries = countries, years = years, apiKey = apiKey, numResults = 100)

# Unlist the JSON file entries
json_data <- lapply(json_file$Data, function(x) { unlist(x) })

# Convert JSON input to a data frame
natl <- as.data.frame(do.call("rbind", json_data),stringsAsFactors=FALSE)


write.csv(natl, '~/Documents/USAID/West Africa Regional 2016/dataout/WAFR_DHS_natl.csv')
