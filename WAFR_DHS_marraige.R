library(llamar)
loadPkgs()
indicator = c('MA_AAFM_W_M2B')
countries = 'MR,NI,BF,CM,CI,TG'
years = paste0(seq(2000,2016), collapse = ',')
apiKey = 'USAAID-405632'

marriageW = loadDHS(breakdown = 'all', indicators = indicator, 
                   apiKey = apiKey, countries = countries)

library(llamar)
loadPkgs()
indicator = c('MA_AAFM_M_M2B')
countries = 'MR,NI,BF,CM,CI,TG'
years = paste0(seq(2000,2016), collapse = ',')
apiKey = 'USAAID-405632'

marriageM = loadDHS(breakdown = 'all', indicators = indicator, 
                   apiKey = apiKey, countries = countries)

marriage = rbind(marriageM, marriageW)
write.csv(marriage, '~/Documents/USAID/West Africa Regional 2016/dataout/marriage.csv')
