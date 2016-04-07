indivRateUI = function(id){
  ns <- NS(id)
  
  fluidRow(column(6,plotOutput(ns('indivRate'))),
           column(6, plotOutput(ns('indivChoroChg')))
  )
  
  # fluidRow(column(6,plotOutput('indiv')),
  # column(6, plotOutput('indivChoro')))),
  # fluidRow(column(6,plotOutput('indivRate')),
  # column(6, plotOutput('indivChoroChg')))),
}

indivRate = function(input, output, session, tfr, selCountry){
  
  filterTFR = reactive({
    tfr %>% 
      filter(country == selCountry)
  })
  
  output$indivChoroChg = renderPlot({
      selYear = '2010-2015'
      
      filteredChg = filterTFR() %>% 
        filter(year == selYear) %>% 
        ungroup() %>% 
        select(country, year, tfr, rate, value = refRate) %>% 
        mutate(colText = ifelse(value < 1.25*mean(value), grey10K, grey90K),
               country = str_to_lower(country))
      
      filteredChoro = left_join(choroData, filteredChg,
                                by = c("country" = "country"))
      
      country_choropleth(filteredChoro, 
                         zoom = filteredChoro$region, num_colors = 1) +
        scale_fill_gradientn(colours = rev(brewer.pal(9, 'PuRd')),
                             name = paste0('Change in TFR \n(', selYear, '  -  1980-1985)'),
                             limits = c(-0.35, 0.10),
                             breaks = seq(-0.35, 0.10, by = 0.1)) +
        geom_text(aes(label = str_to_title(region), 
                      size = 3,
                      colour = colText,
                      x = lon, y = lat, group = region),
                  data = filteredChoro) +
        scale_colour_identity() +
        scale_size(guide = FALSE) +
        guides(fill = guide_colorbar(ticks = FALSE)) +
        ggtitle(selYear)
  })
  
  
  output$indivRate = renderPlot({
    yLim = c(-0.35, 0.10)
    
    
    filteredTFR = filterTFR() %>% 
      filter(country == selCountry)
    
    recentTFR = filteredTFR %>% filter(year == '2010-2015')
    
    ggplot(filteredTFR, aes(x = year, y = rate, 
                            label = percent(rate),
                            group = country)) +
      geom_hline(yintercept = 0, colour = grey90K, size = 0.3) +
      geom_line(colour = accentColor) +
      geom_point(colour = accentColor,
                 data = recentTFR)+
      geom_label(colour = accentColor,
                 data = recentTFR, nudge_y = -0.08)+
      scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                                  '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                       labels = c('1950-1955','',  '',
                                  '1980-1985', 
                                  '','','2010-2015')) +
      theme_xygridlight() + 
      scale_y_continuous(labels = scales::percent,
                         breaks = seq(yLim[1], yLim[2], by = 0.15),
                         limits = yLim) +
      ggtitle(paste0(selCountry, ': rate of change in TFR')) +
      theme(axis.text.x = element_text(size = 14)) +
      ylab('') + xlab('')
    
  }, 
  height = 150)
}