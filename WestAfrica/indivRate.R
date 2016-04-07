indivRateUI = function(id){
  ns <- NS(id)
  
  fluidRow(column(6,plotOutput(ns('indivRate'))))
}

indivRate = function(input, output, session, tfr, selCountry){
  
  filterTFR = reactive({
    tfr %>% 
      # filter(country == 'Niger')
      filter(country == selCountry)
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