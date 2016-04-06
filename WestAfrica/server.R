shinyServer(
  function(input, output, session) {
    
    filterTFR = reactive({
      tfr %>% 
        filter(country %in% input$countryList)
    })
    
    
    
    output$plot1 = renderPlot({
      
      # Filter down the data
      filteredTFR = filterTFR()
      
      recentTFR = filteredTFR %>% filter(year == '2010-2015')
      
      # see if there's > 1 country
      p = ggplot(filteredTFR, aes(x = year, y = tfr, group = country)) +
        geom_line(colour = grey50K,
                  aes(y = tfrWAfr)) +
        geom_line(colour = accentColor) +
        geom_point(colour = accentColor,
                   data = recentTFR)+
        geom_point(aes(y = prb),
                   data = recentTFR,
                   colour = 'dodgerblue')+
        geom_text(aes(label = round(tfr,1)), 
                  colour = accentColor,
                  nudge_y = -1,
                  hjust = 1,
                  data = recentTFR) +
        geom_text(aes(label = round(prb,1)), 
                  colour = 'dodgerblue',
                  nudge_y = -1,
                  hjust = 1,
                  data = recentTFR) +
        facet_wrap(~country) +
        scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                                    '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                         labels = c('1950-1955','',  '',
                                    '1980-1985', 
                                    '','','2010-2015')) +
        theme_xygrid() + 
        scale_y_continuous(limits = c(0,8)) +
        ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
        theme(axis.text.x = element_text(size = 10),
              panel.margin = unit(1, 'lines'))
      
      return(p)
    })
    
    
    output$footer = renderImage({
      return(list(
        src = "img/footer_WestAfrica_WPP.png",
        width = '100%',
        filetype = "image/png",
        alt = "Plots from USAID's GeoCenter"
      ))
    }, deleteFile = FALSE)
    
    
  })