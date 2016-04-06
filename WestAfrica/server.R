shinyServer(
  function(input, output, session) {
    
    filterTFR = reactive({
      tfr %>% 
        filter(country %in% input$countryList)
    })
    
    
    
    output$choro = renderPlot({
      selYear = '2010-2015'
      
      filteredTFR = filterTFR() %>% 
        filter(year == selYear) %>% 
        ungroup() %>% 
        select(country, year, value = tfr, rate, refRate) %>% 
        mutate(colText = ifelse(value > 1.25*mean(value), grey10K, grey90K),
               country = str_to_lower(country))
      
      filteredChoro = left_join(choroData, filteredTFR,
                                by = c("country" = "country"))
      
      country_choropleth(filteredChoro, 
                         zoom = filteredChoro$region, num_colors = 1) +
        scale_fill_gradientn(colours = brewer.pal(9, 'Blues'),
                             name = 'Total Fertility Rate',
                             limits = c(2, 8),
                             breaks = seq(2, 8, by = 2)) +
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
      yLim = c(-0.65, 0.15)
      
      selCountry = 'Niger'
      
      filteredTFR = filterTFR() %>% 
        filter(country == selCountry)
        
        ggplot(filteredTFR, aes(x = year, y = rate, group = country)) +
        geom_line(colour = accentColor) +
        geom_point(colour = accentColor,
                   data = filteredTFR %>% filter(year == '2010-2015'))+
        facet_wrap(~country) +
        scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                                    '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                         labels = c('1950-1955','',  '',
                                    '1980-1985', 
                                    '','','2010-2015')) +
        theme_xygridlight() + 
        scale_y_continuous(labels = scales::percent,
                           limits = yLim) +
        ggtitle(selCountry) +
        theme(axis.text.x = element_text(size = 14))
      
    })
    
    
    output$choroChg = renderPlot({
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
    output$plot1 = renderPlot({
      
      # Filter down the data
      filteredTFR = filterTFR()
      
      recentTFR = filteredTFR %>% filter(year == '2010-2015')
      
      # see if there's > 1 country
      if(nrow(recentTFR) == 1){
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
          scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                                      '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                           labels = c('1950-1955','',  '',
                                      '1980-1985', 
                                      '','','2010-2015')) +
          theme_xygridlight() + 
          scale_y_continuous(limits = c(0,8)) +
          ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
          theme(axis.text.x = element_text(size = 9))
      } else {
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
          theme_xygridlight() + 
          scale_y_continuous(limits = c(0,8)) +
          ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
          theme(axis.text.x = element_text(size = 9))
      }
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