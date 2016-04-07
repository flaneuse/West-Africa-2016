shinyServer(
  function(input, output, session) {
    
    filterTFR = reactive({
      tfr %>% 
        filter(country %in% input$countryList)
    })
    
    
    
    callModule(indivRate, 'benin', tfr, choroData, 'Benin')
    callModule(indivRate, 'burkina', tfr, choroData, 'Burkina Faso')
    callModule(indivRate, 'cabo', tfr, choroData, 'Cabo Verde')
    callModule(indivRate, 'cameroon', tfr, choroData, 'Cameroon')
    callModule(indivRate, 'cote', tfr, choroData, "Côte d'Ivoire")
    callModule(indivRate, 'gambia', tfr, choroData, 'Gambia')
    callModule(indivRate, 'ghana', tfr, choroData, 'Ghana')
    callModule(indivRate, 'guinea', tfr, choroData, 'Guinea')
    callModule(indivRate, 'g-b', tfr, choroData, 'Guinea-Bissau')
    callModule(indivRate, 'liberia', tfr, choroData, 'Liberia')
    callModule(indivRate, 'mali', tfr, choroData, 'Mali')
    callModule(indivRate, 'mauritania', tfr, choroData, 'Mauritania')
    callModule(indivRate, 'niger', tfr, choroData, 'Niger')
    callModule(indivRate, 'nigeria', tfr, choroData, 'Nigeria')
    callModule(indivRate, 'senegal', tfr, choroData, 'Senegal')
    callModule(indivRate, 'sl', tfr, choroData, 'Sierra Leone')
    callModule(indivRate, 'togo', tfr, choroData, 'Togo')
    
    
    output$plot1 = renderPlot({
      
      # Filter down the data
      filteredTFR = filterTFR()
      
      recentTFR = filteredTFR %>% filter(year == '2010-2015')
      
        p = ggplot(filteredTFR, aes(x = year, y = tfr, group = country)) +
          geom_line(colour = grey50K,
                    aes(y = tfrWAfr)) +
          geom_line(colour = accentColor) +
          geom_point(colour = accentColor,
                     data = recentTFR)+
          geom_point(aes(y = tfrWAfr),
                     data = recentTFR,
                     size = 2.5,
                     colour = grey50K) +
          # geom_point(aes(y = prb),
                     # data = recentTFR,
                     # colour = 'dodgerblue')+
          geom_text(aes(label = round(tfr,1)), 
                    colour = accentColor,
                    nudge_y = -1,
                    hjust = 1,
                    data = recentTFR) +
          # geom_text(aes(label = round(prb,1)), 
          #           colour = 'dodgerblue',
          #           nudge_y = -1,
          #           hjust = 1,
          #           data = recentTFR) +
          facet_wrap(~country) +
          scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                                      '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                           labels = c('1950-1955','',  '',
                                      '1980-1985', 
                                      '','','2010-2015')) +
          theme_xygridlight() + 
          scale_y_continuous(limits = c(0,8),
                             breaks = seq(0, 8, by = 2)) +
          xlab('') + ylab('')+
          ggtitle('Niger and Mali have high Total Fertility Rates relative to the rest of the region') +
          theme(axis.text.x = element_text(size = 9),
                legend.position = 'bottom',
                legend.direction = 'horizontal')

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
    
    
    output$footer2 = renderImage({
      return(list(
        src = "img/footer_WestAfrica_WPP.png",
        width = '100%',
        filetype = "image/png",
        alt = "Plots from USAID's GeoCenter"
      ))
    }, deleteFile = FALSE)
    
    output$footer3 = renderImage({
      return(list(
        src = "img/footer_WestAfrica_WPP.png",
        width = '100%',
        filetype = "image/png",
        alt = "Plots from USAID's GeoCenter"
      ))
    }, deleteFile = FALSE)
    
    
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
    
    output$ratePlot  = renderPlot({
      yLim = c(-0.35, 0.10)
      
      
      filteredTFR = filterTFR() 
      
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
        ggtitle(paste0('rate of change in TFR')) +
        theme(axis.text.x = element_text(size = 14)) +
        facet_wrap(~country) +
        ylab('') + xlab('')
    })
  })