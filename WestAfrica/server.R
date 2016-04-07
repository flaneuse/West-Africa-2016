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
    
    callModule(indivRate, 'benin', tfr, 'Benin')
    callModule(indivRate, 'burkina', tfr, 'Burkina Faso')
    callModule(indivRate, 'cabo', tfr, 'Cabo Verde')
    callModule(indivRate, 'cameroon', tfr, 'Cameroon')
    callModule(indivRate, 'cote', tfr, "Côte d'Ivoire")
    callModule(indivRate, 'gambia', tfr, 'Gambia')
    callModule(indivRate, 'ghana', tfr, 'Ghana')
    callModule(indivRate, 'guinea', tfr, 'Guinea')
    callModule(indivRate, 'g-b', tfr, 'Guinea-Bissau')
    callModule(indivRate, 'liberia', tfr, 'Liberia')
    callModule(indivRate, 'mali', tfr, 'Mali')
    callModule(indivRate, 'mauritania', tfr, 'Mauritania')
    callModule(indivRate, 'niger', tfr, 'Niger')
    callModule(indivRate, 'nigeria', tfr, 'Nigeria')
    callModule(indivRate, 'senegal', tfr, 'Senegal')
    callModule(indivRate, 'sl', tfr, 'Sierra Leone')
    callModule(indivRate, 'togo', tfr, 'Togo')
    
    
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