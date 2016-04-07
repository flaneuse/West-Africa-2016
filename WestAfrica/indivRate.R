indivRateUI = function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(column(6,plotOutput(ns('indivPlot'))),
             column(6, plotOutput(ns('indivChoro')))),
    
    fluidRow(column(6,plotOutput(ns('indivRate'))),
             column(6, plotOutput(ns('indivChoroChg')))
    ),
    fluidRow(imageOutput(ns('indivFooter'), width = '90%'))
  )
}

indivRate = function(input, output, session, tfr, choroData, selCountry){
  
  filterTFR = reactive({
    tfr %>% 
      filter(country == selCountry)
  })
  
  output$indivFooter = renderImage({
    return(list(
      src = "img/footer_WestAfrica_WPP.png",
      width = '100%',
      filetype = "image/png",
      alt = "Plots from USAID's GeoCenter"
    ))
  }, deleteFile = FALSE)

  
  output$indivPlot = renderPlot({
    
    # Filter down the data
    filteredTFR = filterTFR()
    
    recentTFR = filteredTFR %>% filter(year == '2010-2015')
    
    compTFR = filteredTFR %>% filter(year == '1980-1985')
    
    p = ggplot(filteredTFR, aes(x = year, y = tfr, group = country)) +
      geom_line(colour = grey50K,
                aes(y = tfrWAfr)) +
      geom_line(colour = accentColor) +
      geom_segment(aes(x = '1980-1985', xend = '2010-2015',
                       y = 7.5, yend = 7.5),
                   arrow = arrow(length = unit(0.03, "npc"), ends = 'both'),
                   colour = ltColor) +
      geom_text(aes(x = '1995-2000',
                    size = 4,
                    y = 7.5, label = paste0('change: ', percent(refRate))),
                nudge_y = -0.3,   
                colour = ltColor, data = recentTFR) +
      geom_point(colour = accentColor,
                 size = 2.5,
                 data = recentTFR)+
      geom_point(colour = accentColor,
                 data = compTFR)+
      geom_point(aes(y = tfrWAfr),
                 data = recentTFR,
                 size = 2.5,
                 colour = grey50K) +
      geom_text(aes(label = 'West Africa (excluding Cameroon)', 
                    x = 1, y = 1), 
                colour = grey50K,
                size = 5,
                nudge_y = 0.4,
                hjust = 0, 
                data = filteredTFR %>% filter(year == '1950-1955')) +
      geom_text(aes(label = selCountry, 
                    x = 1, y = 1.75), 
                colour = accentColor,
                size = 5,
                nudge_y = 0.4,
                hjust = 0,
                data = filteredTFR %>% filter(year == '1950-1955')) +
      geom_text(aes(label = round(tfr,1)), 
                colour = accentColor,
                size = 6,
                nudge_y = -0.8,
                hjust = 1,
                data = recentTFR) +
      geom_text(aes(label = round(tfr,1)), 
                colour = accentColor,
                size = 6,
                nudge_y = -0.8,
                hjust = 1,
                data = compTFR) +
      scale_x_discrete(breaks = c('1950-1955','1960-1965', '1970-1975', 
                                  '1980-1985', '1990-1995', '2000-2005','2010-2015'),
                       labels = c('1950-1955','',  '',
                                  '1980-1985', 
                                  '','','2010-2015')) +
      theme_xygridlight() + 
      scale_y_continuous(limits = c(0,8)) +
      ggtitle(paste0(selCountry, ': total fertility rate')) +
      xlab('') + ylab('')+
      theme(axis.text.x = element_text(size = 12))
    return(p)
  })
  
  output$indivChoro = renderPlot({
    
    selYear = '2010-2015'
    
     
    filteredTFR = tfr %>% 
      filter(year == selYear) %>% 
      ungroup() %>% 
      select(country, year, value = tfr, rate, refRate) %>% 
      mutate(colText = ifelse(value > 1.25*mean(value), grey10K, grey90K),
             country = str_to_lower(country))
    
    filteredChoro = left_join(choroData, filteredTFR,
                              by = c("country" = "country"))
    
    if(selCountry == 'Cabo Verde') {
      selChoroLab = filteredChoro
    } else{
      selChoroLab = filteredChoro %>% filter(country == str_to_lower(selCountry))
    }
    
    
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
                data = selChoroLab) +
      scale_colour_identity() +
      scale_size(guide = FALSE) +
      guides(fill = guide_colorbar(ticks = FALSE)) +
      ggtitle(selYear)
  })
  
  
  output$indivChoroChg = renderPlot({
    selYear = '2010-2015'
    
    filteredChg = tfr %>% 
      filter(year == selYear) %>% 
      ungroup() %>% 
      select(country, year, tfr, rate, value = refRate) %>% 
      mutate(colText = ifelse(value < 1.25*mean(value), grey10K, grey90K),
             country = str_to_lower(country))
    
    filteredChoro = left_join(choroData, filteredChg,
                              by = c("country" = "country"))
    
    if(selCountry == 'Cabo Verde') {
      selChoroLab = filteredChoro
    } else{
      selChoroLab = filteredChoro %>% filter(country == str_to_lower(selCountry))
    }
    
    
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
                data = selChoroLab) +
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