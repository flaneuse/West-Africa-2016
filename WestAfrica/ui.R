# Define sidebar for inputs -----------------------------------------------

sidebar <- dashboardSidebar(
  
  
  # -- Muscle filtering --
  checkboxGroupInput("countryList",label = NULL, inline = FALSE,
                     choices = countries,
                     selected = countries),
  sliderInput('tfrChg', label = 'change in TFR from 1980-1985',
              min = minChg, max = maxChg, post = '%', value = c(minChg, maxChg)),
  
  # -- Sidebar icons --
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("plot", tabName = "plot", icon = icon("bar-chart"))
  )
)


# Header ------------------------------------------------------------------
header <- dashboardHeader(
  title = "TFR in West Africa"
)



# Body --------------------------------------------------------------------

body <- dashboardBody(
  
  # -- Each tab --
  tabItems(
    
    # -- Basic plot -- 
    tabItem(tabName = "plot", 
            plotOutput('plot1')
    )))



# Dashboard definition (main call) ----------------------------------------

dashboardPage(
  title = "Fertility in West Africa",  
  header,
  sidebar,
  body
)