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
    menuItem("each country", tabName = "indivTab", icon = icon("crosshairs")),
    menuItem("maps", tabName = "choroTab", icon = icon("map-o")),
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
    
    # -- Individual country plots --
    tabItem(tabName = 'indivTab',
            fluidRow(
              tabBox(
                tabPanel('Niger', 
                         fluidRow(column(6,plotOutput('indivRate')),
                                  column(6, plotOutput('indivChoro')))),
                tabPanel('Mali', 'text2'),
                width = 12
              )
            )),
    
    # -- Choropleth --
    tabItem(tabName = "choroTab",
            fluidRow(column(6,
                            plotOutput('choro')),
                     column(6,
                            plotOutput('choroChg')))),
    # -- Basic plot -- 
    tabItem(tabName = "plot", 
            fluidRow(plotOutput('plot1', height = '500px')),
            fluidRow(imageOutput('footer', width = '100%')))
  ))



# Dashboard definition (main call) ----------------------------------------

dashboardPage(
  title = "Fertility in West Africa",  
  header,
  sidebar,
  body
)