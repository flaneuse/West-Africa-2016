# Define sidebar for inputs -----------------------------------------------

sidebar <- dashboardSidebar(
  
  
  checkboxGroupInput("countryList",label = NULL, inline = FALSE,
                     choices = countries,
                     selected = countries),
  # sliderInput('tfrChg', label = 'change in TFR from 1980-1985',
              # min = minChg, max = maxChg, post = '%', value = c(minChg, maxChg)),
  
  # -- Sidebar icons --
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("each country", tabName = "indivTab", icon = icon("crosshairs")),
    menuItem("maps", tabName = "choroTab", icon = icon("map-o")),
    menuItem("TFR over time", tabName = "plot", icon = icon("bar-chart"))
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
                tabPanel('Benin', 
                         indivRateUI('benin')),
                tabPanel('Burkina Faso', 
                         indivRateUI('burkina')),
                tabPanel('Cabo Verde', 
                         indivRateUI('cabo')),
                tabPanel('Cameroon', 
                         indivRateUI('cameroon')),
                tabPanel("Cote d'Ivoire", 
                         indivRateUI('cote')),
                tabPanel('Gambia', 
                         indivRateUI('gambia')),
                tabPanel('Ghana', 
                         indivRateUI('ghana')),
                tabPanel('Guinea', 
                         indivRateUI('guinea')),
                tabPanel('Guinea-Bissau', 
                         indivRateUI('g-b')),
                tabPanel('Liberia', 
                         indivRateUI('liberia')),
                tabPanel('Mali', 
                         indivRateUI('mali')),
                tabPanel('Mauritania', 
                         indivRateUI('mauritania')),
                tabPanel('Niger', 
                         indivRateUI('niger')),
                tabPanel('Nigeria', 
                         indivRateUI('nigeria')),
                tabPanel('Senegal', 
                         indivRateUI('senegal')),
                tabPanel('Sierra Leone', 
                         indivRateUI('sl')),
                tabPanel('Togo', 
                         indivRateUI('togo')),
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