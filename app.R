
library(shiny)
library(leaflet)
library(tidyverse)
df <- read.csv(textConnection(
  "Name,Lat,Long, Specialty1, content
  Dendrogreif,54.092478,   13.364295, Everything,  <a href = https://botanik.uni-greifswald.de/en/landscape-ecology-and-ecosystem-dynamics/laboratories/dendroecological-lab-dendrogreif/ > Dendrogreif </a>
  Marieke&Ernst,50.985248,  13.580322, Alles, <a href = https://tu-dresden.de/bu/umwelt/forst/ww/waldwachstum/ >  Chair of Forest growth </ a>"
), stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  div(style="display:inline-block; width: 33%",
      selectInput("filter1", NULL, df$Name)),
  div(style="display:inline-block; width: 33%", 
      uiOutput("filter2")),
  div(style="display:inline-block; width: 33%",
      selectInput("filter3", NULL, c(providers), 'Esri')),
  leafletOutput("mymap", height=600)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Creating a reactive object to automatically update the choices of the first two filters
  
  v <- reactiveValues(df = NULL)
  
  observeEvent(input$filter1, {
    v$df <- df %>% filter(Name == input$filter1)
  })
  
  output$filter2 <- renderUI({selectInput("filter2", NULL, v$df$Specialty1)})
  
  output$mymap <- renderLeaflet({
    print(str(df))
    v$df %>% 
      leaflet() %>%
      addProviderTiles(input$filter3,
                       options = providerTileOptions(noWrap = TRUE), group = 'Filter') %>%
      addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
      addProviderTiles('Esri.WorldImagery', group = 'Sat') %>%
      addProviderTiles('Stamen.TonerLabels', group = 'Sat') %>%
      addLayersControl(baseGroups = c('Filter', 'Topo', 'Sat')) %>%
      addCircleMarkers(~Long, ~Lat, popup=~content) %>%
      
      setView(10, 50, zoom = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
