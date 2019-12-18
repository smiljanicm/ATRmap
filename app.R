library(shiny)
library(leaflet)
library(tidyverse)
df <- read.csv(textConnection(
  "Name,Lat,Long, content
Dendrogreif,54.092478, 13.364295, <a href = https://botanik.uni-greifswald.de/en/landscape-ecology-and-ecosystem-dynamics/laboratories/dendroecological-lab-dendrogreif/ > Dendrogreif </a>
Marieke&Ernst,50.985248, 13.580322, <a href = https://tu-dresden.de/bu/umwelt/forst/ww/waldwachstum/ >  Chair of Forest growth </ a>"
), stringsAsFactors = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  div(style="display:inline-block; width: 33%",
      selectInput("filter1", NULL, c("Filter 1" = "", LETTERS))),
  div(style="display:inline-block; width: 33%", 
      selectInput("filter2", NULL, c("Filter 2" = "", LETTERS))),
  div(style="display:inline-block; width: 33%",
      selectInput("filter3", NULL, c(providers), 'Esri')),
   leafletOutput("mymap", height=600)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    print(str(df))
    df %>%
      filter(Name != 'A') %>% #we do filtering here
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

