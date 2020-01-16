
library(shiny)
library(leaflet)
library(tidyverse)
df <- read.csv("./www/ATR tree-ring lab map.csv", stringsAsFactors = FALSE)### Place it in "www" folder in app directory
df$content <- df %>% {paste("<a href = ", .[["Webpage.URL"]] ,">" , .[["Lab.name...Institution"]] ,  "</a>")} ### adding column with formatted string for "addCircleMarkers()"

specialties <- c("Dendroecology", "Dendroclimatology", "Dendrogeomorphology", 
                 "Dendroarchaeology", "Dendrochemistry")

# Define UI for application that draws a histogram
ui <- fluidPage(
  div(style="display:inline-block; width: 33%",
      selectInput("filter1", NULL, specialties)),
  div(style="display:inline-block; width: 33%", 
      uiOutput("filter2")),
      leafletOutput("mymap", height=600)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Creating a reactive object to automatically update the choices of the first two filters
  
  v <- reactiveValues(df = NULL)
  
  observeEvent(input$filter1, {
    
    sel_vec <- vector(mode = "numeric", length = nrow(df))
    for(j in 1:nrow(df)){
      sel_vec[j] <- input$filter1 %in% (df[j,]$Research.focus.of.the.lab %>% {unlist(strsplit(., split = ";"))})
    }
    
    pos_vec <- which(sel_vec == TRUE)
    v$df <- df[pos_vec,]
  })
  
  output$filter2 <- renderUI({selectInput("filter2", NULL, ifelse(nrow(v$df) != 0, v$df$Lab.name...Institution, "None"))})
  
  output$mymap <- renderLeaflet({
    print(str(df))
    v$df %>% 
      leaflet() %>%
      addProviderTiles("Esri",
                       options = providerTileOptions(noWrap = TRUE), group = 'Filter') %>%
      addProviderTiles('Esri.WorldTopoMap', group = 'Topo') %>%
      addProviderTiles('Esri.WorldImagery', group = 'Sat') %>%
      addProviderTiles('Stamen.TonerLabels', group = 'Sat') %>%
      addLayersControl(baseGroups = c('Filter', 'Topo', 'Sat')) %>%
      addCircleMarkers(~Longitude, ~Latitude, popup=~content) %>%
      
      setView(10, 50, zoom = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
