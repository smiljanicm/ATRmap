
library(shiny)
library(leaflet)
library(tidyverse)
df <- read.csv("./www/ATR tree-ring lab map.csv", stringsAsFactors = FALSE)### Place it in "www" folder in app directory
df$content <- df %>% {paste("<a href = ", .[["Webpage.URL"]] ,">" , .[["Lab.name...Institution"]] ,  "</a>")} ### adding column with formatted string for "addCircleMarkers()"

specialties <- c("Show all", "Dendroecology", "Dendroclimatology", "Dendrogeomorphology", 
                 "Dendroarchaeology", "Dendrochemistry")

infrastructure <- c("Show all", "tree-ring widths", "wood density", "blue intensity", 
                    "wood anatomy", "wood chemistry", "stable isotopes")


# Define UI for application that draws a histogram
ui <- fluidPage(
  div(style="display:inline-block; width: 33%",
      selectInput("filter1", NULL, specialties, selected = "Show all")),
  div(style="display:inline-block; width: 33%", 
      selectInput("filter2", NULL, infrastructure, selected = "Show all")),
  leafletOutput("mymap", height=600)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### Creating a reactive object to automatically update the choices of the first two filters
  
  v <- reactiveValues(df = NULL)
  
  pos_vec <- reactive({
    if(input$filter1 == "Show all"){
      
      seq_len(nrow(df))
      
    }else{
      sel_vec <- sapply(seq_len(nrow(df)), function(x){
        input$filter1 %in% (df[x,]$Research.focus.of.the.lab %>% {unlist(strsplit(., split = ";"))})
      })
      which(sel_vec == TRUE)
    }
  })
  
  pos_vec2 <- reactive({
    if(input$filter2 == "Show all"){
      seq_len(nrow(df))
    }else {
        sel_vec_filt2 <- sapply(seq_len(nrow(df)), function(x){
        input$filter2 %in% (df[x,]$Infrastructure.is.available.for.measuring %>% {unlist(strsplit(., split = ";"))})
      })
      which(sel_vec_filt2 == TRUE)
    }
    
  })
  
 
  output$mymap <- renderLeaflet({
    print(str(df))
    
     intersect(df[pos_vec(), ], df[pos_vec2(),])  %>% 
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
