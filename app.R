
library(shiny)
library(leaflet)
library(tidyverse)
df <- read.csv("./www/ATR tree-ring lab map (Responses) - Form Responses 1.csv", stringsAsFactors = FALSE)### Place it in "www" folder in app directory

df$content <- df %>% {paste("<a href = ", .[["Webpage.URL"]] ,"rel='noopener noreferrer' target='_blank'>" , .[["Lab.name...Institution"]] ,  "</a>", "<br>",
                            "Lab lead: <a href = ", paste("mailto:", .[["Email"]] ,sep = ""), ">" , paste(.[["Academic.title"]], .[["Surname"]], .[["Family.name"]]), "</a> <br />", 
                           paste0("<p><b>", "Address:<br/>", "</b>", .[["Address"]], ", ",.[["Postal.code...city"]], ", ",
                           .[["Country"]],"</p>"), 
                           "<p><b>", "Research foci:", "</b><br />",.[["Research.focus.of.the.lab"]], "</p>",
                           "<p><b>", "Infrastructure for measuring:", "</b><br />", .[["Infrastructure.is.available.for.measuring."]], "</p>", 
                           "<p><b>", "Additional info:", "</b><br />", .[["Additional.lab.info"]], "</p>")}


specialties <- c("Research focus of the lab:", "Dendroecology", "Dendroclimatology", "Dendrogeomorphology", 
                 "Dendroarchaeology", "Dendrochemistry")

infrastructure <- c("Available infrastructure for measuring:", "tree-ring widths", "wood density", "blue intensity", 
                    "wood anatomy", "wood chemistry", "stable isotopes")


# Define UI for application that draws a histogram
ui <- fluidPage(
  div(style="display:inline-block; width: 48%",
      selectInput("focus", NULL, specialties, width='100%')),
  div(style="display:inline-block; width: 48%; float:right", 
      selectInput("infrastructure", NULL, infrastructure, width='100%')),
  leafletOutput("mymap", height='600px')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ### Creating a reactive object to automatically update the choices of the filters
  pos_vec <- reactive({
    if(input$focus == "Research focus of the lab:"){
      seq_len(nrow(df))
    }else{
      sel_vec <- sapply(seq_len(nrow(df)), function(x){
        input$focus %in% (df[x,]$Research.focus.of.the.lab %>% {unlist(strsplit(., split = ", "))})
      })
      which(sel_vec == TRUE)
    }
  })
  
  pos_vec2 <- reactive({
    if(input$infrastructure == "Available infrastructure for measuring:"){
      seq_len(nrow(df))
    }else {
        sel_vec_filt2 <- sapply(seq_len(nrow(df)), function(x){
        input$infrastructure %in% (df[x,]$Infrastructure.is.available.for.measuring %>% {unlist(strsplit(., split = ", "))})
      })
      which(sel_vec_filt2 == TRUE)
    }
  })
  
  output$mymap <- renderLeaflet({
     intersect(df[pos_vec(), ], df[pos_vec2(),])  %>% 
      leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap', group = 'Terrain') %>%
      addProviderTiles('Esri.WorldImagery', group = 'Satellite') %>%
      addProviderTiles('Stamen.TonerLabels', group = 'Satellite') %>%
      addLayersControl(baseGroups = c('Terrain', 'Satellite')) %>%
      addCircleMarkers(~Longitude, ~Latitude, popup=~content) %>%
      
      setView(10, 50, zoom = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
