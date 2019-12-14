#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(rgdal)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Area Median Income Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  blockgroups <- rgdal::readOGR('data/BlockGroupsHC/BlockGroupsHC.shp') %>%
  spTransform(CRSobj = CRS("+init=epsg:4326"))
  
  output$mymap <- renderLeaflet({
    leaflet(blockgroups) %>%
    addTiles() %>%
    addPolygons(color = "#CCCCCC", 
               fillColor = "#FFFFFF",
               opacity = 1.0,
               fillOpacity = 0.5,
               weight = 1,
               smoothFactor = 0.5,
               highlightOptions = highlightOptions(
                 color = "green",
                 weight = 2,
                 bringToFront = TRUE
                )
              )
   })
  
  #observe()
}

# Run the application 
shinyApp(ui = ui, server = server)

