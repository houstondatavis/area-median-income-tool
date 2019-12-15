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
        selectInput("limitInput",
          label = h4("Limit"),
          choices = c(
            "Extremely Low Income" = "pct_below_eli",
            "50%" = "pct_below_ami50",
            "80%" = "pct_below_ami80",
            "100%" = "pct_below_ami100",
            "120%" = "pct_below_ami120"
          ),
          selected = "Extremely Low Income"
        ),
        textOutput("pop_numbers"),
        textOutput("selected_ami"),
        textOutput("selected_geoid"),
        plotOutput("pop_plot")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap")
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {

  blockgroups <- rgdal::readOGR('data/BlockGroupsHC/BlockGroupsHC.shp', stringsAsFactors=FALSE) %>%
    spTransform(CRSobj = CRS("+init=epsg:4326"))
  
  ami_data <- read.csv('data/income_ami.csv', stringsAsFactors=FALSE)
  
  blockgroups_with_ami_data <- merge(
    blockgroups,
    ami_data,
    by = "GEOID"
  )
  
  ami_groups <- c("pct_below_eli", "pct_below_ami50", "pct_below_ami80", "pct_below_ami100", "pct_below_ami120")
  highlighted_block_options <- highlightOptions(
    color = "green",
    fillColor = "green",
    weight = 2,
    bringToFront = TRUE
  )
  mymap <- leaflet(blockgroups_with_ami_data) %>%
    addTiles() %>%
    setView(-95.4757516, 29.8133807, zoom = 10) %>%
    addPolygons(
      color = "#CCCCCC",
      opacity = 1.0,
      fillOpacity = 0.75,
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlighted_block_options,
      group = "pct_below_eli",
      fillColor = ~colorNumeric("YlOrRd", pct_below_eli)(pct_below_eli)
    ) %>%
    addPolygons(
      color = "#CCCCCC",
      opacity = 1.0,
      fillOpacity = 0.75,
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlighted_block_options,
      group = "pct_below_ami50",
      fillColor = ~colorNumeric("YlOrRd", pct_below_ami50)(pct_below_ami50)
    ) %>%
    addPolygons(
      color = "#CCCCCC",
      opacity = 1.0,
      fillOpacity = 0.75,
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlighted_block_options,
      group = "pct_below_ami80",
      fillColor = ~colorNumeric("YlOrRd", pct_below_ami80)(pct_below_ami80)
    ) %>%
    addPolygons(
      color = "#CCCCCC",
      opacity = 1.0,
      fillOpacity = 0.75,
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlighted_block_options,
      group = "pct_below_ami100",
      fillColor = ~colorNumeric("YlOrRd", pct_below_ami100)(pct_below_ami100)
    ) %>%
    addPolygons(
      color = "#CCCCCC",
      opacity = 1.0,
      fillOpacity = 0.75,
      weight = 1,
      smoothFactor = 0.5,
      highlightOptions = highlighted_block_options,
      group = "pct_below_ami120",
      fillColor = ~colorNumeric("YlOrRd", pct_below_ami120)(pct_below_ami120)
    ) %>%
    hideGroup(ami_groups)

  #bg_js <- geojsonio::geojson_json(blockgroups_with_ami_data)
  selectedBG <- reactive({
    
    click <- input$mymap_shape_click
    
    if(is.null(click))
      return()
    
    #pulls lat and lon from shiny click event
    lat <- click$lat
    lon <- click$lng
    
    #puts lat and lon for click point into its own data frame
    coords <- as.data.frame(cbind(lon, lat))
    
    #converts click point coordinate data frame into SP object, sets CRS
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+init=epsg:4326")
    print("Point:")
    print(point)
    
    
    #retrieves country in which the click point resides, set CRS for country
    selected <- blockgroups_with_ami_data[point,]
    print("Selected:")
    print(selected@data[38:42])
    selected
    
  })

  selectedLimit <- reactive({
    input$limitInput
  })
  
  #observe({
    output$pop_plot <- renderPlot({
      req(input$mymap_shape_click)
      barplot(as.numeric(selectedBG()@data[38:42]))
    })
  #})
  
  #observe({
    output$pop_numbers <- renderText({
      req(input$mymap_shape_click)
      selectedBG()@data[["NAMELSAD"]]
    })
  #})
  output$selected_geoid <- renderText({
    req(input$mymap_shape_click)
    selectedBG()@data[["GEOID"]]
  })
  output$mymap <- renderLeaflet(mymap)

  observeEvent(input$limitInput, {
    leafletProxy("mymap", session) %>%
      hideGroup(ami_groups) %>%
      showGroup(selectedLimit())
  })
  
  observeEvent(input$mymap_shape_click, {
    leafletProxy("mymap", session) %>%
      clearGroup("selected") %>%
      addPolygons(
        color = "green",
        weight = 4,
        data = selectedBG(),
        group = "selected"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

