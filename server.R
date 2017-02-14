library(shiny)
library(rgdal) # for readOGR
library(leaflet)

# Create data frame for circles
sanitation <- read.csv("data/sanitationCleaned.csv", stringsAsFactors = FALSE)

# Create SpatialPolygonsDataFrame
data <- read.csv("data/underNourishedCleaned.csv", stringsAsFactors = F)

## fix countries in failed matches
# library(ggmap) # for function geocode
# data$Country[grep("Palestine", data$Country)] <- "Palestine"
# ## to accommodate geocode
# 
# for (fix in c("Gibraltar", "Guadeloupe", "Libyan Arab Jamahiriya",
#               "Martinique", "Mayotte", "Reunion", "Tokelau",
#               "Palestine")) {
#   data$Country[which(data$Country == fix)] <-
#     geocode(fix, output = "more")$country
# }

joined.to.map <- joinCountryData2Map(data, joinCode = "NAME", 
                                     nameJoinColumn = "Country")


shinyServer(function(input, output, session) {
  
  # reactive input
  input_year <- reactive({
    paste0("X", input$yearmap, collapse = "") 
  })
  
  # static part of the map
  output$worldmap <- renderLeaflet({
    leaflet() %>% #create a leaflet object
      addTiles() %>% #add a base map
      setView(lng = 0, lat = 55, zoom = 2)
  })
  
  # put dynamic parts of the map in observers
  ## add polygons for pop. undernourished
  observe({
    chosen_year <- input_year()
    binpal <- colorBin(palette = "Reds", domain = joined.to.map[[chosen_year]],
                       bins = c(0, 20, 40, 60, 80, 100), pretty = FALSE)
    leafletProxy("worldmap", data = joined.to.map) %>%
      clearShapes() %>% 
      addPolygons(stroke = FALSE, smoothFactor = 1, fillOpacity = 0.5,
                  color = binpal(joined.to.map[[chosen_year]]),
                  popup = paste("Country:", joined.to.map$Country, "<br>",
                                "Undernourished:", joined.to.map[[chosen_year]],
                                "%"),
                  group = "undernourished"
      )          
  })
  
  ## add legend for polygons
  observe({
    chosen_year <- input_year()
    binpal <- colorBin(palette = "Reds", domain = joined.to.map[[chosen_year]],
                       bins = c(0, 20, 40, 60, 80, 100), pretty = FALSE)
    
    leafletProxy("worldmap", data = joined.to.map) %>%
      clearControls() %>%
      addLegend(position = "bottomright", pal = binpal,
                values = joined.to.map[[chosen_year]], opacity = 0.5,
                title = "Population undernourished (%)"
      )
  })
  
  ## add markers for pop. access sanitation
  observe({
    proxy <- leafletProxy("worldmap", data = sanitation)
    proxy %>% clearMarkers()
    if (input$sanitation) {
      chosen_year <- input_year()
      proxy %>%
        addCircleMarkers(lng = ~long, lat = ~lat, stroke = TRUE, 
                         color = "#03F", weight = 1, opacity = 3,
                         fillOpacity = 0.2, 
                         radius = ( 101 - sanitation[[chosen_year]] ) / 5,
                         popup = paste("Country:", sanitation$Country, "<br>",
                                       "Sanitation facility:", 
                                       sanitation[[chosen_year]], "%"),
                         group = "sanitation")
    }
  })
  
  
  
  
  output$hist <- renderPlot({
    chosen_hist <- paste0("X", input$yearhist, collapse = "")
    hist(data[[chosen_hist]],
         breaks = c(0, 20, 40, 60, 80, 100),
         col = "grey",
         ylim = c(0, 140),
         xlab = "Population Undernourished (%)",
         main = "Histogram of Population Undernourished")
    box()
  })
})

