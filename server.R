library(shiny)
library(rgdal) # for readOGR
library(leaflet)

#read shapefile
dprk.shp <- readOGR(dsn = "PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF

#read data
data <- read.csv("popdensity.csv")

#Merge shapefile with DPRK data
dprk.shp$Province <- dprk.shp$NAME_1 #create "Province" to merge by "Province";
dprk.shp@data <- merge(dprk.shp@data, data, by = "Province")

# Create color pallete
binpal <- colorBin(palette = "Reds", domain = dprk.shp$PopDensity,
                   bins = c(0, 200, 400, 600, 800, 1000, 4500), pretty = FALSE)

shinyServer(function(input, output) {
  
  #static part of the map
  output$dprkmap <- renderLeaflet({
    leaflet () %>%
      setView(lng = 128, lat = 40, zoom = 7) %>%
      addPolygons(data = dprk.shp, stroke = TRUE, color = "black", weight = 1,
                  smoothFactor = 1,
                  fillOpacity = 0.5, fillColor = binpal(dprk.shp$PopDensity),
                  popup = paste("Province:", dprk.shp$Display, "<br>",
                                "Population Density:", dprk.shp$PopDensity),
                  group = "density") %>%
      addLegend(position = "bottomright", pal = binpal, 
                values = dprk.shp$PopDensity, opacity = 0.5,
                title = "Population Density") %>%
      addMarkers(lng = 125.8, lat = 38.97, 
                 popup = "The Capital: Pyongyang <br> Population Density: 4216.69")
  })  
  
  # put dynamic parts of the map in observers
  ## add circles
  observe({
    proxy <- leafletProxy("dprkmap", data = dprk.shp)
    proxy %>% clearGroup(group = "electricity")
    if (input$cooking) {
      proxy %>%
        addCircles(data = dprk.shp, lng = ~long, lat = ~lat, weight = 1, opacity = 3,
                   radius = sqrt(dprk.shp$PercElectricity / pi) * 30000, 
                   popup = paste("Province:", dprk.shp$Display, "<br>",
                                 "Households that use electricity as cooking fuel:",
                                 dprk.shp$PercElectricity, "%"),
                   group = "electricity")
    }
  })
})
