library(shiny)
library(rgdal) # for readOGR
library(leaflet)

# Version 3: update map with clicking on the provinces #################
################################################

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
                  group = "density",
                  layerId = ~Display) %>%
      addLegend(position = "bottomright", pal = binpal, 
                values = dprk.shp$PopDensity, opacity = 0.5,
                title = "Population Density") %>%
      addMarkers(lng = 125.8, lat = 38.97, 
                 popup = paste("The Capital: Pyongyang <br> Population Density:",
                               dprk.shp$PopDensity[dprk.shp$Display == "Pyongyang"], "<br>",
                               "Households that use electricity as cooking fuel:",
                               dprk.shp$PercElectricityCooking[dprk.shp$Display == "Pyongyang"], "%",
                               "<br>", "Households that use electronic heating:",
                               dprk.shp$PercElectronicHeating[dprk.shp$Display == "Pyongyang"], "%")
      )
  })  
  
  # put dynamic parts of the map in observers
  ## add circles for cooking fuel
  observe({
    proxy <- leafletProxy("dprkmap", data = dprk.shp)
    proxy %>% clearGroup(group = "electricitycooking")
    if (input$cooking) {
      proxy %>%
        addCircles(data = dprk.shp, lng = ~long, lat = ~lat, weight = 1, opacity = 3,
                   radius = sqrt(dprk.shp$PercElectricityCooking / pi) * 30000, 
                   popup = paste("Province:", dprk.shp$Display, "<br>",
                                 "Households that use electricity as cooking fuel:",
                                 dprk.shp$PercElectricityCooking, "%"),
                   group = "electricitycooking",
                   layerId = ~Display)
    }
  })
  
  ## add cricles for heating system
  observe({
    proxy <- leafletProxy("dprkmap", data = dprk.shp)
    proxy %>% clearGroup(group = "electronicheating")
    if (input$heating) {
      proxy %>%
        addCircles(data = dprk.shp, lng = ~long, lat = ~lat, weight = 1, opacity = 3,
                   radius = sqrt(dprk.shp$PercElectronicHeating / pi) * 30000,
                   fillColor = "green", color = "green",
                   popup = paste("Province:", dprk.shp$Display, "<br>",
                                 "Households that use electronic heating:",
                                 dprk.shp$PercElectronicHeating, "%"),
                   group = "electronicheating", 
                   layerId = ~Display)
    }
  })
  
  ## clicking on marker at Pyongyang generates a marker at South Hamgyong
  observeEvent(input$dprkmap_shape_click, {
    proxy <- leafletProxy("dprkmap", data = dprk.shp)
    p <- input$dprkmap_shape_click
    proxy %>% 
      clearGroup(group = "testpopup") %>%
      addMarkers(lng = 128.30, lat = 40.37, popup = paste(p$lng, p$lat, p$id), group = "testpopup")
    
  })
  
  #output$histCooking <- renderPlot({
   # plot(rnorm(23), rnorm(23))
  #})
})
