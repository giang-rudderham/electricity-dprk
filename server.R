library(shiny)
library(rgdal) # for readOGR function
library(leaflet)
library(ggplot2) # for barplot
library(stringr) # for str_wrap function

# Version 3: add bar plot
##############################

#read shapefile
dprk.shp <- readOGR(dsn = "PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF

#read data
data <- read.csv("popdensity.csv")
heating <- read.csv(file = "heating.csv") # heating system data
cooking <- read.csv(file = "cooking.csv") # cooking fuel data

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
                   group = "electricitycooking"#, layerId = ~Display
                   )
    }
  })
  
  ## add circles for heating system
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
                   group = "electronicheating"#, layerId = ~Display
                   )
    }
  })
  
  ## clicking on any province generates bar plot of heating energy for that province

  df_reactive <- eventReactive(input$dprkmap_shape_click, {
    p <- input$dprkmap_shape_click
    data.frame(type_heating = c("Central/Local", "Electronic", "Electronic with others",
                        "Coal boiler/Briquette hole", "Wood hole", "Others"),
               households_heating = as.numeric(heating[heating$Province == p$id, 3:8]),
               type_cooking = c("Electricity", "Gas", "Petroleum", "Coal", "Wood", "Others"),
               households_cooking = as.numeric(cooking[cooking$Province == p$id, 3:8]),
               province_name = p$id #this extra column stores province for title of plot later
    )
  })


  output$barCooking <- renderPlot({
    ggplot(data = df_reactive(), aes(x = type_cooking, y = households_cooking)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = households_cooking), vjust = -0.3, size = 3.5) + #display y values on bars
      labs(title = paste("Households by Type of Cooking Fuel in", df_reactive()[1, 5]),
           #get province name from fifth column of data frame from df_reactive() earlier
           x = "Type of Cooking Fuel Used", y = "Number of Households") +
      theme_minimal() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + #wrap x var names 
      ylim(0, max(cooking[ , 3:8])) #set same y axis for bar plots of all provinces
  })
  
  output$barHeating <- renderPlot({
    ggplot(data = df_reactive(), aes(x = type_heating, y = households_heating)) +
      geom_bar(stat = "identity", fill = "green") +
      geom_text(aes(label = households_heating), vjust = -0.3, size = 3.5) + #display y values on bars
      labs(title = paste("Households by Type of Heating System in", df_reactive()[1, 5]),
            #get province name from fifth column of data frame from df_reactive() earlier
           x = "Type of Heating System", y = "Number of Households") +
      theme_minimal() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + #wrap x var names 
      ylim(0, max(heating[ , 3:8])) #set same y axis for bar plots of all provinces
  })
  
  
})
