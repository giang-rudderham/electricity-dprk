library(rgdal) # for readOGR function
library(leaflet)
library(ggplot2) # for barplot
library(stringr) # for str_wrap function

# Use leaflet to create map
######################
#read shapefile
dprk.shp <- readOGR(dsn = "PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF

#read data
data <- read.csv("popdensity.csv")

#Merge shapefile with DPRK data
dprk.shp$Province <- dprk.shp$NAME_1 #create "Province" to merge by "Province";
dprk.shp@data <- merge(dprk.shp@data, data, by = "Province")

#look at the data of dprk.shp
dprk.map2 <- dprk.shp@data 

# Create color pallete
binpal <- colorBin(palette = "Reds", domain = dprk.shp$PopDensity,
                   bins = c(0, 200, 400, 600, 800, 1000, 4500), pretty = FALSE)

m <- leaflet() %>% 
  setView(lng = 128, lat = 40, zoom = 7) %>%
  addPolygons(data = dprk.shp, stroke = TRUE, color = "black", weight = 1,
              smoothFactor = 1,
              fillOpacity = 0.5, fillColor = binpal(dprk.shp$PopDensity),
              popup = paste("Province:", dprk.shp$Display, "<br>",
                            "Population Density:", dprk.shp$PopDensity),
              group = "density", layerId = ~Display) %>%
  addLegend(position = "bottomright", pal = binpal, 
            values = dprk.shp$PopDensity, opacity = 0.5,
            title = "Population Density") %>%
  addMarkers(lng = 125.8, lat = 38.97, 
           popup = "The Capital: Pyongyang <br> Population Density: 4216.69") %>%
  addCircles(data = dprk.shp, lng = ~long, lat = ~lat, weight = 1, opacity = 3,
             radius = sqrt(dprk.shp$PercElectricityCooking / pi) * 30000, 
             popup = paste("Province:", dprk.shp$Display, "<br>",
                           "Households that use electricity as cooking fuel:",
                           dprk.shp$PercElectricityCooking, "%"),
             group = "electricitycooking") %>%
  addCircles(data = dprk.shp, lng = ~long, lat = ~lat, weight = 1, opacity = 3,
             radius = sqrt(dprk.shp$PercElectronicHeating / pi) * 30000,
             fillColor = "green", color = "green",
             popup = paste("Province:", dprk.shp$Display, "<br>",
                           "Households that use electronic heating:",
                           dprk.shp$PercElectronicHeating, "%"),
             group = "electronicheating")


m %>%
  addMarkers(lng = 128.30, lat = 40.37, popup = "sh")


m %>% addTiles()

# Generate bar plot for heating
##############################
heating <- read.csv(file = "heating.csv")
input <- "Pyongyang"
df <- data.frame(type = c("Central/Local", "Electronic", "Electronic with others",
                          "Coal boiler/Briquette hole", "Wood hole", "Others"),
                 households = as.numeric(heating[heating$Province == input, 3:8])
                 )

b <- ggplot(data = df, aes(x = type, y = households)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = households), vjust = -0.3, size = 3.5) + #display y values on bars
  labs(title = paste("Households by Type of Heating System in", input), 
       x = "Type of Heating System", y = "Number of Households") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + #wrap x var names
  ylim(0, max(heating[ , 3:8]))
b

# Generate bar plot for cooking fuel
################################
cooking <- read.csv(file = "cooking.csv")
inputC <- "Pyongyang"
dfC <- data.frame(typeC = c("Electricity", "Gas", "Petroleum", "Coal", "Wood", "Others"),
                 householdsC = as.numeric(cooking[cooking$Province == inputC, 3:8])
                 )
bC <- ggplot(data = dfC, aes(x = typeC, y = householdsC)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = householdsC), vjust = -0.3, size = 3.5) + #display y values on bars
  labs(title = paste("Households by Type of Cooking Fuel in", inputC), 
       x = "Type of Cooking Fuel Used", y = "Number of Households") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + #wrap x var names
  ylim(0, max(cooking[ , 3:8]))
bC
