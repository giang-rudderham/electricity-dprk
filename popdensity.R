library(rgdal) # for readOGR
library(leaflet)

# Use leaflet
######################
#read shape file
dprk.shp <- readOGR(dsn = "PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF

#read data
data <- read.csv("popdensity.csv")


dprk.shp$Province <- dprk.shp$NAME_1
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
              group = "density") %>%
  addLegend(position = "bottomright", pal = binpal, 
            values = dprk.shp$PopDensity, opacity = 0.5,
            title = "Population Density") %>%
  addMarkers(lng = 125.8, lat = 38.97, 
           popup = "The Capital: Pyongyang <br> Population Density: 4216.69") %>%
  addCircles(data = dprk.shp, lng = ~long, lat = ~lat, weight = 1, opacity = 3,
             radius = sqrt(dprk.shp$PercElectricity / pi) * 30000, 
             popup = paste("Province:", dprk.shp$Display, "<br>",
                           "Households that use electricity as cooking fuel:",
                           dprk.shp$PercElectricity, "%"),
             group = "electricity")

m

m %>%
  addMarkers(lng = 128.30, lat = 40.37, popup = "sh")


m %>% addTiles()

# Use sp package
#################
#read shape file
dprk.shp <- readOGR(dsn = "PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF

#read data
data <- read.csv("popdensity.csv")
data$category <- cut(data$PopDensity, breaks = c(0, 200, 400, 600, 800, 1000, 4500), 
                     include.lowest = TRUE, 
                     labels = c("0-200", "200-400", "400-600", "600-800", "800-1000", "1000+"))

dprk.map2 <- dprk.shp@data #to look at the data of dprk.shp

dprk.shp$Province <- dprk.shp$NAME_1
dprk.shp@data <- merge(dprk.shp@data, data, by = "Province")

spplot(dprk.shp, "PopDensity")

# Use ggplot package
###########################
#read shape file
dprk.shp <- readOGR(dsn = "PRK_adm", layer = "PRK_adm1") #SpatialPolygonsDF
dprk.map<-fortify(dprk.shp)

#read data
data <- read.csv("popdensity.csv")
data$category <- cut(data$PopDensity, breaks = c(0, 200, 400, 600, 800, 1000, 4500), 
                         include.lowest = TRUE, 
                         labels = c("0-200", "200-400", "400-600", "600-800", "800-1000", "1000+"))


merged <- merge(dprk.map, data, by = "id")

#read csv file that has province names and coordinates of labels
provinces <- read.csv("provinces.csv")

ggplot(merged, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = category), colour="black") +
  labs(fill = "Population Density", x = NULL, y = NULL) + 
  scale_fill_brewer(type = "div") +
  ggtitle("Population Density by Province" ) +
  theme_classic() + 
  theme(plot.title = element_text(size=rel(1.5)),
        line = element_blank(), 
        axis.text.x = element_blank(),
        line = element_blank(), 
        axis.text.y = element_blank()) +
  #guides(fill = guide_legend(reverse = TRUE)) + 
  geom_text(data = provinces, aes(long, lat, group=NULL, label = Display), size = 3.8, angle = 20) +
  coord_map() 
