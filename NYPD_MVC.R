#Set up environment and load data into R
library(ggplot2)
library(magrittr)
library(rgdal)
library(sp)
library(leaflet)
library(scales)
setwd("~/Documents/R/NYPD_MVC/")
mvc.raw <- read.csv(file = "NYPD_Motor_Vehicle_Collisions.csv", header = T)
borough_wai <- readOGR("Borough_WAI", layer = "geo_export_e9b6dff4-dd56-4670-b398-e4efe9474a76")
community_wai <- readOGR("Community_WAI", layer = "geo_export_892906eb-17d4-4acd-86b4-33eb7f6d0a12")


#Get info of the data
str(mvc.raw)
head(mvc.raw)

#subset data having lantitude and longitude
mvc.xy <- mvc.raw[!is.na(mvc.raw$LATITUDE) & !is.na(mvc.raw$LONGITUDE), c(1, 2, 5, 6, 24)]

# match polygon coordinate to coordinate of data 
lng_lat <- mvc.xy[, c(3, 4)]
coordinates(lng_lat) <- c("LONGITUDE", "LATITUDE")
proj4string(lng_lat) <- proj4string(borough_wai)
# search events in the polygons
boro <- over(lng_lat, borough_wai)
commu <- over(lng_lat, community_wai)

# drop data containing NA and assign correct attributes to columns
mvc.xy$BOROUGH <- boro$boro_name
mvc.xy$COMMUNITY<- commu$boro_cd
mvc.xy <- mvc.xy[complete.cases(mvc.xy), ]
mvc.xy$DATE <- as.Date(mvc.xy$DATE, format = "%m/%d/%Y")
mvc.xy$COMMUNITY <- as.factor(mvc.xy$COMMUNITY)

# make the plot of collisions incidents grouped by boroughs
qplot(BOROUGH, data = mvc.xy, geom = "bar")
g2 <- qplot(DATE, data = mvc.xy, geom = "histogram", binwidth = 10, facets = . ~ BOROUGH)
g2 + scale_x_date(breaks = date_breaks('4 months'), labels = date_format("%Y-%m")) + 
  theme(axis.text.x = element_text(angle=90))
g1 <- ggplot(mvc.xy, mapping = aes(BOROUGH)) + geom_bar()

#use leaflet package to create a map with collision incident markers
##load borough shape file (https://data.cityofnewyork.us/City-Government/Borough-Boundaries/tqmj-j8zm)
borough <- readOGR("Borough", layer = "geo_export_45eb8df1-3366-4833-a27e-dfb36076578d")
community <- readOGR("Community", layer = "geo_export_45a3484a-ea1e-4a8e-ab00-89356adabe6f")
##use default marker cluster
leaflet(mvc.xy) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>%
  addPolygons(data = borough, fill = FALSE, stroke = TRUE, color = "black", weight = 1.5)
##produce specified clusters by using boroguh or community polygons
attach(mvc.xy)
boro.count <- as.data.frame(table(mvc.xy$BOROUGH))
boro.count$longitude <- tapply(mvc.xy$LONGITUDE, BOROUGH, mean)
boro.count$latitude <- tapply(mvc.xy$LATITUDE, BOROUGH, mean)
commu.count <- as.data.frame(table(mvc.xy$COMMUNITY))
commu.count$longitude <- tapply(mvc.xy$LONGITUDE, COMMUNITY, mean)
commu.count$latitude <- tapply(mvc.xy$LATITUDE, COMMUNITY, mean)
## make figure based on boroguh
leaflet(boro.count) %>% addTiles() %>% 
  addCircles(radius = ~sqrt(Freq)*15, stroke = FALSE, 
             fillOpacity = 0.4, popup = ~as.character(Freq)) %>% 
  addPolygons(data = borough, fill = FALSE, stroke = TRUE, color = "black", weight = 1.5)
## make figure based on community
leaflet(commu.count) %>% addTiles() %>% 
  addCircles(radius = ~sqrt(Freq)*15, stroke = FALSE, 
             fillOpacity = 0.4, popup = ~as.character(Freq)) %>% 
  addPolygons(data = community, fill = FALSE, stroke = TRUE, color = "black", weight = 1)