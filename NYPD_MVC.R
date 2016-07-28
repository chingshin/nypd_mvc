#Set up environment and load data into R
library(ggplot2)
setwd("~/Documents/R/NYPD_MVC/")
mvc.raw <- read.csv(file = "NYPD_Motor_Vehicle_Collisions.csv", header = T)

#Get info of the data
str(mvc.raw)
head(mvc.raw)

#Assign DATE column as a date class
mvc.raw$DATE <- as.Date(mvc.raw$DATE, format = "%m/%d/%Y")

#subset data having lantitude and longitude
mvc.xy <- mvc.raw[!is.na(mvc.raw$LATITUDE) & !is.na(mvc.raw$LONGITUDE), ]
#subset data with complete.cases() 
mvc.del_NA <- mvc.raw[complete.cases(mvc.raw), ]

# make the plot of collisions incidents grouped by boroughs
qplot(BOROUGH, data = mvc.del_NA, geom = "bar")
g2 <- qplot(DATE, data = mvc.del_NA, geom = "histogram", binwidth = 10, facets = . ~ BOROUGH)
g2 + scale_x_date(breaks = date_breaks('4 months'), labels = date_format("%Y-%m")) + 
  theme(axis.text.x = element_text(angle=90))
ggplot(mvc.del_NA, mapping = aes(BOROUGH)) + geom_bar()


mvc.killed <- mvc.del_NA[mvc.del_NA$NUMBER.OF.PERSONS.KILLED > 0, ]
qplot(NUMBER.OF.PERSONS.KILLED, data = mvc.killed, geom = "bar", facets = . ~ BOROUGH)

#use leaflet package to create a map with collision incident markers
leaflet(mvc.xy) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())