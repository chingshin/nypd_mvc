#Set up environment and load data into R
library(ggplot2)
setwd("~/Documents/R/NYPD_MVC/")
mvc.raw <- read.csv(file = "NYPD_Motor_Vehicle_Collisions.csv", header = T)

#Get info of the data
str(mvc.raw)
head(mvc.raw)

#Clean up data
mvc.raw_NA$DATE <- as.Date.character(df.raw_NA$DATE, format = "%m/%d/%Y")
mvc.raw_NA$TIME <- as.character(mvc.raw_NA$TIME)


mvc.del_NA <- mvc.raw[complete.cases(mvc.raw), ]



mvc.tmp <- mvc.del_NA[1:200, ]
ggplot(mvc.del_NA, mapping = aes(DATE)) + geom_bar()