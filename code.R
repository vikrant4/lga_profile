library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(xlsx)
library(plotly)

vic.lga.shp <- readShapeSpatial("vmlite_lga_cm/vmlite_lga_cm.shp")
lga.shp.f <- tidy(vic.lga.shp, region = "lga_name")
lga.shp.f$lga_name <- tolower(lga.shp.f$id)

lga.data <- read.xlsx("Data Sheet.xlsx", sheetName = "LGAs")
lga.data$LGA.Name <- tolower(gsub(' \\(\\w*\\)', '', lga.data$LGA.Name))

merge.lga.profiles <- merge(lga.shp.f, lga.data, by.x="lga_name", by.y="LGA.Name", all.x=TRUE)
merge.lga.profiles <- merge.lga.profiles[order(merge.lga.profiles$order),]

p1 <- ggplot(data=merge.lga.profiles,
             aes(x=long, y=lat, group=group, fill=People.aged.over.75.years.who.live.alone))
p1 + geom_polygon(color="black", size=0.25) + coord_map()
