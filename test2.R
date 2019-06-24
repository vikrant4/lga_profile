library(leaflet)
library(maptools)
library(dplyr)
library(xlsx)

vic.lga.shp <- readShapeSpatial("vmlite_lga_cm/vmlite_lga_cm.shp")
# lga.shp.f <- tidy(vic.lga.shp, region = "lga_name")
# lga.shp.f$lga_name <- tolower(lga.shp.f$id)

# lga.data <- read.xlsx("Data Sheet.xlsx", sheetName = "LGAs")
lga.data <- read.csv("lga_data.csv")
lga.data$lga_name <- toupper(gsub(' \\(\\w*\\)', '', lga.data$LGA.Name))

leaflet.data <-sp::merge(
  vic.lga.shp, lga.data, 
  by="lga_name", duplicateGeoms = TRUE
)

leaflet.data$Registered.mental.health.clients.per.1.000.population <- as.numeric(leaflet.data$Registered.mental.health.clients.per.1.000.population)

plot <- leaflet(leaflet.data) %>%
  setView(lng = 145.5, lat = -36.5, zoom = 6)


pal <- colorNumeric(
  'YlOrRd',
  domain = leaflet.data$Registered.mental.health.clients.per.1.000.population
)

labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  leaflet.data$lga_name, 
  leaflet.data$Registered.mental.health.clients.per.1.000.population
) %>% lapply(htmltools::HTML)

plot %>% addPolygons(
  fillColor = ~pal(Registered.mental.health.clients.per.1.000.population),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE
  ),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"
  )
) %>% 
addLegend(
  pal = pal,
  values = ~Registered.mental.health.clients.per.1.000.population, 
  opacity = 0.7, title = "Notifications/1,000 people",
  position = "bottomright"
) %>% 
addControl('Hello', position = "topright")


