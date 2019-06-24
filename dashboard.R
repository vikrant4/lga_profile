## app.R ##
require(maptools)
require(leaflet)

library(leaflet)
library(maptools)
library(dplyr)
library(ggplot2)
library(shiny)
library(rgeos)

# gpclibPermit()

vic.lga.shp <- readShapeSpatial("vmlite_lga_cm/vmlite_lga_cm.shp")
# lga.shp.f <- tidy(vic.lga.shp, region = "lga_name")
# lga.shp.f$lga_name <- tolower(lga.shp.f$id)

# lga.data <- read.xlsx("Data Sheet.xlsx", sheetName = "LGAs")
lga.data <- read.csv("lga_data.csv")

lga.data <- lga.data %>% select(
  Median.house.price,
  Median.household.income,
  Median.weekly.rent.for.3.bedroom.home,
  New.settler.arrivals.per.100.000.population,
  New.dwellings.approved.per.1.000.population,
  Rental.housing.that.is.affordable,
  Homeless.people..estimated..per.1.000.population,
  Social.housing.dwellings,
  Unemployment.rate,
  LGA.Name
)

# Removing last row for summed values
lga.data <- lga.data[-c(80),]

for(i in 1:9){
  lga.data[[i]] <- gsub('\\$*\\%*\\,*', '', lga.data[[i]])
}
lga.data[1:9] <- lapply(lga.data[1:9], as.numeric)

# Matching the column name for merge
lga.data$lga_name <-
  toupper(gsub(' \\(\\w*\\)', '', lga.data$LGA.Name))

# Merging with spatial data
leaflet.data <- sp::merge(vic.lga.shp,
                          lga.data,
                          by = "lga_name",
                          duplicateGeoms = TRUE)
writeup = list(
  "Median.house.price" = "The house prices throughout Australia are said to be declining staedily.
This has been making people unsure weather to invest in property or not.
  Here we look at the housing market of Victoria to understand differnet aspects of the market.
  Meadian price to but a house in Victoria is $393,000. The map above shows median house prices for each of the local government area(LGA).
  It can be seen that most of the fall close to this number. Although if we see the most expensive LGAs in the bar plot on right,
  the most expensive area is Boroondara with $1.5 million. That is a huge shift from the median value of the entire state.
  Zoom into the map and you can see that expensive areas are in and around the city. Try to see the median value for where you live and see how it stands.
  After this, click on median household income to see how are the household earnings for these places.",
  "Median.household.income" = "Looking at median household earnings, it is interesting to see that the map is almost complementary to the map plotted using Median household prices.
Boroondara is again at top of the list at $1,893 while median for the entire state is $1,216. The map gets more red as we move away from the city but has a better dispersion. It is interesting to see that 
although Melbourne has pretty high house price, the modeian household income is not that high. One explanation for such areas can that there are
more people who are renting than those who are living in their own home. Melton is also an interesting area, with median house price lower than for
the entire state but household income is greater than median for Victoria. Again try to see how your area comapres",
"Median.weekly.rent.for.3.bedroom.home" = "Rent prices show a slightly different story. For rent prices, areas which are close to city and have beaches make it to the top of the list.
Weekly rent of a 3 bedroom apartment in Port Phillp is more than twice costlier than the median for the entire state. Try to see the median rent for your area.",
"New.settler.arrivals.per.100.000.population" = "Demand drives prices. For house prices to be steadily increasing it is important that people keep buying new/old properties. One of the things which
can help this best is arrival of new settlers in the area. New settlers need some place to stay so they will either rent or buy a property. This
allows more businesses to open to service their needs and helps drive the local economy, which in turn can positvely impact house prices. As expected
Melbourne has the highest number of new settlers relative per 1,000 people already living there. The area at second place has a considerably lower 
number of new settlers but has a surprising place, Greater Dandenong. Although the median household income is not that high, new settlers are stil 
coming in.",
"New.dwellings.approved.per.1.000.population" = "Future project plans can also be seen as the degree of confidence of the market in that area. Here we look at the number of new dwelling approved 
per 1,000 population. This is the plot which does not paint a good picture of the housing market. Apart from Melbourne, no other area has any 
considerable number of new dwellings. Large portion of the map is red and the areas surrounding Melbourne have also failed to perform in this 
parameter.",
"Rental.housing.that.is.affordable" = "Affordable rental housings are scarce in the city area which means that place with highest number of new settlers is not going to come cheaply.
But there are more rental housings available as we move further away from the city. Areas nearest to the city with good amount of afforadble rental
housings are Murrindindi, Mitchell and Moorabool.",
"Homeless.people..estimated..per.1.000.population" = "The worrisome state of homelessness in Victoria can be seen in the above map. Areas close to city have the highest count of homeless people relative to their population. Port Phillip is at the worst position with 15.3. Thankfully regional areas are doing better. Greater Dandenong also has a worrying state. In outer areas, Greater Shepparton and Swan Hill have the worst performance."

)

# UI of the dashboard
ui <- fluidPage(
  titlePanel("Victoria House prices"),
  sidebarLayout(
    position = "right",
    sidebarPanel(
      selectInput("var", "Variable", colnames(lga.data[1:7]), selected = colnames(lga.data[1])),
      plotOutput('plot2')
    ),
    mainPanel(
      leafletOutput("plot1"),
      p("Source: 2015 Local Government Area Profile"),
      a('https://www.data.vic.gov.au/data/dataset/2015-local-government-area-profiles'),
      br(),
      textOutput('writeup'),
      p("Latest news:", a('https://www.abc.net.au/news/2018-10-23/house-prices-falling-as-interest-rates-wage-growth-move/10418278')),
      hr(),
      p('Made by: Vikrant Yadav')
    )
  )
  
)


# Server code for dashboard
server <- function(input, output) {
  
  # Setting up map
  plot <- leaflet(leaflet.data) %>% 
    setView(
      lng = 147.5,
      lat = -36.5,
      zoom = 6
  )
  
  output$plot1 <- renderLeaflet({
    if(input$var %in% c('Median.house.price', 'Median.weekly.rent.for.3.bedroom.home', 'Homeless.people..estimated..per.1.000.population')){
      pal <- colorNumeric(
        'YlOrRd',
        domain = lga.data[[input$var]]
      )
    }
    else {
      pal <- colorNumeric(
        'RdYlGn',
        domain = lga.data[[input$var]]
      )
    }
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g",
      leaflet.data$lga_name,
      leaflet.data[[input$var]]
    ) %>% lapply(htmltools::HTML)
    
    plot %>% addPolygons(
      fillColor = ~ pal(get(input$var)),
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
        values = ~get(input$var),
        opacity = 0.7,
        title = input$var,
        position = "bottomright"
      )
  })
  
  output$plot2 <- renderPlot({
    plot2.data <- lga.data
    plot2.data$lga_name <- plot2.data$lga_name %>% factor(levels = plot2.data$lga_name[order(plot2.data[[input$var]])])
    plot2.data <- plot2.data %>% select(lga_name, input$var) %>% arrange(desc(get(input$var))) %>% top_n(5)
    ggplot(plot2.data, aes(y=get(input$var), x=lga_name)) +
      geom_bar(stat="identity",fill = "dodgerblue3") +
      expand_limits(y=max(lga.data[[input$var]])+sd(lga.data[[input$var]])) +
      geom_text(aes(label=get(input$var)), hjust=-0.1, size=3) +
      labs(x="Value", y="Local government areas", title ="Top 5 areas") + coord_flip()
  })
  
  output$writeup <- renderText({
    writeup[[input$var]]
  })
}

shinyApp(ui, server)
