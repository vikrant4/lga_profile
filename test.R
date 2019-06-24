blank_layer <- list(
  title = "",
  showgrid = F,
  showticklabels = F,
  zeroline = F)

merge.lga.profiles %>%
  group_by(group) %>%
  plot_ly(
    x = ~long,
    y = ~lat,
    color = ~Registered.mental.health.clients.per.1.000.population,
    colors = "PRGn",
    hoverinfo = "text",
    text = paste('<b>Area</b>: ', merge.lga.profiles$lga_name)
    ) %>%
  add_polygons(
    line = list(color = 'black', width = 0.5), showlegend = FALSE) %>%
  layout(geo = g)
