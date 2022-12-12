library(tidyverse)
library(colorspace)
library(leaflet)
library(gifski)
library(gganimate)
library(sf)
library(htmltools)


df <- sf::st_as_sf(df3)

qpal = colorQuantile(palette = "Reds",domain = df3$LULA)

labels <- sprintf(
  "<strong>%s/<strong>",
  df$LULA
) %>% lapply(htmltools::HTML)

leaflet()%>%
  addPolygons(
    data = df,
    stroke = TRUE,
    weight = 0.05,
    opacity = 1,
    color = "black",
    dashArray = "1",
    smoothFactor = 0.5,
    fillColor = qpal(df$LULA),
    fillOpacity = 0.2,
    label =labels,
    labelOptions =  labelOptions(
                  style = list("font-weigth"= "normal",
                  padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
) 



