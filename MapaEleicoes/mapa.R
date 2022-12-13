library(tidyverse)
library(colorspace)
library(leaflet)
library(gifski)
library(gganimate)
library(sf)
library(htmltools)

get(load(file="Dados//dados.Rdata"))
df <- sf::st_as_sf(df3) %>% 
        mutate(`VOTO NULO` = coalesce(`VOTO NULO`,0),
               `VOTO BRANCO` = coalesce(`VOTO BRANCO`,0)  ) %>% 
        mutate(NAO_VOTO = `VOTO NULO` + `VOTO BRANCO` ) %>% 
        select("LULA","JAIR MESSIAS BOLSONARO","NAO_VOTO",
                "uf","nome_municipio","name_state","name_region","geom") %>% 
        mutate(GANHOU = case_when(
          LULA > `JAIR MESSIAS BOLSONARO` & LULA > NAO_VOTO ~ "LULA",
         `JAIR MESSIAS BOLSONARO` >  LULA & `JAIR MESSIAS BOLSONARO` > NAO_VOTO ~ "JAIR MESSIAS BOLSONARO",
          NAO_VOTO > `JAIR MESSIAS BOLSONARO` & NAO_VOTO > LULA ~ "BRANCO",
        ) )



paletaLula = colorQuantile(palette = "OrRd",domain = df$LULA)
paletaBolso = colorQuantile(palette = "PuBu",domain = df$`JAIR MESSIAS BOLSONARO`)
paletaBranco = colorQuantile(palette = "White",domain = df$NAO_VOTO)

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
    fillColor = case_when(
      df$GANHOU == "LULA" ~ paletaLula(df$LULA),
      df$GANHOU == "JAIR MESSIAS BOLSONARO" ~ paletaBolso(df$`JAIR MESSIAS BOLSONARO`),
      df$GANHOU == "BRANCO" ~ paletaBranco(df$NAO_VOTO)
    )   ,
    fillOpacity = 0.2,
    label =labels,
    labelOptions =  labelOptions(
                  style = list("font-weigth"= "normal",
                  padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")
) 


# (case_when(
#   df$GANHOU == "LULA" ~ df$LULA,
#   df$GANHOU == "JAIR MESSIAS BOLSONARO" ~df$`JAIR MESSIAS BOLSONARO`,
#   df$GANHOU == "BRANCO" ~ df$NAO_VOTO))
