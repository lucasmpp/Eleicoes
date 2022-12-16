library(tidyverse)
library(colorspace)
library(leaflet)
library(gifski)
library(gganimate)
library(sf)
library(htmltools)

get(load(file="Dados//dados.Rdata"))


df <- sf::st_as_sf(df3) %>% 
  filter(NR_TURNO == 1) 


labels <- paste(sep = "<br/>",
                paste0(df$nome_municipio,", ", df$uf ),
                paste0("Mais votado: ", df$GANHOU),
                paste0("Votos: ", 
                  case_when(
                    df$GANHOU == "LULA" ~ df$LULA,
                    df$GANHOU == "JAIR MESSIAS BOLSONARO" ~ df$`JAIR MESSIAS BOLSONARO`,
                    df$GANHOU == "BRANCO" ~ df$NAO_VOTO))
              
) %>% lapply(htmltools::HTML)


df1 <- df # BACKUP
df <- df1

df <- df %>%
  filter(Ganhador != "VOTO NULO")%>%
  filter(!is.na(Ganhador))

df$cor <- gsub("Blues", "PuBu",df$cor)
df$cor <- gsub("Reds", "OrRd",df$cor)

df <- df[df$Ganhador == "LULA",]


leaflet()%>%
  addPolygons(
    data = df,
    stroke = TRUE,
    weight = 0.05,
    opacity = 1,
    color = "black",
    dashArray = "1",
    smoothFactor = 0.5,
    fillColor = colorQuantile(
      palette = "OrRd",
      domain = eval(parse(text=paste0("df$`",df$Ganhador,"`")))),
    fillOpacity = 0.6,
    label =labels,
    labelOptions =  labelOptions(
      style = list("font-weigth"= "normal",
                   padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) 


# case_when(
#   df$GANHOU == "LULA" ~ df$LULA,
#   df$GANHOU == "JAIR MESSIAS BOLSONARO" ~ df$`JAIR MESSIAS BOLSONARO`,
#   df$GANHOU == "BRANCO" ~ df$NAO_VOTO)
#df %>% filter(uf == "DF") %>% view()# select("LULA","JAIR MESSIAS BOLSONARO")
