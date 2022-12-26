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

#=========================================================================================#

# Carregar banco get(load()) ...

# Filtrar o turno
df_reg2 = sf::st_as_sf(df_reg) %>% filter(turno==2)

# Amplitude dos valores possiveis de percentual de voto
valores = range(df_est2$freq, na.rm = T)

# Funcao que retorna a paleta da cor desejada
paleta = function(cor, freq){
  
  pal = colorQuantile(cor, n=8, domain = c(valores[1],valores[2])) 
  
  return(pal(freq))
}


# Funcao que cria os labels para cada area (municipio, estado ou regiao)
#   - verificar pq as labels aparecem na ordem errada!
labels = function(dados, area){
  
  geral = paste(sep = "<br/>",
               paste(dados$candidato[dados$ranking==1],
                      paste0(dados$freq[dados$ranking==1]*100,"%", sep=" ")),
               paste(dados$candidato[dados$ranking==2],
                      paste0(dados$freq[dados$ranking==2]*100,"%", sep=" ")))
  
  if(area=="regiao"){
    label = paste(sep= "<br/>",paste0("Região ",dados$regiao ),geral) %>% 
      lapply(htmltools::HTML)
  }else if(area=='estado'){
    label = paste(sep= "<br/>",paste0(dados$estado,", ",dados$UF),geral) %>% 
      lapply(htmltools::HTML)
  }else{
    label = paste(sep= "<br/>",paste0(dados$municipio,", ",dados$UF),geral) %>% 
      lapply(htmltools::HTML)
  }
  
  return(label)
}


# Funcao que plota o mapa
mapa = function(dados, area){
  grafico = leaflet()%>%
    addPolygons(
      data = dados,
      stroke = TRUE,
      weight = 0.05,
      opacity = 1,
      color = "black",
      dashArray = "1",
      smoothFactor = 0.5,
      fillColor = case_when(
        dados$ganhou & dados$cor == 'Reds' ~ paleta('Reds', dados$freq), 
        dados$ganhou & dados$cor == 'Blues' ~ paleta('Blues', dados$freq),
        dados$ganhou & dados$cor == 'Greys' ~ paleta('Greys', dados$freq),
        dados$ganhou & dados$cor == 'Greens' ~ paleta('Greens', dados$freq), 
        dados$ganhou & dados$cor == 'Purples' ~ paleta('Purples', dados$freq),
        dados$ganhou & dados$cor == 'Oranges' ~ paleta('Oranges', dados$freq),
        dados$ganhou & dados$cor == 'PuRd' ~ paleta('PuRd', dados$freq)
      ),
      fillOpacity = 0.8,
      label = labels(dados, area),
      labelOptions =  labelOptions(
        style = list("font-weigth"= "normal",
                     padding = "3px 8px"),
        textsize = "10px",
        direction = "auto")
    ) 
  return(grafico)
}


mapa(df_reg2, 'regiao')
