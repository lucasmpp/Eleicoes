library(tidyverse)
library(colorspace)
library(leaflet)
library(gifski)
library(gganimate)
library(sf)
library(htmltools)

df <- get(load(file="Dados//dados.Rdata"))
colors <- get(load(file="Dados//colors.Rdata"))


df <- sf::st_as_sf(df3) 
df1 <- df # BACKUP
df <- df1
df <- df %>%
  filter(Ganhador != "VOTO NULO")%>%
  filter(!is.na(Ganhador))%>% 
  filter(NR_TURNO == 1)

start_time <- Sys.time()

labels <- paste(sep = "<br/>",
                paste0(df$nome_municipio,", ", df$uf ),
                paste0("Mais votado: ", df$Ganhador),
                paste0("Votos: ", 
                       case_when(
                         df$Ganhador == "LULA" ~ df$LULA,
                         df$Ganhador == "JAIR MESSIAS BOLSONARO" ~ df$`JAIR MESSIAS BOLSONARO`,
                         df$Ganhador == "BRANCO" ~ df$NAO_VOTO))
                
) %>% lapply(htmltools::HTML)

#####  
# Definindo a paleta de cores

cor_funcoes <- paste0("colors$`",
                     unique(df$Ganhador),
                     "`(df$`",unique(df$Ganhador),"`)") # Consulta para funções

Lcores <- lapply(cor_funcoes, function(x) eval(parse(text=x))) # Gerando as paletas de cores para cada candidato
Lcores <- as.data.frame(do.call(cbind, Lcores)) # Guardando em um dataframe
names(Lcores) <- unique(df$Ganhador) # Identificando cada candidato
Lcores$venceu <- df$Ganhador #Vendo quem venceu em cada local
Lcores$cor <- Lcores[cbind(1:nrow(Lcores),match(Lcores$venceu, names(Lcores)))] # Atribuindo a cor a linha correspondente
df$cor <- Lcores$cor # Adicionando ao banco original

##### 


leaflet()%>%
  addPolygons(
    data = df,
    stroke = TRUE,
    weight = 0.05,
    opacity = 1,
    color = "black",
    dashArray = "1",
    smoothFactor = 0.5,
    fillColor = ~ cor,
    fillOpacity = 1,
    label =labels,
    labelOptions =  labelOptions(
      style = list("font-weigth"= "normal",
                   padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) 
end_time <- Sys.time()
end_time - start_time

# ------------------------------------------------------------------------------------------#



paletaLula = colorQuantile(palette = "Reds",domain = df$LULA)
paletaBolso = colorQuantile(palette = "Blues",domain = df$`JAIR MESSIAS BOLSONARO`)
paletaBranco = colorQuantile(palette = "White",domain = df$NAO_VOTO)

labels <- paste(sep = "<br/>",
                paste0(df$nome_municipio,", ", df$uf ),
                paste0("Mais votado: ", df$Ganhador),
                paste0("Votos: ", 
                       case_when(
                         df$Ganhador == "LULA" ~ df$LULA,
                         df$Ganhador == "JAIR MESSIAS BOLSONARO" ~ df$`JAIR MESSIAS BOLSONARO`,
                         df$Ganhador == "BRANCO" ~ df$NAO_VOTO))
                
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
      df$Ganhador == "LULA" ~ paletaLula(df$LULA),
      df$Ganhador == "JAIR MESSIAS BOLSONARO" ~ paletaBolso(df$`JAIR MESSIAS BOLSONARO`),
      df$Ganhador == "BRANCO" ~ paletaBranco(df$NAO_VOTO)
    )   ,
    fillOpacity = 1,
    label =labels,
    labelOptions =  labelOptions(
      style = list("font-weigth"= "normal",
                   padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) 
# case_when(
#   df$Ganhador == "LULA" ~ df$LULA,
#   df$Ganhador == "JAIR MESSIAS BOLSONARO" ~ df$`JAIR MESSIAS BOLSONARO`,
#   df$Ganhador == "BRANCO" ~ df$NAO_VOTO)
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
        dados$Ganhador & dados$cor == 'Reds' ~ paleta('Reds', dados$freq), 
        dados$Ganhador & dados$cor == 'Blues' ~ paleta('Blues', dados$freq),
        dados$Ganhador & dados$cor == 'Greys' ~ paleta('Greys', dados$freq),
        dados$Ganhador & dados$cor == 'Greens' ~ paleta('Greens', dados$freq), 
        dados$Ganhador & dados$cor == 'Purples' ~ paleta('Purples', dados$freq),
        dados$Ganhador & dados$cor == 'Oranges' ~ paleta('Oranges', dados$freq),
        dados$Ganhador & dados$cor == 'PuRd' ~ paleta('PuRd', dados$freq)
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
