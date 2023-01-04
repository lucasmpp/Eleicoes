#=========================================================================================#
#                                   Labest: Dados Eleicoes
#=========================================================================================#

## Pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','geobr','sf','readr','openxlsx','leaflet')
## Configrando local para o Brasil
Sys.setlocale("LC_ALL","Portuguese") # "pt_BR.UTF-8"

## Carregando bancos
setwd("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/7º Semestre/LabEst/")
dados = read.csv("votacao_secao_2022_BR.csv",sep = ";", encoding = 'Latin-1')
municipios = read.csv("municipios_brasileiros_tse.csv", encoding = 'UTF-8')

## Salvando banco reduzido como .Rdata
#dados = dados %>%
#  select(c("NR_TURNO","SG_UF","CD_MUNICIPIO","NM_MUNICIPIO","NR_VOTAVEL","NM_VOTAVEL","QT_VOTOS"))
#save(dados, file = "dados.Rdata")


## Geometria 'sf' do pacote geobr
geo_mun = geobr::read_municipality(code_muni='all', year=2020, simplified=TRUE)
geo_est = geobr::read_state(code_state='all', year=2020, simplified=TRUE)
geo_reg = geobr::read_region(year=2020, simplified=TRUE)

colnames(municipios)[colnames(municipios) == 'codigo_tse'] <- 'CD_MUNICIPIO'
colnames(geo_mun)[colnames(geo_mun) == 'code_muni'] <- 'codigo_ibge'
colnames(geo_est)[colnames(geo_est) == 'abbrev_state'] <- 'SG_UF'

geo_mun = left_join(geo_mun,municipios[,c(1,5)],by='codigo_ibge')%>%
  select(-"name_muni",-"code_state",-"abbrev_state",-"name_state",-"code_region" )

regioes <- as.data.frame(geo_est) %>%
  select(SG_UF,name_region) 

## Banco de dados - Nomes presidentes: linha
dados_base <- dados %>%
  group_by(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_VOTAVEL,NR_VOTAVEL ) %>%
  summarise('VOTOS' = sum(QT_VOTOS))

dados_base <- left_join(dados_base, regioes)


remove(dados,municipios,regioes)

### Criando cores padrões 

rank_cor <- dados_base %>%
  filter(NR_TURNO == 1) %>%
  group_by(NM_VOTAVEL,NR_VOTAVEL)%>%
  summarise(TOTAl = sum(VOTOS))%>%
  arrange(desc(TOTAl))

cores <- c('Greens', 'Greys', 'Oranges','Purples',
                  'BuGn', 'BuPu', 'GnBu', 'RdPu',
                  'OrRd', 'PuBu', 'PuBuGn', 'PuRd',   
                  'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')

rank_cor$rank <- c(1:nrow(rank_cor))
rank_cor$cor <- NA
rank_cor$cor[rank_cor$NR_VOTAVEL == 13] <- "Reds"
rank_cor$cor[rank_cor$NR_VOTAVEL != 13 & rank_cor$rank<=2] <- "Blues"
rank_cor$cor[is.na(rank_cor$cor)] = cores[1:sum(is.na(rank_cor$cor))]
remove(cores)

cod.cor <- list()
for(linha in rank_cor$rank){
  cod.cor[[ rank_cor$NM_VOTAVEL[linha]]] <- colorQuantile(
    palette = rank_cor$cor[linha],
    domain = eval(parse(text=paste0("dados_base$`",rank_cor$NM_VOTAVEL[linha],"`"))))
  print(rank_cor$NM_VOTAVEL[linha])
}

## Funções


tratamento <- function(dados_base, geografia, coluna){
  df <- dados_base
  df <- df %>% ungroup()%>%
    group_by(NR_TURNO, NM_VOTAVEL, eval(parse(text=coluna)))%>%
    summarise(VOTOS = sum(VOTOS)) 
  colnames(df) <- c("NR_TURNO","NM_VOTAVEL",coluna,"VOTOS")
  df <- na.omit(df)
  df <- pivot_wider(df, names_from = NM_VOTAVEL,values_from = VOTOS)
  df[is.na(df)] <- 0
  
  df$TOTAL <- rowSums( df[,3:ncol(df)] )
  
  temp <- df[,-c(1,2,ncol(df))] %>%
    mutate(Ganhador = names(.)[max.col(., 'first')])
  df[,-c(1,2,ncol(df))] <- df[,-c(1,2,ncol(df))]/df$TOTAL
  df$Ganhador = temp$Ganhador
  df <- left_join(df, geografia)
  df <- na.omit(df)
  return(df)
}

Paleta.cores <- function(df,vetor.ganhador){
  cor_funcoes <- paste0("cod.cor$`",
                        unique(vetor.ganhador),
                        "`(",df,"$`",unique(vetor.ganhador),"`)") # Consulta para funções
  Lcores <- lapply(cor_funcoes, function(x) eval(parse(text=x))) # Gerando as paletas de cores para cada candidato
  Lcores <- as.data.frame(do.call(cbind, Lcores)) # Guardando em um dataframe
  names(Lcores) <- unique(vetor.ganhador) # Identificando cada candidato
  Lcores$venceu <- vetor.ganhador #Vendo quem venceu em cada local
  Lcores$cor <- Lcores[cbind(1:nrow(Lcores),match(Lcores$venceu, names(Lcores)))] # Atribuindo a cor a linha correspondente
  return(Lcores$cor) # Adicionando ao banco original
}


mapa <- function(dados, turno){
  df <- sf::st_as_sf(dados) %>% filter(NR_TURNO == turno)
  
  leaflet() %>%
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
      #label =labels,
      labelOptions =  labelOptions(
        style = list("font-weigth"= "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    )
}

###


# Banco de dados

dados_base$NR_VOTAVEL <- NULL

dados_mun <- tratamento(dados_base,geo_mun,'CD_MUNICIPIO')
dados_est <- tratamento(dados_base,geo_est,'SG_UF')
dados_reg <- tratamento(dados_base,geo_reg,'name_region')

remove(geo_mun,geo_est,geo_reg)

# Definindo a paleta de cores

dados_mun$cor <- Paleta.cores('dados_mun',dados_mun$Ganhador)
dados_est$cor <- Paleta.cores('dados_est',dados_est$Ganhador)
dados_reg$cor <- Paleta.cores('dados_reg',dados_reg$Ganhador)

mapa(dados_mun,1)
mapa(dados_mun,2)
mapa(dados_est,1)
mapa(dados_est,2)
mapa(dados_reg,1)
mapa(dados_reg,2)

