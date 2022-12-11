library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)
library(readr)

Sys.setlocale("LC_ALL","pt_BR.UTF-8")

## Carregando bancos
dados <- read.csv("Dados//votacao_secao_2022_BR.csv", sep=";")
municipios <- read.csv('Dados//municipios_brasileiros_tse.csv')

## Modificando DF da forma que precisamos
all_mun_ms <- read_municipality(code_muni= 'all', year=2020) # para ver codigos dos municipios
colnames(municipios)[colnames(municipios) == 'codigo_tse'] <- 'CD_MUNICIPIO'
colnames(all_mun_ms)[colnames(all_mun_ms) == 'code_muni'] <- 'codigo_ibge'


## Banco de dados - Nomes presidentes: linha
df <- dados %>%
  group_by(NR_TURNO,NM_UE, SG_UF, CD_MUNICIPIO, NM_VOTAVEL ) %>%
  summarise('votos' = sum(QT_VOTOS))%>%
  group_by(NR_TURNO,NM_UE, SG_UF, CD_MUNICIPIO) %>%
  mutate('freq' = round(votos / sum(votos),2)) %>% 
  mutate('ganhou' = freq == max(freq))

## Banco de dados - Nomes presidentes: coluna

df2 <- df
df2$freq <- NULL
df2$ganhou <- NULL

df2 <- pivot_wider(df2,names_from = NM_VOTAVEL,
                   values_from = votos)


## Banco de dados - Coordenadas x Eleição

df3 <- left_join(df2, municipios )
df3 <- left_join(df3, all_mun_ms )




## --------------------------------------- ## 

saveRDS(df3,file =  "dados.csv")

#save.image("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/7º Semestre/LabEst/dados.RData")