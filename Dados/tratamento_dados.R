library(tidyverse)
library(geobr)
library(ggplot2)
library(sf)
library(readr)
library(openxlsx)

Sys.setlocale("LC_ALL","pt_BR.UTF-8")

## Carregando bancos
dados <- read.csv("UNB/SEMESTRE - 8/LABEST/votacao_secao_2022_BR.csv",sep = ";")
municipios <- read.csv("UNB/SEMESTRE - 8/LABEST/municipios_brasileiros_tse.csv")
teste<- read.csv("dados.csv")
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

df$NM_VOTAVEL <- gsub("LUIZ IN<c1>CIO LULA DA SILVA", "LULA",df$NM_VOTAVEL)
df$NM_VOTAVEL <- gsub("LEONARDO P<c9>RICLES VIEIRA ROQUE", "LEONARDO PERICLES",df$NM_VOTAVEL)

## Banco de dados - Nomes presidentes: coluna

df2 <- df
df2$freq <- NULL
df2$ganhou <- NULL

df2 <- pivot_wider(df2,names_from = NM_VOTAVEL,
                   values_from = votos)

df2[is.na(df2)] <- 0
df2$TOTAL <- rowSums( df2[,5:ncol(df2)] )
df2 <- df2 %>%
  mutate(NAO_VOTO = `VOTO NULO` + `VOTO BRANCO` )%>%
  select(-`VOTO BRANCO`, - `VOTO NULO`)

## Banco de dados - Coordenadas x Eleição

df3 <- left_join(df2, municipios )
df3 <- left_join(df3, all_mun_ms )
## --------------------------------------- ## 

df1 <- df
vencedores <- df %>% filter(ganhou == TRUE) %>% select("NR_TURNO","NM_UE",  
                                                       "SG_UF",   "CD_MUNICIPIO",
                                                       "NM_VOTAVEL" )
colnames(vencedores )[colnames(vencedores ) == 'NM_VOTAVEL'] <- 'Ganhador'

cor <- dados %>% 
  group_by(NR_TURNO,NM_VOTAVEL,NR_VOTAVEL) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))

cor1 <- cor %>% 
  filter(NR_TURNO == 1)

cor1$index <- c(1:nrow(cor1)) 
cor1 <- cor1 %>% filter(index<=5 | NM_VOTAVEL %in% c('NAO_VOTO') )
cor1$cor <- NA
demais_cores <- c("Bugn","BuPu","PuRd","#FFFF00","#964b00")
cor1$cor[cor1$NR_VOTAVEL == 13] <- "Reds"
cor1$cor[cor1$NR_VOTAVEL != 13 & cor1$index<=2] <- "Blues"
cor1$cor[is.na(cor1$cor)] = demais_cores[1:sum(is.na(cor1$cor))]


colnames(cor1)[colnames(cor1) == 'NM_VOTAVEL'] <- 'Ganhador'
cor1 <-cor1%>% ungroup() %>% select(Ganhador,cor)

vencedor <- left_join(vencedores,cor1)


df4 <- left_join(df3,vencedor)

save(df3, file = "dados.Rdata")
dados2<- get(load(file = "dados.Rdata"))
#save.image("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/7º Semestre/LabEst/dados.RData")