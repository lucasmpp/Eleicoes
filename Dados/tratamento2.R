

#=========================================================================================#
#                                   Labest: Dados Eleicoes
#=========================================================================================#

# Pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','geobr','readr','openxlsx','leaflet')

# Configurando local para o Brasil
Sys.setlocale("LC_ALL","Portuguese") # "pt_BR.UTF-8"

# Carregando bancos
setwd("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/7º Semestre/LabEst/")
ano <- '2022'
dados = read.csv(paste0("votacao_secao_",ano,"_BR.csv"),sep = ";", encoding = 'Latin-1')

#   Salvando banco reduzido como .Rdata
dados_tse = dados %>%
  select(c("NR_TURNO","SG_UF","CD_MUNICIPIO","NM_MUNICIPIO","NR_VOTAVEL","NM_VOTAVEL","QT_VOTOS"))
#save(dados_tse, file = "dados_tse.Rdata")

municipios = read.csv("municipios_brasileiros_tse.csv", encoding = 'UTF-8')

# Geometria 'sf' do pacote geobr
geo_mun = geobr::read_municipality(code_muni='all', year=2020, simplified=TRUE)
geo_est = geobr::read_state(code_state='all', year=2020, simplified=TRUE)
geo_reg = geobr::read_region(year=2020, simplified=TRUE)

colnames(municipios)[colnames(municipios) == 'codigo_tse'] <- 'CD_MUNICIPIO'
colnames(geo_mun)[colnames(geo_mun) == 'code_muni'] <- 'codigo_ibge'
colnames(geo_est)[colnames(geo_est) == 'abbrev_state'] <- 'SG_UF'

geo_mun = left_join(geo_mun,municipios[,c(1,5)],by='codigo_ibge')%>%
  select(-"name_muni",-"code_state",-"code_region" )

regioes <- as.data.frame(geo_est) %>%
  select(SG_UF,name_region) 

setwd("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/7º Semestre/LabEst/Git/Eleicoes/Banco de dados")
#-----------------------------------------------------------------------------------------#

# Banco de dados - Nomes presidentes: linha

banco_linha <- function(dados){
  
  dados_base <- dados %>%
    group_by(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_VOTAVEL,NR_VOTAVEL ) %>%
    summarise('VOTOS' = sum(QT_VOTOS))
  
  dados_base <- left_join(dados_base, regioes)
  
  return(dados_base)
}


# Criando cores padrões 
#   colococar o output da funcao 'banco_linha'

cores <- c('Greens','Oranges','Purples',
           'BuGn', 'BuPu', 'GnBu', 'RdPu',
           'OrRd', 'PuBu', 'PuBuGn', 'PuRd',   
           'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')


cor <- function(dados_base){
  
  rank_cor <- dados_base %>%
    filter(NR_TURNO == 1) %>%
    group_by(NM_VOTAVEL,NR_VOTAVEL)%>%
    summarise(TOTAl = sum(VOTOS))%>%
    arrange(desc(TOTAl))
  
  
  rank_cor$rank <- c(1:nrow(rank_cor))
  rank_cor$cor <- NA
  rank_cor$cor[rank_cor$NR_VOTAVEL == 13] <- "Reds"
  rank_cor$cor[rank_cor$NR_VOTAVEL %in% c(95,96)] <- "Greys"
  rank_cor$cor[rank_cor$NR_VOTAVEL != 13 & rank_cor$rank<=2] <- "Blues"
  rank_cor$cor[is.na(rank_cor$cor)] = cores[1:sum(is.na(rank_cor$cor))]
  
  cod.cor <- list()
  for(linha in rank_cor$rank){
    cod.cor[[ as.character(rank_cor$NM_VOTAVEL[linha])]] <- colorQuantile(
      palette = rank_cor$cor[linha], n=9,
      domain = seq(-1,1,by=0.1))
  }
  
  return(cod.cor)
  
}

# Banco de dados final

votos_2022 <- banco_linha(dados_tse)
mapa_2022 <- list(banco_linha(dados_tse),cor(banco_linha(dados_tse)))

save(votos_2022, file = paste0("votos_",ano,".Rdata"))
save(mapa_2022, file = paste0("mapa_",ano,".Rdata"))

save(geo_mun, file = "geo_mun.Rdata")
save(geo_est, file =  "geo_est.Rdata")
save(geo_reg, file =  "geo_reg.Rdata")

