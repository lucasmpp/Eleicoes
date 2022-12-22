#=========================================================================================#
#                                   Labest: Dados Eleicoes
#=========================================================================================#

## Pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse','geobr','sf','readr','openxlsx')

## Configrando local para o Brasil
Sys.setlocale("LC_ALL","Portuguese") # "pt_BR.UTF-8"

## Carregando bancos
setwd("D:/Unb/Disciplinas/EST/Labest/teste/Dados/")
dados = read.csv("votacao_secao_2022_BR.csv",sep = ";", encoding = 'Latin-1')
municipios = read.csv("municipios_brasileiros_tse.csv", encoding = 'UTF-8')

## Salvando banco reduzido como .Rdata
dados_tse = dados %>%
  select(c("NR_TURNO","SG_UF","CD_MUNICIPIO","NM_MUNICIPIO","NR_VOTAVEL","NM_VOTAVEL","QT_VOTOS"))
save(dados_tse, file = "dados_tse.Rdata")

## Geometria 'sf' do pacote geobr
geo_mun = geobr::read_municipality(code_muni='all', year=2020, simplified=TRUE)
geo_est = geobr::read_state(code_state='all', year=2020, simplified=TRUE)
geo_reg = geobr::read_region(year=2020, simplified=TRUE)

colnames(municipios)[colnames(municipios) == 'codigo_tse'] <- 'CD_MUNICIPIO'
colnames(geo_mun)[colnames(geo_mun) == 'code_muni'] <- 'codigo_ibge'
colnames(geo_est)[colnames(geo_est) == 'abbrev_state'] <- 'SG_UF'
colnames(geo_reg)[colnames(geo_reg) == 'name_region'] <- 'REGIAO'

geo_mun = left_join(geo_mun,municipios[,c(1,5)],by='codigo_ibge')



## Bancos de dados

# Municipio
df_mun <- dados_tse %>%
  group_by(NR_TURNO, SG_UF, CD_MUNICIPIO, NR_VOTAVEL, NM_VOTAVEL) %>%
  summarise('VOTOS' = sum(QT_VOTOS))%>%
  group_by(NR_TURNO,SG_UF, CD_MUNICIPIO) %>%
  mutate('FREQ' = round(VOTOS / sum(VOTOS),4),
         'GANHOU' = FREQ == max(FREQ))  

df_mun = left_join(df_mun, geo_mun, by='CD_MUNICIPIO') %>%
            select(c("NR_TURNO","SG_UF","CD_MUNICIPIO","NR_VOTAVEL","NM_VOTAVEL","VOTOS","FREQ",
                     "GANHOU","name_muni","geom"))

colnames(df_mun) = c("turno","UF","codigo_muni","n_candidato","candidato",
                     "votos","freq","ganhou","municipio","geom")


df_mun = df_mun %>% arrange(turno, UF, municipio, desc(votos)) %>%
  group_by(turno,UF,municipio) %>% mutate(ranking=rank(-votos)) %>%
  select(c(1,2,4,5:7,9,8,11,10))


# Estado
df_est <- dados_tse %>%
  group_by(NR_TURNO, SG_UF, NR_VOTAVEL, NM_VOTAVEL) %>%
  summarise('VOTOS' = sum(QT_VOTOS))%>%
  group_by(NR_TURNO,SG_UF) %>%
  mutate('FREQ' = round(VOTOS / sum(VOTOS),4),
         'GANHOU' = FREQ == max(FREQ))  

df_est = left_join(df_est, geo_est, by='SG_UF') %>%
  select(c("NR_TURNO","SG_UF","NR_VOTAVEL","NM_VOTAVEL",
           "VOTOS","FREQ","GANHOU","name_state","geom"))

colnames(df_est) = c("turno","UF","n_candidato","candidato","votos","freq","ganhou","estado","geom")

df_est = df_est %>% arrange(turno, estado, votos) %>%
  group_by(turno,estado) %>% mutate(ranking=rank(-votos)) %>%
  select(1:8,10,9)

# Regiao
df_reg <- dados_tse %>%
  mutate(REGIAO = case_when(
    SG_UF %in% c('RO','AC','AM','RR','PA','AP','TO') ~ 'Norte',
    SG_UF %in% c('MA','PI','CE','RN','PB','PE','AL','SE','BA') ~ 'Nordeste',
    SG_UF %in% c('MG','ES','RJ','SP') ~ 'Sudeste',
    SG_UF %in% c('PR','SC','RS') ~ 'Sul',
    SG_UF %in% c('MS','MT','GO','DF') ~ 'Centro Oeste',
    SG_UF == 'ZZ' ~ 'Exterior'
  )) %>%
  filter(SG_UF != 'ZZ') %>%
  group_by(NR_TURNO, NR_VOTAVEL, NM_VOTAVEL, REGIAO) %>%
  summarise('VOTOS' = sum(QT_VOTOS))%>%
  group_by(NR_TURNO,REGIAO) %>%
  mutate('FREQ' = round(VOTOS / sum(VOTOS),4),
         'GANHOU' = FREQ == max(FREQ)) 
  
df_reg = left_join(df_reg, geo_reg, by='REGIAO') %>%
  select(-code_region)

colnames(df_reg) = c("turno","n_candidato","candidato","regiao","votos","freq","ganhou","geom")

df_reg = df_reg %>% arrange(turno, regiao, votos) %>%
  group_by(turno,regiao) %>% mutate(ranking=rank(-votos)) %>%
  select(1:7,9,8)


# Exterior

df_ext = dados_tse %>%
  filter(SG_UF == 'ZZ')

#-----------------------------------------------------------------------------------------#
## Cores

# Funcao que faz o ranking dos candidatos com base nos votos do 1 turno
#   - nao considera: candidatos do PT, votos nulos, votos brancos
#   - para otimizacao, utilizar o menor banco (regiao)

ranking = function(dados){
  
  aux = dados %>% filter(turno==1 & !(n_candidato %in% c(13,95,96))) %>%
    group_by(n_candidato, candidato) %>%
    summarise(total_votos = sum(votos)) %>%
    arrange(desc(total_votos)) %>% pull(n_candidato)
  
  return(aux)
  
}

vencedores = ranking(df_reg)

# Funcao que gera as cores de cada candidato, a partir do ranking
#   - se for candidato do PT eh sempre vermelho
#   - o candidato mais forte (que nao for do PT) eh sempre azul
#   - cores disponiveis ate 6 candidatos

cor = function(dados){
  
  cores = c("Blues","Greens","Purples","Oranges","PuRd")
  n = ifelse(length(cores)<= length(vencedores), length(cores), length(vencedores))
  
  dados$cor = NA
  dados$cor[dados$n_candidato == 13] = "Reds"           # PT
  dados$cor[dados$n_candidato %in% c(95,96)] = "Greys"  # votos nulos ou brancos
  for(i in 1:n){                                        # demais candidatos
    dados$cor[dados$n_candidato==vencedores[i]] = cores[i]
  }
  
  return(dados)
}

df_mun = cor(df_mun)
df_est = cor(df_reg)
df_reg = cor(df_reg)

#-----------------------------------------------------------------------------------------#
## Salvando

writexl::write_xlsx(df_mun, 'df_mun.xlsx')
save(df_mun, file = "df_mun.Rdata")

writexl::write_xlsx(df_reg, 'df_reg.xlsx')
save(df_reg, file = "df_reg.Rdata")

writexl::write_xlsx(df_est, 'df_est.xlsx')
save(df_est, file = "df_est.Rdata")
