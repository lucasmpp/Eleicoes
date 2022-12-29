municipios <- read.csv("C:/Users/raalon/OneDrive - SAS/Documents/LABEST/municipios_brasileiros_tse.csv")

## Banco de dados - Nomes presidentes: linha

#NAO VAI SER NECESSARIO
dados_tse$NM_VOTAVEL <- gsub("LUIZ IN<c1>CIO LULA DA SILVA", "LULA",dados_tse$NM_VOTAVEL)
dados_tse$NM_VOTAVEL <- gsub("LEONARDO P<c9>RICLES VIEIRA ROQUE", "LEONARDO PERICLES",dados_tse$NM_VOTAVEL)

df2 <- dados_tse %>% mutate(REGIAO = case_when(
  SG_UF %in% c('RO','AC','AM','RR','PA','AP','TO') ~ 'Norte',
  SG_UF %in% c('MA','PI','CE','RN','PB','PE','AL','SE','BA') ~ 'Nordeste',
  SG_UF %in% c('MG','ES','RJ','SP') ~ 'Sudeste',
  SG_UF %in% c('PR','SC','RS') ~ 'Sul',
  SG_UF %in% c('MS','MT','GO','DF') ~ 'Centro Oeste',
  SG_UF == 'ZZ' ~ 'Exterior'
)) %>%
  filter(SG_UF != 'ZZ') %>%
  group_by(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_VOTAVEL, NR_VOTAVEL,REGIAO) %>%
  summarise('votos' = sum(QT_VOTOS))%>%
  group_by(NR_TURNO,CD_MUNICIPIO) %>%
  mutate('freq' = round(votos / sum(votos),4)*100) %>% 
  mutate('ganhou' = freq == max(freq))
  
  # POR municipios
  
  df_mun <- df2 %>%
  select(NM_VOTAVEL, CD_MUNICIPIO,NR_TURNO,freq,NR_VOTAVEL,ganhou)%>%
  group_by(NM_VOTAVEL, CD_MUNICIPIO)%>%
mutate(diff=freq -lag(
  freq,default=first(freq))) %>%
  filter(NR_TURNO==2)

  # POR UF

df_uf <- df2 %>%
  group_by(NR_TURNO, SG_UF, NM_VOTAVEL, NR_VOTAVEL) %>%
  summarise('voto' = sum(votos))%>%
  group_by(NR_TURNO,SG_UF) %>%
  mutate('freq_uf' = round(voto / sum(voto),4)*100)
  
df_uf2 <- df_uf %>%
  select(NM_VOTAVEL, SG_UF,NR_TURNO,freq_uf,NR_VOTAVEL)%>%
  group_by(NM_VOTAVEL,SG_UF)%>%
  mutate(diff_uf=freq_uf -lag(
    freq_uf,default=first(freq_uf))) %>%
  group_by(SG_UF)%>%
  mutate('ganhou_uf' = diff_uf == max(diff_uf)) %>%
  filter(NR_TURNO==2)

# POR REGIAO

df_reg <- df2 %>%
  group_by(NR_TURNO, REGIAO, NM_VOTAVEL, NR_VOTAVEL) %>%
  summarise('voto' = sum(votos))%>%
  group_by(NR_TURNO,REGIAO) %>%
  mutate('freq_reg' = round(voto / sum(voto),4)*100)

df_reg2 <- df_reg %>%
  select(NM_VOTAVEL, REGIAO,NR_TURNO,freq_reg,NR_VOTAVEL)%>%
  group_by(NM_VOTAVEL,REGIAO)%>%
  mutate(diff_reg=freq_reg -lag(
    freq_reg,default=first(freq_reg))) %>%
  group_by(REGIAO)%>%
  mutate('ganhou_reg' = diff_reg == max(diff_reg)) %>%
  filter(NR_TURNO==2)

#JUNÇÃO

dados_municipio = left_join(df_mun, geo_mun, by='CD_MUNICIPIO') %>%
  select(c("CD_MUNICIPIO","NR_VOTAVEL","NM_VOTAVEL",
           "diff","ganhou","name_state","geom"))

# REALIZAR O TRANSPOSE

dados_uf = left_join(df_uf2, geo_est, by='SG_UF') %>%
  select(c("SG_UF","NR_VOTAVEL","NM_VOTAVEL",
           "diff_uf","ganhou_uf","name_state","geom"))

# MUDAR GEOM PARA GEOM_UF PARA JUNTAR TUDO EM UMA TABELA

dados_regiao = left_join(df_reg2, geo_reg, by='REGIAO') %>%
  select(c("REGIAO","NR_VOTAVEL","NM_VOTAVEL",
           "diff_reg","ganhou_reg","geom"))
