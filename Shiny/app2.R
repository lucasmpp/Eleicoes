#=========================================================================================#
#                                           Shiny
#=========================================================================================#

# instalar pacotes:
# install.packages("nome_pacote", repos = "http://cran.us.r-project.org")

# cor azul: #003366
# cor verde: #00843D
# cor off white fundo: #F5F5F5

if (!require("pacman")) install.packages("pacman")
pacman::p_load('shiny','shinythemes','shinyWidgets','tidyverse','geobr',
               'sf','readr','openxlsx','leaflet',"leaflet.extras")

for(ano in seq(2010,2022,by=4)){
  get(load(paste0("./Banco de dados/votos_",ano,".Rdata")))
  get(load(paste0("./Banco de dados/mapa_",ano,".Rdata")))
}

get(load("./Banco de dados/geo_mun.Rdata"))
get(load("./Banco de dados/geo_est.Rdata"))
get(load("./Banco de dados/geo_reg.Rdata"))


# Funções

label.function <- function(df,coluna,diff=FALSE){
  if(diff){
    df$VOTOS <- paste0(ifelse(df$VOTOS>0,"+",""),df$VOTOS*100)
  }else{
    df <- df%>%
      group_by_at(c(1,3)) %>%
      mutate(`VOTOS` = `VOTOS`/sum(`VOTOS`))
    df$VOTOS <- round(df$VOTOS*100,2)
  }
  
  df.1 <- df %>%
    group_by_at(match(c('NR_TURNO',coluna),names(df)))%>% # Pela coluna
    arrange(desc(VOTOS))%>%
    summarize(labels = paste(NM_VOTAVEL,": ",VOTOS,"%", collapse = '<br/>'))
  df.1$labels
  return(df.1)
}

tratamento <- function(dados, filtro.geo , diff = FALSE){
  
  if(filtro.geo == "Município"){
    geografia <- geo_mun
    coluna <- 'CD_MUNICIPIO'
  }else if(filtro.geo == "Estado"){
    geografia <- geo_est
    coluna <- 'SG_UF'
  }else{
    geografia <-geo_reg
    coluna <- 'name_region'
  }
  
  
  df <- dados[[1]]
  myCols <- c('NR_TURNO', 'NM_VOTAVEL', coluna,'VOTOS')
  df <- df %>% 
    ungroup()%>%
    select(all_of(myCols)) %>%
    group_by_at(myCols[1:3])%>%
    summarise(VOTOS = sum(VOTOS)) 
  df <- na.omit(df) # Retirando indefinidos
  
  if(diff){
    
    df <- df %>%
      pivot_wider(names_from = NR_TURNO, values_from = VOTOS) %>% 
      na.omit() %>%
      group_by_at(2) %>%
      mutate(`1` = `1`/sum(`1`))%>%
      mutate(`2` = `2`/sum(`2`))
    
    df$VOTOS <- round((df$`2` - df$`1`),4)
    df$NR_TURNO <- "Diferença"
    df <- df %>% select(match(myCols,names(df)))
  }
  
  tabela.labels <- label.function(df,coluna,diff)
  df <- pivot_wider(df, names_from = NM_VOTAVEL,values_from = VOTOS)
  df[is.na(df)] <- 0
  
  temp <- df[,-c(1,2,ncol(df))] %>%
    mutate(Ganhador = names(.)[max.col(., 'first')])
  
  if(diff==FALSE){
    df$TOTAL <- rowSums( df[,3:ncol(df)] )
    df[,-c(1,2,ncol(df))] <- df[,-c(1,2,ncol(df))]/df$TOTAL
    print("total")
  }
  
  df$Ganhador = temp$Ganhador
  df <- left_join(df,tabela.labels)
  df <- left_join(df, geografia, by = coluna)
  df <- na.omit(df)
  df$labels <-  paste0(paste(
    ifelse(is.na(df$name_state),"",
           paste0(df$name_state,", ")),
    df$name_region,"<br/>"),df$labels) %>% lapply(htmltools::HTML)
  
  #Paleta.cores(df, df$Ganhador)
  cod.cor <- dados[[2]]
  cor_funcoes <- paste0("cod.cor$`",
                        unique(df$Ganhador),
                        "`(df$`",unique(df$Ganhador),"`)") # Consulta para funções
  Lcores <- lapply(cor_funcoes, function(x) eval(parse(text=x))) # Gerando as paletas de cores para cada candidato
  Lcores <- as.data.frame(do.call(cbind, Lcores)) # Guardando em um dataframe
  names(Lcores) <- unique(df$Ganhador) # Identificando cada candidato
  Lcores$venceu <- df$Ganhador #Vendo quem venceu em cada local
  Lcores$cor <- Lcores[cbind(1:nrow(Lcores),match(Lcores$venceu, names(Lcores)))] # Atribuindo a cor a linha correspondente
  df$cor <-Lcores$cor # Adicionando ao banco original
  return(df)
}

mapa <- function(df, turno, diff=FALSE){
  df <- sf::st_as_sf(df)
  if(diff== FALSE){
    df <- df %>% filter(NR_TURNO == turno)
  }
  
  
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
      label = df$labels,
      labelOptions =  labelOptions(
        style = list("font-weigth"= "normal",
                     padding = "3px 8px"),
        textsize = "11px",
        direction = "auto")
    ) %>%
    setMapWidgetStyle(list(background= "#F5F5F5"))
}

#=========================================================================================#


ui <- div(
  
  # Definindo html -------------------------------------------------------------
  
  setBackgroundColor("#FFFFFF"),
  tags$style(HTML("
        @media (min-width: 768px) {
            body > div .container-fluid {
                width: 750px;
            }
        }
        @media (min-width: 992px) {
            body > div > .container-fluid {
                width: 970px;
            }
        }
        @media (min-width: 1200px) {
            body > div .container-fluid {
                width: 1170px;
            }
        }
        body > div > .container-fluid:nth-of-type(1) {
            margin: 0 auto;
            padding-top: 60px;
        }
        body > div > nav .nav.navbar-nav {
            float: right; /* Joga o menu para a esquerda */
        }
        .navbar-default {
        background-color: #003366;  !important; /* Define a cor do fundo*/
        }
        .navbar-nav li a{
        color: white !important;   /* Define a cor do menu */
        }    
  
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:	#003366 !important;background-color: #fff;} /* Quando seleciona o menu */
        .navbar-default .navbar-nav > li > a:hover {color: #F5F5F5 !important;}        /* Quando esta com o mouse em cima das opcoes do menu */

        
        .navbar-nav > li > a, .navbar-brand { /* Define o tamanho do menu*/
                            padding-top:20px !important; 
                            padding-bottom:20 !important;
                            height: 60px;
                            }
                           .navbar {min-height:25px !important;}  
                           
        .content-wrapper, .right-side {
                                background-color: #7da2d1;
                                }
        ")),
  
  tags$head(tags$style("
      .myRow1{height:250px;}
      .myRow2{height:350px;background-color: pink;}")
  ),
 
  # Abas e estrutura das paginas -----------------------------------------------
  
  navbarPage(
    position = "fixed-top",
    windowTitle = "",
    fluid = TRUE,
    title = div("",
                img(src = "https://raw.githubusercontent.com/lucasmpp/Eleicoes/Gabriel/Shiny/www/unb.png",
                    height = "45px",
                    style = "position: relative; 
                    margin:-15px 0px; 
                    display:right-align;")),
    selected = "Inicio",
    
    # Aba Inicio ---------------------------------------------------------------
    tabPanel(
      "Inicio",
      fluidPage(
        mainPanel(h1("Bem vindo ao analisador de dados das Eleições"),
                  h1("")
        ))
    ),
    
    # Aba Apuracao -------------------------------------------------------------
    tabPanel("Apuração",
             fluidRow(
               
               # Filtros -------------------------------------------------------
               column(12, 
                      wellPanel(style = "height: 100px;",
                                fluidRow(column(4,
                                                selectInput("ano", label = "Selecione o Ano",
                                                            choices = seq(2010,2022,by=4))),
                                         column(4, 
                                                selectInput("turno", label = "Selecione o Turno",
                                                            choices =c(1,2),
                                                            selected = 2),
                                         ),
                                         column(4, 
                                                selectInput("area", label = "Selecione a Área",
                                                            choices = c("Município","Estado","Região"),
                                                            selected = "Região")))
                                
                      )),
               
               # Mapa ----------------------------------------------------------
               column(7, 
                      wellPanel(style = "height: 800px;",
                                leafletOutput("mapa",width="100%",height="750px"))),
               
               # Tabelas -------------------------------------------------------
               column(5, 
                      fluidRow(column(12,
                                      wellPanel(style = "height: 580px;",
                                                p(tags$b("Votos Válidos", style = "font-size: 120%")),
                                                DT::dataTableOutput("tab_validos")
                                      ))),
                      
                      fluidRow(column(12, 
                                      wellPanel(style = "height: 200px;",
                                                p(tags$b("Nulos e Brancos", style = "font-size: 120%")),
                                                DT::dataTableOutput("tab_nulos")
                                                
                                      )))))),
    
    # Aba Projecao -------------------------------------------------------------
    tabPanel(
      "Projeção")
    
    )
  
)



server <- function(input, output, session) {
  
  
  dados_mapa <- reactive({ 
    req(input$ano)
    req(input$area)
    
    if(input$ano == 2010){
      mapa_db <- mapa_2010
      
    } else if(input$ano == 2014){
      mapa_db <- mapa_2014
      
    }else if(input$ano == 2018){
      mapa_db <- mapa_2018
      
    }else if(input$ano == 2022){
      mapa_db <- mapa_2022
    }
    tratamento(mapa_db, input$area, diff=FALSE)
    
  }) %>%
    bindCache(input$ano, input$area)

  
  output$mapa <- renderLeaflet({
    mapa(dados_mapa(),input$turno)
  })#%>%
    #bindCache(input$ano,input$turno,input$area)
  
  votos <- reactive({
    req(input$ano)
    
    if(input$ano == 2010){
      votos_2010
    } else if(input$ano == 2014){
      votos_2014
    }else if(input$ano == 2018){
      votos_2018
    }else if(input$ano == 2022){
      votos_2022
    }
  }) #%>%
    #bindCache(input$ano)
  
  tabela <- reactive({ 
    votos() %>%
      filter(NR_TURNO == input$turno & !(NR_VOTAVEL %in% c(95,96))) %>%
      group_by(NM_VOTAVEL)%>%
      summarise(TOTAL = sum(VOTOS))%>%
      mutate('FREQ' = paste0(100*round(TOTAL / sum(TOTAL),4),"%")) %>%
      arrange(desc(TOTAL))
    
  }) # %>% bindCache(input$ano,input$turno)
  
  nulo <- reactive({ 
    votos() %>%
      filter(NR_TURNO == input$turno) %>%
      group_by(NM_VOTAVEL)%>%
      summarise(TOTAL = sum(VOTOS))%>%
      mutate('FREQ' = paste0(100*round(TOTAL / sum(TOTAL),4),"%")) %>%
      filter(NM_VOTAVEL %in% c("VOTO NULO","VOTO BRANCO")) %>%
      arrange(desc(TOTAL))
    
  }) %>% bindCache(input$ano,input$turno)
  
  output$tab_validos <- DT::renderDataTable({
    DT::datatable(tabela(), 
                  rownames = F,
                  options = list(info = F,
                                 paging = F,
                                 searching = F,
                                 stripeClasses = F, 
                                 lengthChange = F,
                                 scrollY = '500px',
                                 scrollCollapse = T,
                                 headerCallback = JS(
                                   "function(thead, data, start, end, display){",
                                   "  $(thead).remove();",
                                   "}")))
  })
  
  output$tab_nulos <- DT::renderDataTable({
    DT::datatable(nulo(),
                  rownames = F,
                  options = list(info = F,
                                 paging = F,
                                 searching = F,
                                 stripeClasses = F, 
                                 lengthChange = F,
                                 scrollY = '150px',
                                 scrollCollapse = T,
                                 headerCallback = JS(
                                   "function(thead, data, start, end, display){",
                                   "  $(thead).remove();",
                                   "}")))
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
