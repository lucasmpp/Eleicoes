library(shiny)
library(shinythemes)
library(shinyWidgets)

pacman::p_load('tidyverse','geobr','sf','readr','openxlsx','leaflet',"leaflet.extras")

setwd("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/7º Semestre/LabEst/Git/Eleicoes/Shiny")

get(load("dados_reg.RData"))

mapa <- function(dados, turno){
  df <- sf::st_as_sf(dados) %>% filter(NR_TURNO == turno)
  
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
      #label =labels,
      labelOptions =  labelOptions(
        style = list("font-weigth"= "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
    setMapWidgetStyle(list(background= "#F5F5F5"))
  
}


ui <- div(
  setBackgroundColor("#F5F5F5"),
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
        background-color: #00843D;  !important; /* Define a cor do fundo*/
        }
        .navbar-nav li a{
        color: white !important;   /* Define a cor do menu */
        }    
  
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:	#00843D !important;background-color: #fff;} /* Quando seleciona o menu */
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
      

  navbarPage(
    position = "fixed-top",
    windowTitle = "",
    fluid = TRUE,
    title = div("",
                img(src = "unb.png",
                    height = "45px",
                    style = "position: relative; 
                    margin:-15px 0px; 
                    display:right-align;")),
    selected = "Inicio",
  tabPanel(
    "Inicio",
    fluidPage(
        mainPanel(h1("Bem vindo ao analisador de dados das Eleições"),
                  h1("")
      ))
    ),
  tabPanel(
    "Apuração",
    fluidPage(
      fluidRow(style = "height:700px", 
        column(width = 8,
               leafletOutput("mymap",width="90%",height="500px")
        ),
        column(width = 3,
               h1(""),
               selectInput("var1", label = "Selecione o ano",
                           choices = seq(2010,2022,by=4)),
               selectInput("var2", label = "Selecione o Turno",
                           choices =c(1,2)),
               selectInput("var3", label = "Selecione a área",
                           choices = c("Município","Estado","Região"))#,
               #tableOutput("tabela.apuracao")
               
        )
      ) 
    )),
  tabPanel(
    "Projeção")
  )
  , tags$head(tags$style("
      .myRow1{height:250px;}
      .myRow2{height:350px;background-color: pink;}")
  )
)



server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    mapa(dados_reg,input$var2)
  })
  
  output$tabela.apuracao <- renderTable({
    data.frame(x=c("A","B"),
               y = c(1,2))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
