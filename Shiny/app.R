setwd("C:/Users/gabri/OneDrive - unb.br/Estatística (1)/Padronização/Shiny")

library(shiny)
library(shinythemes)
library(shinyWidgets)


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
    selected = "Density",
  tabPanel(
    "Apuração"
  ),
  tabPanel(
    "Projeção"
    )
  )
)



server <- function(input, output, session) {}

# Run the application
shinyApp(ui = ui, server = server)
