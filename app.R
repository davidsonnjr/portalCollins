source("global/packageLoad.R")
#source("global/dataLoad.R")

# Main login screen

loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   fluidRow(tags$img(src = "logo.png", width="200", style = "position : relative; left: 120px;")),
                   tags$h2("", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Usuário", label = tagList(icon("user"), "Usuário")),
                   passwordInput("passwd", placeholder="Senha", label = tagList(icon("unlock-alt"), "Senha")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "Entrar", style = "color: white; background-color:#3c8dbc;
                                  padding: 10px 15px; width: 150px; cursor: pointer;
                                  font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Dados incorrectos!",
                                  style = "color: red; font-weight: 600; 
                                  padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Usuário: admin  Senha: admin123"),
                     br(),
                     tags$code("Usuário: collins  Senha: 12345")
                     ))
                     )

credentials = data.frame(
  username_id = c("collins", "admin"),
  passod   = sapply(c("12345", "admin123"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- bs4DashNavbar(skin = "light",
                          status = "white",
                          border = TRUE,
                          sidebarIcon = "bars",
                          compact = FALSE,
                          controlbarIcon = "th",
                          fixed = TRUE,
                        rightUi = uiOutput("logoutbtn"))

sidebar <- bs4DashSidebar(  skin = "light",
                            status = "primary",
                            title = tags$img(src = "logo.png", width = "100", style = "position: relative; right:-60px;"),
                            brandColor = "light",
                            url = "#",
                            src = NULL,
                            elevation = 3,
                            opacity = 0.8, uiOutput("sidebarpanel")) 

body <- bs4DashBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-bs4DashPage(    old_school = FALSE,
                    sidebar_min = TRUE,
                    sidebar_collapsed = FALSE,
                    controlbar_collapsed = FALSE,
                    controlbar_overlay = TRUE,
                    title = "Portal de Dados",
                    navbar = header,
                    sidebar = sidebar,
                    controlbar = NULL,
                    #footer = footer,
                    body = body
                    )

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Sair", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
            font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    
    if (USER$login == TRUE ){ 
        bs4SidebarMenu(
          br(),
          br(),
          br(),
          bs4SidebarMenuItem(
            "Horas de bombagem",
            tabName = "horas",
            icon = "clock"
          ),
          bs4SidebarMenuItem(
            "Volume de Água",
            icon = "fill",
            startExpanded = TRUE,
            condition = NULL,
            selected = NULL,
            bs4SidebarMenuSubItem(
              text = "Controle de volume", tabName = "volume", icon = "circle-thin"
            ),
            bs4SidebarMenuSubItem(
              text = "Caudal", tabName = "caudal", icon = "circle-thin"
            ),
            bs4SidebarMenuSubItem(
              text = "Perdas e facturação", tabName = "facturacao", icon = "circle-thin"
            )
          ),
          bs4SidebarMenuItem(
            "Qualidade da Água",
            tabName = "qualidade",
            icon = "id-card"
          ),
          bs4SidebarMenuItem(
            "Químicos e Reagentes",
            tabName = "quimicos",
            icon = "flask"
          ),
          bs4SidebarMenuItem(
            "Avarias e Interrupções",
            tabName = "avarias",
            icon = "columns"
          ),
          bs4SidebarMenuItem(
            "Tabelas",
            tabName = "tabela",
            icon = "list-ul"
          ),
          br(),
          pickerInput(
            inputId = "lcl",
            label = NULL, 
            choices = levels(dbVolume$local),
            selected = "Moamba",
            options = list(`live-search`=T,
          ),
          dateRangeInput(
            "dri",
            width = "290px",
            "Seleccione o intervalo de tempo",
            start = "2019-06-01",
            end = Sys.Date(),
            separator = "até",
            language = "pt"
          ),
          
          radioGroupButtons(
            inputId = "prd",
            label = "Período",
            choices = c("Diario", "Semanal", "Mensal"),
            status = "success",
            selected = "Mensal",
            direction = "horizontal",
            size = "normal"
            
          )
        )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
     # bs4DashBody(
        bs4TabItems(
          bs4TabItem(
            tabName = "horas",
            uiOutput("horas_B")
          ),
          bs4TabItem(
            tabName = "volume",
            uiOutput("vol"),
            #uiOutput("relacaoPF"),
            uiOutput("resumoVol")
            
          ),
          bs4TabItem(
            tabName = "caudal",
            uiOutput("caudal_")
            
          ),
          bs4TabItem(
            tabName = "facturacao",
            uiOutput("fact_card"),
            uiOutput("factPerdas")
            
          ),
          bs4TabItem(
            tabName = "qualidade",
            uiOutput("qualidade_agua")
          ),
          bs4TabItem(
            tabName = "quimicos",
            fluidRow(
              bs4ValueBoxOutput(width = 3,  "sa"),
              bs4ValueBoxOutput(width = 3,  "cl"),
              bs4ValueBoxOutput(width = 3, "dpd1"),
              bs4ValueBoxOutput(width = 3, "ph_1")
            ),
            fluidRow(
              bs4Box(
                width = 12,
                solidHeader = F,
                collapsible = F,
                highchartOutput("barChartq")
              )
            ),
            fluidRow(
              bs4Box(
                # title = "Consumo de Reagentes de Analise de Qualidade de Agua (Unidades)",
                width = 12,
                solidHeader = F,
                #   status = "primary",
                collapsible = F,
                highchartOutput("barChartt")
              )
            )
          ),
          bs4TabItem(
            tabName = "avarias",
            fluidRow(
              bs4ValueBoxOutput(width = 4,  "avarias_adutora"),
              bs4ValueBoxOutput(width = 4,  "avarias_rd"),
              bs4ValueBoxOutput(width = 4,  "avarias_outras")
            ),
            fluidRow(
              bs4Box(
                width = 12,
                solidHeader = T,
                collapsible = F,
                highchartOutput("chartAvariasT")
              )
            ),
            
            fluidRow(
              bs4ValueBoxOutput(width = 4,  "horas_avarias"),
              bs4ValueBoxOutput(width = 4,  "horas_energia"),
              bs4ValueBoxOutput(width = 4,  "horas_outras")
            ),
            fluidRow(
              bs4Box(
                width = 12,
                solidHeader = T,
                collapsible = F,
                highchartOutput("chartAvarias")
              )
            )
          ),
          bs4TabItem(
            tabName = "tabela",
            bs4TabCard(
              id = "tabelas",
              width = 12,
              # collapsible = FALSE,
              # collapsed = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Volumes de Água",
                active = TRUE,
                DTOutput("tbl")
              ),
              
              bs4TabPanel(
                tabName = "Qualidade",
                active = FALSE,
                DTOutput("tbl1")),
              
              bs4TabPanel(
                tabName = "Quimicos",
                active = FALSE,
                
                DTOutput("tbl2")),
              
              bs4TabPanel(
                tabName = "Avarias",
                active = FALSE,
                
                DTOutput("tbl3")),
              
              bs4TabPanel(
                tabName =   "Funcionamento bombas",
                active = FALSE,
                DTOutput("tbl4"))
              
            )
            
          )
        )
      #)
      
    }
    else {
      loginpage
    }
  })
  
 
  
  
  #HORAS DE BOMBAGEM
  horas <- reactive({
    horas_tbl %>% filter(between(date, input$dri[1], input$dri[2]) &
                           local == input$lcl)
  })
  
  horas_c <- reactive({
    switch (
      input$prd,
      "Diario" = horas() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "day")) %>%
        summarise_at(vars(C1.B1:C5.B2), sum),
      
      "Semanal" =  horas() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "week")) %>%
        summarise_at(vars(C1.B1:C5.B2), sum),
      
      "Mensal" =  horas() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "month")) %>%
        summarise_at(vars(C1.B1:C5.B2), sum)
      
    )
    
  })
  
  horas_mensais <- reactive({
    horas() %>%
      select_all() %>%
      group_by(date = lubridate::floor_date(date, unit = "month")) %>%
      summarise_at(vars(C1.B1:C5.B2), sum)
    
  })
  
  
  #AVARIAS
  avarias_tbl <- reactive({
    avarias %>% filter(between(date, input$dri[1], input$dri[2]) &
                         local == input$lcl)})
  
  #VOLUMES
  newDb <- reactive({
    dbVolume %>%  filter(between(date, input$dri[1], input$dri[2]) &
                           local == input$lcl)
  })
  
  nova_base <- reactive ({
    switch (
      input$prd,
      "Diario" = newDb() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "day")) %>%
        summarise_at(vars(aduzido,
                          distribuido,
                          distribuido_estacao,
                          captado,
                          tratado,
                          elevado,
                          pessene
                          
        ), sum),
      "Semanal" =  newDb() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "week")) %>%
        summarise_at(vars(
          aduzido,
          distribuido,
          distribuido_estacao,
          captado,
          tratado,
          elevado,
          pessene
        ), sum),
      "Mensal" =  newDb() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "month")) %>%
        summarise_at(vars(
          aduzido,
          distribuido,
          distribuido_estacao,
          captado,
          tratado,
          elevado,
          pessene
        ), sum)
    )
  })
  
  #QUIMICOS
  quimicos <- reactive({
    consumo %>% filter(between(date, input$dri[1], input$dri[2]) &
                         local == input$lcl)
    
  })
  
  quimicos_tbl <-  reactive({
    quimicos() %>%
      select_all() %>%
      group_by(date = lubridate::floor_date(date, unit = "month")) %>%
      summarise_at(vars(stock_sa:qtd_ph), mean)
  })
  
 

  #NOVAS BASES
  newDb <- reactive({
    dbVolume %>%  filter(between(date, input$dri[1], input$dri[2]) &
                           local == input$lcl)
  })
  
  
  quality <- reactive({
    qualidade %>% filter(between(date, input$dri[1], input$dri[2])&
                           local == input$lcl)
    
  })
  
  facturado <- reactive({
    fact %>% filter(between(date, input$dri[1], input$dri[2])
                    & local== input$lcl)
  })
  
  
  quality_agua <- reactive ({
    switch (
      input$prd,
      "Diario" = quality() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "day")) %>%
        summarise_at(
          vars(
            pH_Captacao,
            Turvacao_Captacao,
            pH_CD,
            Turvacao_CD,
            `Cloro residual_CD`,
            phDeposito_elevado,
            turvacao_deposito_elevado,
            `cloro residual_DE`,
            pH_RD1,
            Turvacao_RD1,
            `Cloro residual_RD1`,
            pH_RD2,
            Turvacao_RD2,
            `Cloro residual_RD2`,
            phETA,
            turvacaoETA,
            `Cloro residual_ETA`,
            turvacao_CD,
            pH_RD3,
            Turvacao_RD3,
            `Cloro residual_RD3`,
            pCaptacaoF1,
            phCaptacaoF2,
            phCaptacaoF3,
            phCaptacaoF4,
            ceCaptacaoF1,
            ceCaptacaoF2,
            ceCaptacaoF3,
            ceCaptacaoF4,
            CECD,
            `Cloro residual_CE`,
            CE_RD1,
            CE_RD2,
            CE_RD3,
            `Cloro residual_DE3`,
            phCaptacaoCE1,
            phCaptacaoCE2,
            phCaptacaoCE3,
            CDRD1,
            CDRD2,
            CDRD3,
            pH_Captacao_CE4,
            pH_CF,
            pH_CB,
            `cloro residual_CB`,
            `Conformes CD`,
            `Conformes DE`,
            `Conformes RD1`
          ),
          mean
        ),
      "Semanal" =  quality() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "week")) %>%
        summarise_at(
          vars(
            pH_Captacao,
            Turvacao_Captacao,
            pH_CD,
            Turvacao_CD,
            `Cloro residual_CD`,
            phDeposito_elevado,
            turvacao_deposito_elevado,
            `cloro residual_DE`,
            pH_RD1,
            Turvacao_RD1,
            `Cloro residual_RD1`,
            pH_RD2,
            Turvacao_RD2,
            `Cloro residual_RD2`,
            phETA,
            turvacaoETA,
            `Cloro residual_ETA`,
            turvacao_CD,
            pH_RD3,
            Turvacao_RD3,
            `Cloro residual_RD3`,
            pCaptacaoF1,
            phCaptacaoF2,
            phCaptacaoF3,
            phCaptacaoF4,
            ceCaptacaoF1,
            ceCaptacaoF2,
            ceCaptacaoF3,
            ceCaptacaoF4,
            CECD,
            `Cloro residual_CE`,
            CE_RD1,
            CE_RD2,
            CE_RD3,
            `Cloro residual_DE3`,
            phCaptacaoCE1,
            phCaptacaoCE2,
            phCaptacaoCE3,
            CDRD1,
            CDRD2,
            CDRD3,
            pH_Captacao_CE4,
            pH_CF,
            pH_CB,
            `cloro residual_CB`,
            `Conformes CD`,
            `Conformes DE`,
            `Conformes RD1`
          ),
          mean
        ),
      "Mensal" =  quality() %>%
        select_all() %>%
        group_by(date = lubridate::floor_date(date, unit = "month")) %>%
        summarise_at(
          vars(
            pH_Captacao,
            Turvacao_Captacao,
            pH_CD,
            Turvacao_CD,
            `Cloro residual_CD`,
            phDeposito_elevado,
            turvacao_deposito_elevado,
            `cloro residual_DE`,
            pH_RD1,
            Turvacao_RD1,
            `Cloro residual_RD1`,
            pH_RD2,
            Turvacao_RD2,
            `Cloro residual_RD2`,
            phETA,
            turvacaoETA,
            `Cloro residual_ETA`,
            turvacao_CD,
            pH_RD3,
            Turvacao_RD3,
            `Cloro residual_RD3`,
            pCaptacaoF1,
            phCaptacaoF2,
            phCaptacaoF3,
            phCaptacaoF4,
            ceCaptacaoF1,
            ceCaptacaoF2,
            ceCaptacaoF3,
            ceCaptacaoF4,
            CECD,
            `Cloro residual_CE`,
            CE_RD1,
            CE_RD2,
            CE_RD3,
            `Cloro residual_DE3`,
            phCaptacaoCE1,
            phCaptacaoCE2,
            phCaptacaoCE3,
            CDRD1,
            CDRD2,
            CDRD3,
            pH_Captacao_CE4,
            pH_CF,
            pH_CB,
            `cloro residual_CB`,
            `Conformes CD`,
            `Conformes DE`,
            `Conformes RD1`
          ),
          mean
        )
    )
  })
  # 
  aux_tbl <- reactive({
    aux_tbl <- quality() %>%
      select_all() %>%
      summarise(
        conformes = sum(quality()$`Conformes CD`) + sum(quality()$`Conformes DE`) +
          sum(quality()$`Conformes RD1`),
        n_conformes = (nrow(quality()) * 3) - (sum(quality()$`Conformes CD`) + sum(quality()$`Conformes DE`) +
                                                 sum(quality()$`Conformes RD1`)
        )
      )
    aux_tbl <- t(aux_tbl)
    
    aux_tbl
    
  })
  
  
  
  ######## INICIO BOMBAGEM RESUMO ##########
 
  
  
  output$horas_B <- renderUI({
    if (input$lcl == "Combomune"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 1",
          active = FALSE,
          uiOutput("c1_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_1",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartH")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotCaptacao")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 2",
          active = FALSE,
          uiOutput("c2_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_2",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartAd")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotAducao")
              )))
        )
      )
      
    } else if (input$lcl == "Homoine"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 1",
          active = FALSE,
          uiOutput("c1_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_1",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartH")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotCaptacao")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 2",
          active = FALSE,
          uiOutput("c2_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_2",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartAd")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotAducao")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 3",
          active = FALSE,
          uiOutput("c3_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_3",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartTr")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotTratamento")
              )))
        )
      )
      
    } else if (input$lcl == "Inharrime"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 2",
          active = FALSE,
          uiOutput("c2_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_2",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartAd")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotAducao")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 3",
          active = FALSE,
          uiOutput("c3_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_3",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartTr")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotTratamento")
              )))
        )
      )
    } else if(input$lcl == "Ulongue"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 1",
          active = FALSE,
          uiOutput("c1_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_1",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartH")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotCaptacao")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 2",
          active = FALSE,
          uiOutput("c2_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_2",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartAd")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotAducao")
              )))
          
          
        )
      )
    } else if (input$lcl == "Jangamo"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 1",
          active = FALSE,
          uiOutput("c1_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_1",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartH")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotCaptacao")
              )))
        )
      )
    } else if(input$lcl == "Moamba"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 1",
          active = FALSE,
          uiOutput("c1_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_1",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartH")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotCaptacao")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 2",
          active = FALSE,
          uiOutput("c2_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_2",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartAd")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotAducao")
              )))
          
          
        ),
        
        bs4TabPanel(
          tabName =  "Conjunto 3",
          active = FALSE,
          uiOutput("c3_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_3",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartTr")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotTratamento")
              )))
        ),
        
        bs4TabPanel(
          tabName =  "Conjunto 4",
          active = FALSE,
          uiOutput("c4_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_4",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartHel")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotElevacao")
              )))
        ),
        
        bs4TabPanel(
          tabName =  "Conjunto 5",
          active = FALSE,
          uiOutput("c5_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_5",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartHel_5")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotElevacao_5")
              )))
        )
      )
    } else if (input$lcl == "Tomanine"){
      
      fluidRow(
        bs4TabCard(
          id = "tabcard",
          width = 12,
          collapsible = FALSE,
          closable = FALSE,
          title = "Tomanine",
          bs4TabPanel(
            tabName = "Funcionamento",
            active = TRUE,
            highchartOutput("varResumo")
          ),
          bs4TabPanel(
            tabName = "Sumário",
            active = FALSE,
            highchartOutput("bxplotResumo")
          )))
      
      
    } else if (input$lcl == "Mopeia"){
      bs4TabSetPanel(
        id="bombagem",
        side = "left",
        bs4TabPanel(
          tabName = "Resumo",
          active = TRUE,
          uiOutput("resumo_ind"),
          fluidRow(
            bs4TabCard(
              id = "tabcard",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("varResumo")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotResumo")
              )))
        ),
        bs4TabPanel(
          tabName =  "Conjunto 1",
          active = FALSE,
          uiOutput("c1_ind"),
          fluidRow(
            bs4TabCard(
              id = "conj_1",
              width = 12,
              collapsible = FALSE,
              closable = FALSE,
              title = NULL,
              bs4TabPanel(
                tabName = "Funcionamento",
                active = TRUE,
                highchartOutput("barChartH")
              ),
              bs4TabPanel(
                tabName = "Sumário",
                active = FALSE,
                highchartOutput("bxplotCaptacao")
              )))
        )
      )
    }
    
  })
  
  
  ####### INICIO RESUMO ###########
  output$resumo_ind <- renderUI({
    
    if (input$lcl=="Combomune"){
      fluidRow(
        br(),
        
        bs4ValueBoxOutput(width = 4, "rc"),
        bs4ValueBoxOutput(width = 4, "ra"),
        bs4ValueBoxOutput(width = 4, "rTotal")#
        
      )
    } else if (input$lcl=="Homoine"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "rc"),
        bs4ValueBoxOutput(width = 3,  "ra"),
        bs4ValueBoxOutput(width = 3, "rt"),
        bs4ValueBoxOutput(width = 3,  "rTotal")
      )
      
    }else if (input$lcl=="Inharrime"){
      fluidRow(
        br(),
        # bs4ValueBoxOutput(width = 2, outputId = "rc"),
        bs4ValueBoxOutput(width = 4,  "ra"),
        bs4ValueBoxOutput(width = 4,  "rt"),
        # bs4ValueBoxOutput(width = 2, outputId = "re"),
        # bs4ValueBoxOutput(width = 2, outputId = "re_5"),
        bs4ValueBoxOutput(width = 4,  "rTotal")
      )
      
    }else if (input$lcl=="Jangamo"|input$lcl=="Mopeia"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 6,  "rc"),
        bs4ValueBoxOutput(width = 6,  "rTotal")
      )
      
    }else if (input$lcl=="Ulongue"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4,  "rc"),
        bs4ValueBoxOutput(width = 4, "ra"),
        bs4ValueBoxOutput(width = 4,  "rTotal")
      )
      
    }else if (input$lcl=="Moamba"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3, "rc"),
        bs4ValueBoxOutput(width = 3, "ra"),
        bs4ValueBoxOutput(width = 3,  "rt"),
        bs4ValueBoxOutput(width = 3,  "re"),
        bs4ValueBoxOutput(width = 3,   "re_5"),
        bs4ValueBoxOutput(width = 3,  "rTotal")
      )
    }
  })
  
  
  #### INICIO QUADRADINHOS######
  
  #### PRIMEIRO
  output$rc <- renderbs4ValueBox({
    prc <- (sum(horas()[c(2:4)])/sum(horas()[2:14])) * 100
    
    bs4ValueBox(
      value =   tags$p(paste(scales::comma(sum(horas()[c(2:4)]))
      ), " horas", "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      subtitle = tags$p(strong(paste(
        "Captação"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
      
    )
  })
  
  ##### SEGUNDO
  
  
  output$ra <- renderbs4ValueBox({
    prc <- (sum(horas()[c(5:7)])/sum(horas()[2:14])) * 100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[c(5:7)]))
      ), " horas", "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "Adução"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  
  #### TERCEIRO
  
  output$rt <- renderbs4ValueBox({
    prc <- (sum(horas()[c(8:10)])/sum(horas()[2:14])) * 100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[c(8:10)]))
      ), " horas", "(", round(prc, 2), "%)",  style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =   tags$p(strong(paste(
        "Tratamento"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  #### TERCEIRO
  
  output$re <- renderbs4ValueBox({
    prc <- (sum(horas()[c(11:12)])/sum(horas()[2:14])) * 100
    
    bs4ValueBox(
      value =   tags$p(paste(scales::comma(sum(horas()[c(11:12)]))
      ), " horas", "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "Elevação"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ####### QUARTO
  
  output$re_5 <- renderbs4ValueBox({
    prc <- (sum(horas()[c(13:14)])/sum(horas()[2:14])) * 100
    
    bs4ValueBox(
      value =  tags$p(paste( 
        scales::comma(sum(horas()[c(13:14)]))
      ), " horas", "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        " Conjunto 5"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ##### TOTAL
  output$rTotal <- renderbs4ValueBox({
    bs4ValueBox(
      value =   tags$p(paste(
        scales::comma(sum(horas()[2:14])
        ), "horas", sep = " "
      ), style = "font-size: 120%;"),
      subtitle = "Total",
      status = "warning",
      icon = "calculator"
    )
  })
  
  
  
  ##### FIM QUADRADINHOS
  
  output$varResumo <- renderHighchart({
    horas_mensais<- horas() %>%
      select_all() %>%
      group_by(date = lubridate::floor_date(date, unit = "month")) %>%
      summarise_at(vars(C1.B1:C5.B2), sum)
    
    if(input$lcl=="Inharrime"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        
        hc_add_series(name="C2",
                      data=horas_mensais()$C2.B1+horas_mensais()$C2.B2+horas_mensais()$C2.B3)%>%
        
        hc_add_series(name="C3",
                      data=horas_mensais()$C3.B1+horas_mensais()$C3.B2+horas_mensais()$C3.B3)%>%
        
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "",
                    style = list(fontWeight = "bold")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }else if(input$lcl=="Homoine"){
      
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C1",
                      data=horas_mensais()$C1.B1+horas_mensais()$C1.B2+horas_mensais()$C1.B3)%>%
        
        hc_add_series(name="C2",
                      data=horas_mensais()$C2.B1+horas_mensais()$C2.B2+horas_mensais()$C2.B3)%>%
        
        hc_add_series(name="C3",
                      data=horas_mensais()$C3.B1+horas_mensais()$C3.B2+horas_mensais()$C3.B3)%>%
        
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "",
                    style = list(fontWeight = "bold")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }else if(input$lcl=="Moamba"){
      
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C1",
                      data=horas_mensais()$C1.B1+horas_mensais()$C1.B2+horas_mensais()$C1.B3)%>%
        
        hc_add_series(name="C2",
                      data=horas_mensais()$C2.B1+horas_mensais()$C2.B2+horas_mensais()$C2.B3)%>%
        
        hc_add_series(name="C3",
                      data=horas_mensais()$C3.B1+horas_mensais()$C3.B2+horas_mensais()$C3.B3)%>%
        
        hc_add_series(name="C4",
                      data=horas_mensais()$C4.B1+horas_mensais()$C4.B2)%>%
        hc_add_series(name="C5",
                      data=horas_mensais()$C5.B1+horas_mensais()$C5.B2)%>%
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "",
                    style = list(fontWeight = "bold")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
      
    }else if(input$lcl=="Combomune"| input$lcl=="Ulongue"){
      
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C1",
                      data=horas_mensais()$C1.B1+horas_mensais()$C1.B2+horas_mensais()$C1.B3)%>%
        
        hc_add_series(name="C2",
                      data=horas_mensais()$C2.B1+horas_mensais()$C2.B2+horas_mensais()$C2.B3)%>%
        
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "",
                    style = list(fontWeight = "bold")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }else if(input$lcl=="Mopeia"|input$lcl=="Jangamo"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C1",
                      data=horas_mensais()$C1.B1+horas_mensais()$C1.B2+horas_mensais()$C1.B3)%>%
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "",
                    style = list(fontWeight = "bold")) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }
    
  })
  
  output$bxplotResumo <- renderHighchart({
    
    if(input$lcl=="Inharrime"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C2",x = horas()$C2.B1+horas()$C2.B2+horas()$C2.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C3",x = horas()$C3.B1+horas()$C3.B2+horas()$C3.B3, outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        # hc_subtitle(text = "4") %>%
        
        hc_plotOptions(boxplot = list(
          fillColor = alphabet()[6],
          lineWidth = 5
          
        ))
      
    }else if(input$lcl=="Homoine"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C1",x = horas()$C1.B1+horas()$C1.B2+horas()$C1.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C2",x = horas()$C2.B1+horas()$C2.B2+horas()$C2.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C3",x = horas()$C3.B1+horas()$C3.B2+horas()$C3.B3, outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        # hc_subtitle(text = "4") %>%
        
        hc_plotOptions(boxplot = list(
          fillColor = alphabet()[6],
          lineWidth = 5
          
        ))
      
    }else if(input$lcl=="Moamba"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C1",x = horas()$C1.B1+horas()$C1.B2+horas()$C1.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C2",x = horas()$C2.B1+horas()$C2.B2+horas()$C2.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C3",x = horas()$C3.B1+horas()$C3.B2+horas()$C3.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C4",x = horas()$C4.B1+horas()$C4.B2,outliers = F)%>%
        
        hc_add_series_boxplot(name="C5",x = horas()$C5.B1+horas()$C5.B2, outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        # hc_subtitle(text = "4") %>%
        
        hc_plotOptions(boxplot = list(
          fillColor = alphabet()[6],
          lineWidth = 5
          
        ))
      
    }else if(input$lcl=="Combomune"| input$lcl=="Ulongue"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C1",x = horas()$C1.B1+horas()$C1.B2+horas()$C1.B3, outliers = F)%>%
        
        hc_add_series_boxplot(name="C2",x = horas()$C2.B1+horas()$C2.B2+horas()$C2.B3, outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        # hc_subtitle(text = "4") %>%
        
        hc_plotOptions(boxplot = list(
          fillColor = alphabet()[6],
          lineWidth = 5
          
        ))
      
    }else if(input$lcl=="Mopeia"|input$lcl=="Jangamo"){
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C1",x = horas()$C1.B1+horas()$C1.B2+horas()$C1.B3, outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        # hc_subtitle(text = "4") %>%
        
        hc_plotOptions(boxplot = list(
          fillColor = alphabet()[6],
          lineWidth = 5
          
        ))
      
    }
    
    
  })
  
  ####### FIM RESUMO ###########
  
  
  ###### INICIO CONJUNTO 1
  
  output$c1_ind <- renderUI({
    if (input$lcl=="Ulongue" | input$lcl=="Combomune" | input$lcl=="Moamba"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4, "conjunto_1_1"),
        bs4ValueBoxOutput(width = 4, "conjunto_1_2"),
        #  bs4ValueBoxOutput(width = 3, outputId = "conjunto_1_3"),
        bs4ValueBoxOutput(width = 4, "conjunto_1_Total")
      )
      
    }else{
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "conjunto_1_1"),
        bs4ValueBoxOutput(width = 3,  "conjunto_1_2"),
        bs4ValueBoxOutput(width = 3,  "conjunto_1_3"),
        bs4ValueBoxOutput(width = 3,  "conjunto_1_Total")
      )
      
    }
  })
  
  output$c2_ind <- renderUI({
    if (input$lcl=="Homoine"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "cA"),
        bs4ValueBoxOutput(width = 3,  "ccA"),
        bs4ValueBoxOutput(width = 3,  "ccA_3"),
        bs4ValueBoxOutput(width = 3,  "caTotal")
      )
      
    }else{
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4,  "cA"),
        bs4ValueBoxOutput(width = 4,  "ccA"),
        bs4ValueBoxOutput(width = 4, "caTotal")
      )
      
    }
  })
  
  #### INICIO QUADRADINHOS   
  
  #### PRIMEIRO
  output$conjunto_1_1 <- renderbs4ValueBox({
    prc <- (sum(horas()[2])/sum(horas()[2:4]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma( sum(horas()[2])), " horas"), "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      subtitle =  tags$p(strong(paste(
        "C1-B1"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  
  ##### SEGUNDO
  output$conjunto_1_2 <- renderbs4ValueBox({
    prc <- (sum(horas()[3])/sum(horas()[2:4]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma(sum(horas()[3]))
        , " horas"), "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      subtitle =   tags$p(strong(paste(
        "C1-B2"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ## Terceiro
  output$conjunto_1_3 <- renderbs4ValueBox({
    prc <- (sum(horas()[4])/sum(horas()[2:4]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma(sum(horas()[4]))
        , " horas"), "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      subtitle =  tags$p(strong(paste(
        "C1-B3"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  
  #### TOTAL 
  
  ##### Total
  output$conjunto_1_Total <- renderbs4ValueBox({
    
    bs4ValueBox(
      value =   tags$p(paste(
        scales::comma(sum(horas()[2:4]))
        , "horas", sep = " "), style = "font-size: 120%;"),
      "Total conjunto",
      
      status = "warning",
      icon = "calculator"
    )
  })
  
  
  ### FIM QUADRADINHOS
  
  
  ####### INICIO CHART
  
  output$barChartH <- renderHighchart({
    
    if (input$lcl=="Ulongue" | input$lcl=="Combomune" | input$lcl=="Moamba"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_title(text = "Horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "Conjunto Captação",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) %>%
        
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%  
        
        hc_add_series(name="C1-B1",
                      data=horas_mensais()$C1.B1)%>%
        
        hc_add_series(name="C1-B2",
                      data=horas_mensais()$C1.B2)
      
      
    }else{
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_title(text = "Horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "Conjunto Captação",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) %>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C1-B1",
                      data=horas_mensais()$C1.B1)%>%
        
        hc_add_series(name="C1-B2",
                      data=horas_mensais()$C1.B2)%>%
        
        hc_add_series(name="C1-B3",
                      data=horas_mensais()$C1.B3)
      
      
      
    }
    
  })
  
  ##### FIM CHART
  
  ##### INICIO SUMARIO
  output$bxplotCaptacao <- renderHighchart({
    
    
    if (input$lcl=="Ulongue" | input$lcl=="Combomune" | input$lcl=="Moamba"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        hc_subtitle(text = "Conjunto 1") %>%
        
        hc_plotOptions(boxplot = list(
          lineWidth = 5
          
        ))%>%
        
        hc_add_series_boxplot(name="C1-B1",x = horas()$C1.B1, outliers = F)%>%
        
        hc_add_series_boxplot(name="C1-B2",x = horas()$C1.B2, outliers = F)
      
      
      
    }else{
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        hc_subtitle(text = "Conjunto 1") %>%
        
        hc_plotOptions(boxplot = list(
          lineWidth = 5
          
        ))%>%
        
        hc_add_series_boxplot(name="C1-B1",x = horas()$C1.B1, outliers = F)%>%
        
        hc_add_series_boxplot(name="C1-B2",x = horas()$C1.B2,outliers = F)%>%
        
        hc_add_series_boxplot(name="C1-B3",x = horas()$C1.B3,outliers = F)
      
    }
    
  })
  
  ##### FIM SUMARIO
  
  ##### FIM CONJUNTO 1
  
  
  ##### INICIO CONJUNTO 2
  output$c2_ind <- renderUI({
    if (input$lcl=="Homoine"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "cA"),
        bs4ValueBoxOutput(width = 3, "ccA"),
        bs4ValueBoxOutput(width = 3,  "ccA_3"),
        bs4ValueBoxOutput(width = 3,  "caTotal")
      )
      
    }else{
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4,  "cA"),
        bs4ValueBoxOutput(width = 4,  "ccA"),
        bs4ValueBoxOutput(width = 4,  "caTotal")
      )
      
    }
  })
  
  ## CONJUNTO 2 QUADRADINHOS
  output$cA <- renderbs4ValueBox({
    prc <- (sum(horas()[5])/sum(horas()[5:7]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[5])), " horas"), "(",
                      round(prc, 2),"%)", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C2-B1"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  output$ccA <- renderbs4ValueBox({
    prc <- (sum(horas()[6])/sum(horas()[5:7]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[6])), " horas"), "(", 
                      round(prc, 2), "%)", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C2-B2"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ## Terceiro
  
  output$ccA_3 <- renderbs4ValueBox({
    prc <- (sum(horas()[7])/sum(horas()[5:7]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(sum(horas()[7]), " horas"), "(", round(prc, 2), "%)", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste("C2-B3"
      )), style = "font-size: 100%;"),
      
      status  = "primary"
    )
  })
  
  ##### Total
  output$caTotal <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[5:7]))
                            , "horas", sep = " "), style = "font-size: 120%;"),
      "Total conjunto",
      status = "warning",
      icon = "calculator"
      
    )
  })
  
  ###### FIM QUADRADINHOS
  
  #### FIM CONJUNTO 2
  
  #### INICIO RESUMO 
  output$barChartAd <- renderHighchart({
    
    if(input$lcl=="Homoine"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C2-B1",
                      data=horas_mensais()$C2.B1)%>%
        
        hc_add_series(name="C2-B2",
                      data=horas_mensais()$C2.B2)%>%
        
        hc_add_series(name="C2-B3",
                      data=horas_mensais()$C2.B3)%>%
        
        hc_title(text = "Horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "Conjunto Captação",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }else{
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C2-B1",
                      data=horas_mensais()$C2.B1)%>%
        
        hc_add_series(name="C2-B2",
                      data=horas_mensais()$C2.B2)%>%
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "Conjunto Aduçao",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }
    
  })
  
  
  output$bxplotAducao <- renderHighchart({
    
    if(input$lcl=="Homoine"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C2-B1",x = horas()$C2.B1, outliers = F)%>%
        
        hc_add_series_boxplot(name="C2-B2",x = horas()$C2.B2,outliers = F)%>%
        
        hc_add_series_boxplot(name="C2-B3",x = horas()$C2.B3,outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        hc_subtitle(text = "Conjunto 2") %>%
        
        hc_plotOptions(boxplot = list(
          # fillColor = alphabet()[6],
          lineWidth = 5
          
        )) 
      
    }else{
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C2-B1",x = horas()$C2.B1, outliers = F)%>%
        
        hc_add_series_boxplot(name="C2-B2",x = horas()$C2.B2,outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        hc_subtitle(text = "Conjunto 2") %>%
        
        hc_plotOptions(boxplot = list(
          fillColor = alphabet()[6],
          lineWidth = 5
          
        )) 
      
    }
    
    
  })
  
  #### FIM RESUMO
  
  ###### INICIO CONJUNTO 3
  
  output$c3_ind <- renderUI({
    if (input$lcl=="Inharrime"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4, "cT"),
        bs4ValueBoxOutput(width = 4, "ccT"),
        bs4ValueBoxOutput(width = 4, "ccTTotal")
      )
      
    }else{
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3, "cT"),
        bs4ValueBoxOutput(width = 3, "ccT"),
        bs4ValueBoxOutput(width = 3,  "cccT"),
        bs4ValueBoxOutput(width = 3, "ccTTotal")
      )
      
    }
  })
  
  
  ##### INICIO QUADRADINHOS
  
  output$cT <- renderbs4ValueBox({
    
    prc <- (sum(horas()[8])/sum(horas()[8:10]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[8])), " horas (", paste(
                           round(prc, 2), "%)")), style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C3-B1"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon =  "chart-line"
    )
  })
  
  ## Segundo
  
  output$ccT <- renderbs4ValueBox({
    prc <- (sum(horas()[9])/sum(horas()[8:10]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[9])), " horas (",paste(
                             round(prc, 2), "%)"  )), style = "font-size: 120%;"),
      
      subtitle = tags$p(strong(paste(
        "C3-B2"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  #### Terceiro
  
  output$cccT <- renderbs4ValueBox({
    prc <- (sum(horas()[10])/sum(horas()[8:10]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(scales::comma(sum(horas()[10])), "horas (",paste(
                            round(prc, 2), "%)"  )), style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C3-B3"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
      
    )
  })
  ##### Total
  output$ccTTotal <- renderbs4ValueBox({
    bs4ValueBox(
      value =   tags$p(paste((scales::comma(sum(horas()[8:10]))), "horas", sep = " "), style = "font-size: 120%;"),
      subtitle =    "Total conjunto",
      status = "warning",
      icon = "calculator"
    )
  })
  
  
  ##### INICIO RESUMO
  
  output$barChartTr <- renderHighchart({
    
    if (input$lcl=="Inharrime"){
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C3-B1",
                      data=horas_mensais()$C3.B1)%>%
        
        hc_add_series(name="C3-B2",
                      data=horas_mensais()$C3.B2)%>%
        
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "Conjunto 3",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }else{
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
        
        hc_add_series(name="C3-B1",
                      data=horas_mensais()$C3.B1)%>%
        
        hc_add_series(name="C3-B2",
                      data=horas_mensais()$C3.B2)%>%
        
        hc_add_series(name="C3-B3",
                      data=horas_mensais()$C3.B3)%>%
        
        hc_title(text = "horas de funcionamento das bombas") %>%
        
        hc_subtitle(text = "Conjunto 3",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
      
    }
    
  })
  
  output$bxplotTratamento <- renderHighchart({
    
    if(input$lcl=="Inharrime"){
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C3-B1",x = horas()$C3.B1, outliers = F)%>%
        
        hc_add_series_boxplot(name="C3-B2",x = horas()$C3.B2,outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        hc_subtitle(text = "Conjunto 3") %>%
        
        hc_plotOptions(boxplot = list(
          #fillColor = alphabet()[6],
          lineWidth = 5
          
        ))
      
    }else{
      
      highchart()%>%
        
        hc_xAxis(categories=c("Sumário"))%>%
        
        hc_add_series_boxplot(name="C3-B1",x = horas()$C3.B1, outliers = F)%>%
        
        hc_add_series_boxplot(name="C3-B2",x = horas()$C3.B2,outliers = F)%>%
        
        hc_add_series_boxplot(name="C3-B3",x = horas()$C3.B3,outliers = F)%>%
        
        hc_chart(type = "column")%>%
        hc_title(text = "Distribuição das horas de funcionamento") %>%
        hc_subtitle(text = "Conjunto 3") %>%
        
        hc_plotOptions(boxplot = list(
          lineWidth = 5
          
        ))
    }
    
  })
  
  
  
  
  
  #####FIM RESUMO
  #### FIM CONJUNTO 3
  
  ##### INICIO CONJUNTO 4
  
  output$c4_ind <- renderUI({
    
    fluidRow(
      br(),
      bs4ValueBoxOutput(width = 4, "ceT"),
      bs4ValueBoxOutput(width = 4, "ceeT"),
      bs4ValueBoxOutput(width = 4, "ceTotal")
    )
    
  })
  
  ## PRIMEIRO
  output$ceT <- renderbs4ValueBox({
    prc <- (sum(horas()[11])/sum(horas()[11:12]))*100
    
    bs4ValueBox(
      value =   tags$p(paste(
        scales::comma(sum(horas()[11]))), "(", round(prc, 2), "%)", " horas", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C4-B1"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ## Segundo
  
  output$ceeT <- renderbs4ValueBox({
    prc <- (sum(horas()[12])/sum(horas()[11:12]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma(sum(horas()[12]))),"(", round(prc, 2), "%)", " horas", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C4-B2"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ##### Total
  output$ceTotal <- renderbs4ValueBox({
    bs4ValueBox(tags$p(paste(
      scales::comma(sum(horas()[11:12])), "horas", sep = " "
    ), style = "font-size: 120%;"),
    "Total conjunto",
    
    status = "warning",
    icon = "calculator"
    )
  })
  
  
  #### INICIO RESUMO
  
  output$barChartHel <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
      
      hc_add_series(name="C4-B1",
                    data=horas_mensais()$C4.B1)%>%
      
      hc_add_series(name="C4-B2",
                    data=horas_mensais()$C4.B2)%>%
      
      hc_title(text = "horas de funcionamento das bombas") %>%
      
      hc_subtitle(text = "Conjunto 4",
                  style = list(fontWeight = "bold")) %>%
      
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5
      ) %>%
      hc_exporting(enabled = TRUE) 
  })
  # 
  output$bxplotElevacao <- renderHighchart({
    
    highchart()%>%
      
      hc_xAxis(categories=c("Sumário"))%>%
      
      hc_add_series_boxplot(name="C4-B1",x = horas()$C4.B1, outliers = F)%>%
      
      hc_add_series_boxplot(name="C4-B2",x = horas()$C4.B2,outliers = F)%>%
      
      hc_chart(type = "column")%>%
      hc_title(text = "Distribuição das horas de funcionamento") %>%
      hc_subtitle(text = "Conjunto 4") %>%
      
      hc_plotOptions(boxplot = list(
        lineWidth = 5
        
      ))
  })
  
  #### FIM RESUMO
  
  #### FIM CONJUNTO 4
  
  
  ##### INICIO CONJUNTO 5
  output$c5_ind <- renderUI({
    
    fluidRow(
      br(),
      bs4ValueBoxOutput(width = 4, "ceT_5"),
      bs4ValueBoxOutput(width = 4, "ceeT_5"),
      bs4ValueBoxOutput(width = 4, "ceTotal_5")
    )
    
  })
  
  ## Primeiro
  
  output$ceT_5 <- renderbs4ValueBox({
    prc <- (sum(horas()[13])/sum(horas()[13:14]))*100
    
    bs4ValueBox(
      value =  tags$p(paste(sum(horas()[13])), "(", round(prc, 2), "%)", " horas", style = "font-size: 120%;"),
      
      subtitle =  tags$p(strong(paste(
        "C5-B1"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ## Segundo
  
  output$ceeT_5 <- renderbs4ValueBox({
    prc <- (sum(horas()[14])/sum(horas()[13:14]))*100
    
    bs4ValueBox(
      value =   tags$p(paste(sum(horas()[14])), "(", round(prc, 2), "%)", " horas", style = "font-size: 120%;"),
      
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      subtitle =  tags$p(strong(paste(
        "C5-B2"
      )), style = "font-size: 100%;"),
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ##### Total
  output$ceTotal_5 <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma(sum(horas()[13:14])), "horas", sep = " "
      ), style = "font-size: 120%;"),
      "Total conjunto",
      
      status = "warning",
      icon = "calculator"
    )
  })
  #### FIM CONJUNTO 5
  
  ####### INICIO RESUMO
  
  output$barChartHel_5 <- renderHighchart({
  
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_xAxis(categories=format(horas_mensais()$date, format="%m-%y"))%>%
      
      hc_add_series(name="C5-B1",
                    data=horas_mensais()$C5.B1)%>%
      
      hc_add_series(name="C5-B2",
                    data=horas_mensais()$C5.B2)%>%
      
      hc_title(text = "horas de funcionamento das bombas") %>%
      
      hc_subtitle(text = "Conjunto 5",
                  style = list(fontWeight = "bold")) %>%
      
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5
      ) %>%
      hc_exporting(enabled = TRUE) 
  })
  # 
  output$bxplotElevacao_5 <- renderHighchart({
    
    highchart()%>%
      
      hc_xAxis(categories=c("Sumário"))%>%
      
      hc_add_series_boxplot(name="C5-B1",x = horas()$C5.B1, outliers = F)%>%
      
      hc_add_series_boxplot(name="C5-B2",x = horas()$C5.B2,outliers = F)%>%
      
      hc_chart(type = "column")%>%
      hc_title(text = "Distribuição das horas de funcionamento") %>%
      hc_subtitle(text = "Conjunto 5") %>%
      
      hc_plotOptions(boxplot = list(
        #fillColor = alphabet()[6],
        lineWidth = 5
        
      ))
  })
  
  ##### FIM RESUMO
  ######## FIM BOMBAGEM RESUMO ##########
  
  #############  INICIO VOLUMES DE AGUA   ##########
  
  ######## INICIO QUADRADINHO
  output$vol <- renderUI({
    if (input$lcl=="Combomune"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "volume_captado"),
        bs4ValueBoxOutput(width = 3,  "volume_aduzido"),
        bs4ValueBoxOutput(width = 3,  "volume_distribuido"),
        bs4ValueBoxOutput(width = 3, "vol_estacao"),
        bs4ValueBoxOutput(width = 3, "vol_distri_total")
      )
      
    } else if (input$lcl=="Homoine" | input$lcl=="Tomanine"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4,  "volume_captado"),
        bs4ValueBoxOutput(width = 4,  "volume_elevado"),
        bs4ValueBoxOutput(width = 4,  "volume_distribuido")
        
      )
      
    }else if (input$lcl=="Inharrime"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "volume_captado"),
        bs4ValueBoxOutput(width = 3,  "volume_aduzido"),
        bs4ValueBoxOutput(width = 3,  "volume_elevado"),
        bs4ValueBoxOutput(width = 3,  "volume_distribuido")
        
      )
      
    }else if (input$lcl=="Jangamo"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 6,  "volume_captado"),
        bs4ValueBoxOutput(width = 6,  "volume_distribuido")
      )
      
    }else if (input$lcl=="Moamba"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 3,  "volume_aduzido"),
        bs4ValueBoxOutput(width = 3,  "volume_tratado"),
        bs4ValueBoxOutput(width = 3,  "volume_distribuido"),
        bs4ValueBoxOutput(width = 3,  "volume_pessene"),
        bs4ValueBoxOutput(width = 3,  "volume_total_moamba")
        
      )
      
    }else if (input$lcl=="Mopeia"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 6,  "volume_captado"),
        bs4ValueBoxOutput(width = 6,  "volume_elevado")
      )
    }else if (input$lcl=="Ulongue"){
      fluidRow(
        br(),
        bs4ValueBoxOutput(width = 4,  "volume_captado"),
        bs4ValueBoxOutput(width = 4,  "volume_tratado"),
        bs4ValueBoxOutput(width = 4,  "volume_distribuido")
      )
    }
  })
  
  output$volume_aduzido <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma( sum(nova_base()$aduzido))
        , " m³", sep = ""), style = "font-size: 120%;"),
      "Aduzido",
      status = "primary",
      icon = icon("mountain")
    )
  })
  
  #Segundo
  output$volume_tratado <- renderbs4ValueBox({
    
    if (input$lcl == "Ulongue"){
      v <-((sum(newDb()$tratado) - sum(newDb()$captado)) / (sum(newDb()$captado))) *100
      prc <- (sum(newDb()$tratado) / sum(newDb()$captado)) * 100
      bs4ValueBox(
        value =   tags$p(paste(
          scales::comma(sum(nova_base()$tratado))
          , " m³", sep = ""), "(", round(prc, 2), " %)", style = "font-size: 120%;"
        ),
        subtitle = tags$p(strong(paste( "Tratado"
        )), style = "font-size: 100%;"),
        status = "primary",
        icon = "flask"
      )
    } else {
      v <-((sum(newDb()$tratado) - sum(newDb()$aduzido)) / (sum(newDb()$aduzido))) *100
      prc <- (sum(newDb()$tratado) / sum(newDb()$aduzido)) * 100
      bs4ValueBox(
        value =   tags$p(paste(
          scales::comma(sum(nova_base()$tratado))
          , " m³", sep = ""), "(", round(prc, 2), " %)", style = "font-size: 120%;"
        ),
        subtitle = tags$p(strong(paste( "Tratado"
        )), style = "font-size: 100%;"),
        status = "primary",
        icon = "flask"
      )
    }
  })
  ## TERCEIRO
  output$volume_distribuido <- renderbs4ValueBox({
    v <-
      ((sum(nova_base()$distribuido) - sum(nova_base()$aduzido)) / (sum(nova_base()$distribuido))) *
      100
    prc <- (sum(nova_base()$distribuido) / sum(nova_base()$aduzido)) * 100
    if (input$lcl == "Combomune" | input$lcl == "Moamba"){
      
      bs4ValueBox(
        value =   tags$p(paste(
          scales::comma( round( sum(nova_base()$distribuido)))
          , " m³", sep = ""), "(", round(prc, 2), " %)", style = "font-size: 120%;"), 
        
        subtitle =   tags$p(strong(paste( "Distribuído na sede"
        )), style = "font-size: 100%;"),
        status = "primary",
        icon = "faucet"
      )
    } else if (input$lcl == "Ulongue" | input$lcl == "Homoine" | input$lcl == "Jangamo" | input$lcl == "Tomanine"){
      v <-
        ((sum(nova_base()$distribuido) - sum(nova_base()$captado)) / (sum(nova_base()$distribuido))) *
        100
      prc <- (sum(nova_base()$distribuido) / sum(nova_base()$captado)) * 100
      
      bs4ValueBox(
        value =   tags$p(paste(
          scales::comma( round( sum(nova_base()$distribuido)))
          , " m³", sep = ""), "(", round(prc, 2), "%)", style = "font-size: 120%;"),
        subtitle =   tags$p(strong(paste(
          " Distribuído"
        )), style = "font-size: 100%;"),
        status = "primary",
        icon = "faucet"
      )
    } 
    
    else(
      
      bs4ValueBox(
        value =   tags$p(paste(
          scales::comma( round( sum(nova_base()$distribuido)))
          , " m³", sep = ""), "(", round(prc, 2), "%)", style = "font-size: 120%;"),
        subtitle =   tags$p(strong(paste(
          " Distribuído"
        )), style = "font-size: 100%;"),
        
        status = "primary",
        icon = "faucet"
        
      )
    )
  })
  
  ### Estacao
  output$vol_estacao <- renderbs4ValueBox({
    v <-
      ((sum(nova_base()$distribuido_estacao) - sum(nova_base()$aduzido)) / (sum(nova_base()$distribuido_estacao))) *
      100
    prc <- (sum(nova_base()$distribuido_estacao) / sum(nova_base()$aduzido)) * 100
    
    if (input$lcl == "Combomune"){
      bs4ValueBox(
        value =   tags$p(paste(
          scales::comma( sum(nova_base()$distribuido_estacao))
          , " m³", sep = ""), "(", round(prc, 2), " %)", style = "font-size: 120%;"),
        "Distribuído na estação",
        status = "primary",
        icon = "chart-line"
      )
    }
    
  })
  
  
  
  ######Captado #####
  output$volume_captado <- renderbs4ValueBox({
    bs4ValueBox(
      value =   tags$p(paste(
        scales::comma( sum(nova_base()$captado))
        , " m³", sep = ""), style = "font-size: 120%;"),
      "Captado",
      status = "primary",
      icon = "charging-station"
    )
  })
  
  #### ELEVADO
  output$volume_elevado <- renderbs4ValueBox({
    bs4ValueBox(
      value =   tags$p(paste(
        scales::comma(sum(nova_base()$elevado))
        , " m³", sep = ""), style = "font-size: 120%;"),
      "Elevado",
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  #### Distribuido total
  output$vol_distri_total <- renderbs4ValueBox({
    total <- scales::comma(sum(nova_base()$distribuido_estacao) + sum(nova_base()$distribuido))
    bs4ValueBox(
      value =   tags$p(paste(
        total
        , " m³", sep = ""), style = "font-size: 120%;"),
      "Total distribuído",
      status = "warning",
      icon = "faucet"
    )
  })
  
  ### Distribuido Pessene
  output$volume_pessene <- renderbs4ValueBox({
    v <-
      ((sum(newDb()$distribuido) - sum(newDb()$aduzido)) / (sum(newDb()$pessene))) *
      100
    prc <- (sum(newDb()$pessene) / sum(newDb()$aduzido)) * 100
    total <- scales::comma(sum(nova_base()$pessene))
    bs4ValueBox(
      value =   tags$p(paste(
        total
        , " m³", sep = ""), style = "font-size: 120%;", "(",  round(prc, 2), "%)"),
      subtitle = tags$p(strong(paste("(Distribuído em Pessene)"
      )), style = "font-size: 100%;"),
      status = "primary",
      icon = "faucet"
    )
  })
  
  ##### Distribuido total Moamba
  
  output$volume_total_moamba <- renderbs4ValueBox({
    total <- scales::comma(sum(nova_base()$pessene) + sum(nova_base()$distribuido))
    bs4ValueBox(
      value =   tags$p(paste(
        total
        , " m³", sep = ""), style = "font-size: 120%;"),
      "Total Distribuído",
      status = "warning",
      icon = "faucet"
    )
  })
  
  ##### FIM QUADRADINHOS 
  
  
  #### INICIO PERDAS D FACTURADOS ###
  
  output$relacaoPF <- renderUI({
    fluidRow(
      bs4Box(
        title = "Relação entre perdas e facturação",
        highchartOutput("desempenho"),
        width = 12,
        height = "100%",
        solidHeader = F,
        status = "warning",
        collapsible = T,
        collapsed = T
      )
    )
  })
  
  ##### INICIO RESUMO
  output$resumoVol <- renderUI({
    
    fluidRow(
      br(),
      br(),
      bs4TabCard(
        id = "res_vol",
        width = 12,
        collapsible = FALSE,
        closable = FALSE,
        title = "",
        bs4TabPanel(
          tabName =  "Variação",
          highchartOutput("table")
        ),
        bs4TabPanel(
          tabName =  "Acumulado",
          highchartOutput("barPlot")
        )
      )
    )
  })
  
  # # GRAFICO DE VARICAO DE VOLUME DE AGUA
  output$table <- renderHighchart({
    if (input$lcl == "Combomune"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuído",
          data = nova_base()$tratado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Aduzido",
          data = nova_base()$aduzido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuído na estação",
          data = nova_base()$distribuido_estacao,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
    }else if (input$lcl == "Homoine"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Elevado",
          data = nova_base()$elevado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Aduzido",
          data = nova_base()$aduzido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuído",
          data = nova_base()$distribuido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    }else if (input$lcl == "Inharrime"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Aduzido",
          data = nova_base()$aduzido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Elevado",
          data = nova_base()$elevado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuído",
          data = nova_base()$distribuido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    }else if (input$lcl == "Jangamo"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuído",
          data = nova_base()$distribuido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    }
    else if (input$lcl == "Moamba"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Aduzido",
          data = nova_base()$aduzido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Tratado",
          data = nova_base()$tratado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuido para vila",
          data = nova_base()$distribuido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Distribuido para Pessene",
          data = nova_base()$pessene,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    } else if (input$lcl == "Mopeia"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Elevado",
          data = nova_base()$elevado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    } else if (input$lcl == "Tomanine"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Elevado",
          data = nova_base()$elevado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "Distribuído",
          data = nova_base()$distribuido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    }
    
    else if (input$lcl == "Ulongue"){
      highchart() %>%
        
        hc_title(text = "Variação do volume de água no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Volumes")
          
        ) %>%
        
        hc_add_series(
          name = "Captado",
          data = nova_base()$captado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Tratado",
          data = nova_base()$tratado,
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "Distribuído",
          data = nova_base()$distribuido,
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
    }
    
  })
  
  ######################## INICIDO BARPLOT ##################
  output$barPlot <- renderHighchart({
    if (input$lcl == "Combomune"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Captado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Tratado",
                      data=nova_base()$tratado)%>%
        
        hc_add_series(name="Distribuido",
                      data=nova_base()$distribuido)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Combomune",
                    style = list(fontWeight = "bold")) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }else  if (input$lcl == "Homoine"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Captado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Elevado",
                      data=nova_base()$elevado)%>%
        
        hc_add_series(name="Aduzido",
                      data=nova_base()$aduzido)%>%
        
        hc_add_series(name="Distribuido",
                      data=nova_base()$distribuido)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Homoine",
                    style = list(fontWeight = "bold")) %>%
        
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    } else  if (input$lcl == "Inharrime"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Captado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Elevado",
                      data=nova_base()$elevado)%>%
        
        hc_add_series(name="Aduzido",
                      data=nova_base()$aduzido)%>%
        
        hc_add_series(name="Distribuido",
                      data=nova_base()$distribuido)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Inharrime",
                    style = list(fontWeight = "bold")) %>%
        
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }
    else  if (input$lcl == "Jangamo"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Captado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Distribuido",
                      data=nova_base()$distribuido)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Jangamo",
                    style = list(fontWeight = "bold")) %>%
        
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }
    else  if (input$lcl == "Moamba"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Aduzido",
                      data=nova_base()$aduzido)%>%
        
        hc_add_series(name="Tratado",
                      data=nova_base()$tratado)%>%
        
        hc_add_series(name="Distribuido para a vila",
                      data=nova_base()$distribuido)%>%
        
        hc_add_series(name="Distribuido para Pessene",
                      data=nova_base()$pessene)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Moamba",
                    style = list(fontWeight = "bold")) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }
    else  if (input$lcl == "Mopeia"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Capatado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Elevado",
                      data=nova_base()$elevado)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Mopeia",
                    style = list(fontWeight = "bold")) %>%
        
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }
    else  if (input$lcl == "Tomanine"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Capatado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Elevado",
                      data=nova_base()$elevado)%>%
        
        hc_add_series(name="Distribuido",
                      data=nova_base()$distribuido)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Tomanine",
                    style = list(fontWeight = "bold")) %>%
        
        
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }
    
    else  if (input$lcl == "Ulongue"){
      
      highchart() %>%
        
        hc_chart(type="column")%>%
        
        hc_xAxis(categories=format(nova_base()$date, format="%m-%y"))%>%
        
        hc_add_series(name="Capatado",
                      data=nova_base()$captado)%>%
        
        hc_add_series(name="Tratado",
                      data=nova_base()$tratado)%>%
        
        hc_add_series(name="Distribuido",
                      data=nova_base()$distribuido)%>%
        
        hc_title(text = "Volumes de água") %>%
        
        hc_subtitle(text = "Ulongue",
                    style = list(fontWeight = "bold")) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 5
        ) %>%
        hc_exporting(enabled = TRUE) 
    }
  })
  
  output$bxplot <- renderHighchart({
    
    hcboxplot(var = dados_mensais()$tipo, x = dados_mensais()$volume) %>%
      hc_chart(type = "column")%>%
      
      hc_plotOptions(boxplot = list(
        fillColor = '#E4FABD',
        lineWidth = 2,
        color = 'black'
      ))  %>%
      hc_colors(c("#203d7d","#a0a0ed","#203d7e","#a0a0ad"))
    
  })
  
  output$perdas_pie <- renderHighchart({
    
    if(input$lcl=="Moamba"){
      
      highchart() %>%
        
        hc_title(
          text = "RELACAO ENTRE AS PERDAS FISICAS E ECONOMICAS",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        # hc_subtitle(
        #   text = paste("Total de perdas:",scales::comma(sum(facturado()$perdas_totais)) ,sep = "\t"),
        #   align = "left",
        #   style = list(color = "#2b908f", fontWeight = "bold")
        # ) %>%
        
        hc_chart(type = "pie") %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(vol_mensal()$aduzido-vol_mensal()$distribuido+vol_mensal()$pessene),
            name = "Fisicas  "
          ),
          list(
            y = sum(vol_mensal()$distribuido+vol_mensal()$pessene)-sum(facturado()$Facturado),
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
    }else if(input$lcl=="Mopeia"){
      
      highchart() %>%
        
        hc_title(
          text = "RELACAO ENTRE AS PERDAS FISICAS E ECONOMICAS",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        # hc_subtitle(
        #   text = paste("Total de perdas:",scales::comma(sum(facturado()$perdas_totais)) ,sep = "\t"),
        #   align = "left",
        #   style = list(color = "#2b908f", fontWeight = "bold")
        # ) %>%
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(vol_mensal()$captado-vol_mensal()$elevado) ,
            name = "Fisicas  "
          ),
          list(
            y = sum(vol_mensal()$elevado-facturado()$Facturado) ,
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
      
    }else if(input$lcl=="Combomune"){
      
      
      highchart() %>%
        
        hc_title(
          text = "RELACAO ENTRE AS PERDAS FISICAS E ECONOMICAS",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        # hc_subtitle(
        #   text = paste("Total de perdas:",scales::comma(sum(facturado()$perdas_totais)) ,sep = "\t"),
        #   align = "left",
        #   style = list(color = "#2b908f", fontWeight = "bold")
        # ) %>%
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(vol_mensal()$captado-(vol_mensal()$distribuido+vol_mensal()$distribuido_estacao)) ,
            name = "Fisicas  "
          ),
          list(
            y = sum(vol_mensal()$distribuido+vol_mensal()$distribuido_estacao)-facturado()$Facturado,
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
      
    }else{
      
      highchart() %>%
        
        hc_title(
          text = "RELACAO ENTRE AS PERDAS FISICAS E ECONOMICAS",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
      
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(vol_mensal()$captado-vol_mensal()$distribuido) ,
            name = "Fisicas  "
          ),
          list(
            y = sum(vol_mensal()$distribuido-facturado()$Facturado) ,
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
      
      
    }
    
  })
  
  
  ###################### FIM BARPLOT ########################
  
  
  
  output$sumario <- function() {
    med <- c(
      mean(nova_base()$aduzido),
      mean(nova_base()$tratado),
      #mean(nova_base()$moamba),
      mean(nova_base()$pessene),
      mean(nova_base()$distribuido)
    )
    
    desvio <- c(
      sd(nova_base()$aduzido),
      sd(nova_base()$tratado),
      #sd(nova_base()$moamba),
      sd(nova_base()$pessene),
      sd(nova_base()$distribuido)
    )
    
    mini <- c(
      min(nova_base()$aduzido),
      min(nova_base()$tratado),
      #min(nova_base()$moamba),
      min(nova_base()$pessene),
      min(nova_base()$distribuido)
    )
    
    maxi <- c(
      max(nova_base()$aduzido),
      max(nova_base()$tratado),
      #max(nova_base()$moamba),
      max(nova_base()$pessene),
      max(nova_base()$distribuido)
    )
    
    total <- c(
      sum(nova_base()$aduzido),
      sum(nova_base()$tratado),
      #sum(nova_base()$moamba),
      sum(nova_base()$pessene),
      sum(nova_base()$distribuido)
    )
    
    col <-
      c(
        "Aduzido",
        "Tratado ",
        "Distribuido", input$lcl,
        "Distribuido Pessene",
        "Distribuido total"
      )
    
    sumario <- cbind(col, mini, med, maxi, total)
    colnames(sumario) <-
      c("Area", "Minimo", "Media", "Maximo", "Total")
    
    sumario <- as.data.frame(sumario)
    
    sumario$Area <- unfactor(sumario$Area)
    sumario$Minimo <- unfactor(sumario$Minimo)
    sumario$Media <- unfactor(sumario$Media)
    sumario$Maximo <- round(unfactor(sumario$Maximo), 0)
    sumario$Total <- unfactor(sumario$Total)
    
    sumario[, 2:5] <- round(sumario[, 2:5], 2)
    sumario$Minimo <- scales::comma(sumario$Minimo)
    sumario$Media <- scales::comma(sumario$Media)
    sumario$Maximo <- scales::comma(sumario$Maximo)
    sumario$Total <- scales::comma(sumario$Total)
    
    sumario %>%
      kable() %>%
      kable_styling(
        bootstrap_options = c("condensed", "responsive", "bordered", "hover"),
        font_size = 16,
        full_width = T,
        fixed_thead = T
      )
  }
  
  
  #### FIM RESUMO
  
  ################   INICIO CAUDAL ################
  output$caudal_ <- renderUI({
    
    if (input$lcl=="Combomune"){
      fluidRow(
        br(),
        valueBoxOutput(width = 6,  "caudal_captacao"),
        valueBoxOutput(width = 6,  "caudal_aducao"),
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
      
    } else if (input$lcl=="Homoine" | input$lcl=="Tomanine"){
      
      fluidRow(
        br(),
        valueBoxOutput(width = 6,  "caudal_captacao"),
        valueBoxOutput(width = 6,  "caudal_elevacao"),
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
    }else if (input$lcl=="Inharrime"){
      fluidRow(
        br(),
        valueBoxOutput(width = 4,  "caudal_captacao"),
        valueBoxOutput(width = 4,  "caudal_aducao"),
        valueBoxOutput(width = 4,  "caudal_elevacao"),
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
      
    }else if (input$lcl=="Jangamo"){
      
      fluidRow(
        br(),
        valueBoxOutput(width = 12,  "caudal_captacao"),
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
    }else if (input$lcl=="Moamba"){
      fluidRow(
        br(),
        valueBoxOutput(width = 6,  "caudal_aducao"),
        valueBoxOutput(width = 6,  "caudal_tratamento"),
        
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
    }else if (input$lcl=="Mopeia"){
      
      fluidRow(
        br(),
        valueBoxOutput(width = 6,  "caudal_captacao"),
        valueBoxOutput(width = 6, "caudal_elevacao"),
        
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
    }else if (input$lcl=="Ulongue"){
      fluidRow(
        br(),
        valueBoxOutput(width = 6,  "caudal_captacao"),
        valueBoxOutput(width = 6,  "caudal_tratamento"),
        
        bs4Box(
          width = 12,
          status = NULL,
          collapsible = F,
          highchartOutput("caudal")
          
          
        )
      )
      
    }
    
  })
  
  output$caudal_captacao <- renderbs4ValueBox({
    
    
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma(round(sum(nova_base()$captado)/sum(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2))
        , " m³/h", sep = ""), style = "font-size: 120%;"),
      "Caudal de Captação",
      status = "primary",
      icon = "chart-line")
    
    
  })
  
  output$caudal_tratamento <- renderbs4ValueBox({
    
    
    bs4ValueBox(
      value =  tags$p(paste(
        scales::comma(round(sum(nova_base()$tratado)/sum(horas_c()$C3.B1+horas_c()$C3.B2+horas_c()$C3.B3),2))
        , " m³/h", sep = ""), style = "font-size: 120%;"),
      "Caudal de Tratamento",
      status = "primary",
      icon = "chart-line"
    )
    
    
  })
  
  output$caudal_elevacao <- renderbs4ValueBox({
    
    
    bs4ValueBox(
      value =  tags$p(paste(
        round(sum(nova_base()$elevado)/sum(horas_c()$C4.B1+horas_c()$C4.B2,2))
        , " m³/h", sep = ""), style = "font-size: 120%;"),
      "Caudal de Elevação",
      status = "primary",
      icon = "chart-line"
    )
    
    
  })
  
  output$caudal_aducao <- renderbs4ValueBox({
    
    
    bs4ValueBox(
      value =  tags$p(paste(
        round(sum(nova_base()$aduzido)/sum(horas_c()$C2.B1+horas_c()$C2.B2+horas_c()$C2.B3),2)
        , " m³/h", sep = ""), style = "font-size: 120%;"),
      "Caudal de Adução",
      status = "primary",
      icon = "chart-line"
    )
    
    
  })
  
  
  output$caudal <- renderHighchart({
    
    if (input$lcl == "Inharrime"){
      
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Captação",
          data = round((nova_base()$captado)/(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Adução",
          data =round((nova_base()$aduzido)/(horas_c()$C2.B1+horas_c()$C2.B2+horas_c()$C2.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Tratamento",
          data = round((nova_base()$tratado)/(horas_c()$C3.B1+horas_c()$C3.B2+horas_c()$C3.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "Elevação",
          data =round((nova_base()$elevado)/(horas_c()$C4.B1+horas_c()$C4.B2),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
    }else if(input$lcl=="Homoine" | input$lcl=="Tomanine"){
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Captação",
          data = round((nova_base()$captado)/(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Elevação",
          data =round((nova_base()$elevado)/(horas_c()$C4.B1+horas_c()$C4.B2),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
    }else if(input$lcl=="Combomune"){
      
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Captação",
          data = round((nova_base()$captado)/(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Adução",
          data =round((nova_base()$aduzido)/(horas_c()$C2.B1+horas_c()$C2.B2+horas_c()$C2.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
      
    }else if(input$lcl=="Jangamo"){
      
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Captação",
          data = round((nova_base()$captado)/(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
      
    }else if(input$lcl=="Moamba"){
      
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Adução",
          data =round((nova_base()$aduzido)/(horas_c()$C2.B1+horas_c()$C2.B2+horas_c()$C2.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Tratamento",
          data = round((nova_base()$tratado)/(horas_c()$C3.B1+horas_c()$C3.B2+horas_c()$C3.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
    }else if (input$lcl=="Mopeia"){
      
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Captação",
          data = round((nova_base()$captado)/(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Elevação",
          data =round((nova_base()$elevado)/(horas_c()$C4.B1+horas_c()$C4.B2),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
    }else if (input$lcl=="Ulongue"){
      
      highchart() %>%
        
        hc_title(text = "Variação do caudal no sistema") %>%
        
        hc_xAxis(
          categories = nova_base()$date,
          title = list(text = "Caudal")
          
        ) %>%
        
        hc_add_series(
          name = "Captação",
          data = round((nova_base()$captado)/(horas_c()$C1.B1+horas_c()$C1.B2+horas_c()$C1.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Tratamento",
          data = round((nova_base()$tratado)/(horas_c()$C3.B1+horas_c()$C3.B2+horas_c()$C3.B3),2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = TRUE,
          borderWidth = 3
        ) %>%
        hc_exporting(enabled = TRUE)
      
      
    }
    
    
  })
  
  ############## FIM CAUDAL ######################
  
  ############ INICIO FACTURACAO E PERDAS #####
  output$fact_card <- renderUI({
    fluidRow(
      br(),
      br(),
      bs4InfoBoxOutput("faturado_box", width = 4),
      bs4InfoBoxOutput("perdas_eco", width = 4),
      bs4InfoBoxOutput("perdas_fisicas", width = 4)
    )
  })
  
  output$factPerdas <- renderUI({
    
    fluidRow(
      bs4Box(
        highchartOutput("fact_plot"),
        width = 9,
        status = NULL,
        collapsible = F
      ),
      bs4Box(
        width = 3,
        height = "100%",
        solidHeader = F,
        status = NULL,
        collapsible = T
        ,
        highchartOutput("perdas_pie")
      ))
  })
  
  output$faturado_box = renderbs4ValueBox({
    
    
    if(input$lcl=="Moamba"){
      
      prc = sum(facturado()$Facturado)/sum(nova_base()$distribuido+nova_base()$pessene)
      prc = prc*100
      
    }else if(input$lcl=="Mopeia"){
      
      prc = (sum(facturado()$Facturado)/sum(nova_base()$elevado))*100
      
    }else if(input$lcl=="Combomune"){
      
      prc = (sum(facturado()$Facturado)/sum(nova_base()$distribuido+nova_base()$distribuido_estacao))*100
      
    }else{
      
      prc = (sum(facturado()$Facturado)/sum(nova_base()$distribuido))*100
      
    }
    
    bs4ValueBox(
      value = tags$p(scales::comma(sum(facturado()$Facturado)), " m³", "(Facturado)", style="font-size:120%;"),
      subtitle =  tags$p(paste(round(prc,1),"% do volume distribuído")) , 
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      
      icon = icon("stopwatch"),
      status = "teal"
    )
    
  })
  
  output$perdas_fisicas = renderbs4ValueBox({
    
    
    if(input$lcl=="Moamba"){
      
      perdas = sum(nova_base()$aduzido)-sum(nova_base()$distribuido+nova_base()$pessene)
      prc = (perdas/sum(nova_base()$distribuido+nova_base()$pessene))*100
      
    }else if(input$lcl=="Mopeia"){
      
      perdas = sum(nova_base()$captado)-sum(nova_base()$elevado)
      prc = (perdas/sum(nova_base()$distribuido+nova_base()$pessene))*100
      
    }else if(input$lcl=="Combomune"){
      
      perdas = sum(nova_base()$captado) -sum(nova_base()$distribuido+nova_base()$distribuido_estacao)
      prc = (perdas/sum(nova_base()$distribuido+nova_base()$distribuido_estacao))*100
      
    }else{
      
      perdas = sum(nova_base()$captado-nova_base()$distribuido)
      prc = (perdas/sum(nova_base()$distribuido))*100
      
    }
    
    bs4ValueBox(
      value = tags$p(scales::comma(round(perdas,1)), " m³", "(Perdas físicas)", style="font-size:120%;"),
      subtitle =  tags$p(paste(round(prc,1),"% do volume distribuído")), 
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      
      icon = icon("stopwatch"),
      status = "warning"
    )
    
  })
  
  output$perdas_eco = renderbs4ValueBox({
    
    
    if(input$lcl=="Moamba"){
      
      perdas = sum(nova_base()$distribuido+nova_base()$pessene)-sum(facturado()$Facturado)
      prc = (perdas/sum(nova_base()$distribuido+nova_base()$pessene))*100
      
    }else if(input$lcl=="Mopeia"){
      
      perdas = sum(nova_base()$elevado-facturado()$Facturado)
      prc = (perdas/sum(nova_base()$distribuido+nova_base()$pessene))*100
      
    }else if(input$lcl=="Combomune"){
      
      perdas = sum((nova_base()$distribuido+nova_base()$distribuido_estacao)-facturado()$Facturado)
      prc = (perdas/sum(nova_base()$distribuido+nova_base()$distribuido_estacao))*100
      
    }else{
      
      perdas = sum(nova_base()$distribuido-facturado()$Facturado)
      prc = (perdas/sum(nova_base()$distribuido))*100
      
    }
    
    bs4ValueBox(
      value = tags$p(scales::comma(round(perdas,1)), " m³","(Perdas económicas)", style="font-size:120%;"),
      subtitle =  tags$p(paste(round(prc,1),"% do volume distribuído")) , 
      # paste(sum(horas()[horas()$Conjunto == "C3-B1",]$Duracao), "horas", sep = " "),
      
      icon = icon("stopwatch"),
      status = "info"
    )
    
    
  })
  
  
  output$perdas_pie <- renderHighchart({
    
    if(input$lcl=="Moamba"){
      
      highchart() %>%
        
        hc_title(
          text = "Relação entre perdas físicas e económicas",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(nova_base()$aduzido-nova_base()$distribuido+nova_base()$pessene),
            name = "Fisicas  "
          ),
          list(
            y = sum(nova_base()$distribuido+nova_base()$pessene)-sum(facturado()$Facturado),
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
    }else if(input$lcl=="Mopeia"){
      
      highchart() %>%
        
        hc_title(
          text = "RELACAO ENTRE AS PERDAS FISICAS E ECONOMICAS",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        # hc_subtitle(
        #   text = paste("Total de perdas:",scales::comma(sum(facturado()$perdas_totais)) ,sep = "\t"),
        #   align = "left",
        #   style = list(color = "#2b908f", fontWeight = "bold")
        # ) %>%
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(nova_base()$captado-nova_base()$elevado) ,
            name = "Fisicas  "
          ),
          list(
            y = sum(nova_base()$elevado-facturado()$Facturado) ,
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
      
    }else if(input$lcl=="Combomune"){
      
      
      highchart() %>%
        
        hc_title(
          text = "Relação entre perdas físicas e económicas",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(nova_base()$captado-(nova_base()$distribuido+nova_base()$distribuido_estacao)) ,
            name = "Fisicas  "
          ),
          list(
            y = sum(nova_base()$distribuido+nova_base()$distribuido_estacao)-facturado()$Facturado,
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
      
    }else{
      
      highchart() %>%
        
        hc_title(
          text = "RELACAO ENTRE AS PERDAS FISICAS E ECONOMICAS",
          margin = 20,
          # align = "left",
          style = list(color = "#2b908f", useHTML = TRUE)
        ) %>%
        
        
        hc_chart(type = "pie") %>%
        
        # hc_add_theme(hc_theme_google()) %>%
        
        hc_plotOptions(series = list(showInLegend = TRUE)) %>%
        
        hc_add_series(data = list(
          list(
            y = sum(nova_base()$captado-nova_base()$distribuido) ,
            name = "Fisicas  "
          ),
          list(
            y = sum(nova_base()$distribuido-facturado()$Facturado) ,
            name = "Economicas  "
          )
        ),
        dataLabels = list(enabled = F)) %>%
        
        
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 5,
          sort = TRUE,
          shared = TRUE,
          table = TRUE,
          pointFormat = paste(
            '<b>{point.percentage:.1f}%</b><br>Perdas: {point.y}</br>'
          )
        )
      
      
    }
    
  })
  
  output$fact_plot <- renderHighchart({
    
    if(input$lcl=="Moamba"){
      
      perdas_eco = (nova_base()$distribuido+nova_base()$pessene)-facturado()$Facturado
      perdas_fis = nova_base()$aduzido-(nova_base()$distribuido+nova_base()$pessene)
      
    }else if(input$lcl=="Mopeia"){
      
      perdas_eco = nova_base()$elevado-facturado()$Facturado
      perdas_fis = nova_base()$captado-nova_base()$elevado
      
    }else if(input$lcl=="Combomune"){
      
      perdas_eco = (nova_base()$distribuido+nova_base()$distribuido_estacao)-facturado()$Facturado
      perdas_fis = nova_base()$captado-(nova_base()$distribuido+nova_base()$distribuido_estacao)
      
    }else{
      
      perdas_eco = nova_base()$distribuido-facturado()$Facturado
      perdas_fis = nova_base()$captado-nova_base()$distribuido
    }
    
    highchart() %>%
      
      hc_title(text = "Volume facturado e perdas no sistema") %>%
      
      hc_xAxis(
        categories = nova_base()$date,
        title = list(text = "Caudal")
        
      ) %>%
      
      hc_add_series(
        name = "Facturado",
        data = round(facturado()$Facturado,1),
        dataLabels = list(enabled = TRUE)
      ) %>%
      
      hc_add_series(
        name = "Perdas Económicas",
        data =round(perdas_eco,1),
        dataLabels = list(enabled = TRUE)
      ) %>%
      
      hc_add_series(
        name = "Perdas Fisícas",
        data = round(perdas_fis,2),
        dataLabels = list(enabled = TRUE)
      ) %>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 3
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  
  ####### FIM FACTURACAO E PERDAS #############
  
  ############# FIM VOLUMES DE AGUA  ##########
  
  
  #### INICIO QUALIDADE ######
  
  output$qualidade_agua <- renderUI({
    fluidRow(
      br(),
      br(),
      bs4TabCard(
        id = "tabcard_Qual",
        width = 9,
        collapsible = TRUE,
        closable = FALSE,
        title = NULL,
        bs4TabPanel(
          tabName = "pH",
          active = TRUE,
          highchartOutput("var_ph")
          
        ),
        bs4TabPanel(
          tabName =  "Turvação",
          active = FALSE,
          highchartOutput("var_ntu")
        ),
        bs4TabPanel(
          tabName =  "Clr Residual",
          active = FALSE,
          highchartOutput("var_clr")
        )
        
      ),
      bs4Box(
        width = 3,
        height = 500,
        solidHeader = F,
        collapsible = F,
        highchartOutput("conf")
        
      ),
      bs4Box(
        width = 12,
        solidHeader = F,
        collapsible = F,
        dataTableOutput("intervalos")
        
      )
    )
  })
  
  output$var_ph <- renderHighchart({
    if(input$lcl == "Combomune"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação",
          data = round(quality_agua()$pH_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro Distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Depósito elevado",
          data = round(quality_agua()$phDeposito_elevado, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
      
    } else if(input$lcl == "Homoine"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação",
          data = round(quality_agua()$pH_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH ETA",
          data = round(quality_agua()$phETA, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
      
    } else if(input$lcl == "Inharrime"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação Furo 1",
          data = round(quality_agua()$pCaptacaoF1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 2",
          data = round(quality_agua()$phCaptacaoF2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 3",
          data = round(quality_agua()$phCaptacaoF3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 4",
          data = round(quality_agua()$phCaptacaoF4, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if(input$lcl == "Moamba"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação",
          data = round(quality_agua()$pH_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if(input$lcl == "Jangamo"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação Furo 1",
          data = round(quality_agua()$pCaptacaoF1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 2",
          data = round(quality_agua()$phCaptacaoF2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 3",
          data = round(quality_agua()$phCaptacaoF3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Captação Furo 4",
          data = round(quality_agua()$phCaptacaoF4, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if(input$lcl == "Mopeia"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação",
          data = round(quality_agua()$pH_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } 
    else if(input$lcl == "Tomanine"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Captação Furo 1",
          data = round(quality_agua()$pCaptacaoF1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 2",
          data = round(quality_agua()$phCaptacaoF2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Captação Furo 3",
          data = round(quality_agua()$phCaptacaoF3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Captação Furo 4",
          data = round(quality_agua()$phCaptacaoF4, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Centro distribuidor",
          data = round(quality_agua()$pH_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
      
    }  else if(input$lcl == "Ulongue"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de pH") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "pH Casa do floculador",
          data = round(quality_agua()$pH_CF, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Casa das bombas",
          data = round(quality_agua()$pH_CB, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Depósito elevado",
          data = round(quality_agua()$phDeposito_elevado, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 1",
          data = round(quality_agua()$pH_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "pH Medição 2",
          data = round(quality_agua()$pH_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "pH Medição 3",
          data = round(quality_agua()$pH_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
      
    }
    
    
  })
  
  output$var_ntu <- renderHighchart({
    if (input$lcl == "Combomune"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de NTU-Turvação") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Turvacao Captação",
          data = round(quality_agua()$Turvacao_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvação Centro Distribuidor",
          data = round(quality_agua()$Turvacao_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "Turvação Depósito elevado",
          data = round(quality_agua()$turvacao_deposito_elevado, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 1",
          data = round(quality_agua()$Turvacao_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 2",
          data = round(quality_agua()$Turvacao_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    }else if (input$lcl == "Homoine"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de NTU-Turvação") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Turvacao Captação",
          data = round(quality_agua()$Turvacao_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvação ETA",
          data = round(quality_agua()$turvacaoETA, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        hc_add_series(
          name = "Turvação Centro Distribuidor",
          data = round(quality_agua()$Turvacao_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 1",
          data = round(quality_agua()$Turvacao_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 2",
          data = round(quality_agua()$Turvacao_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 3",
          data = round(quality_agua()$Turvacao_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if (input$lcl == "Moamba"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de NTU-Turvação") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Turvacao Captação",
          data = round(quality_agua()$Turvacao_Captacao, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvação Centro Distribuidor",
          data = round(quality_agua()$Turvacao_CD, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 1",
          data = round(quality_agua()$Turvacao_RD1, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 2",
          data = round(quality_agua()$Turvacao_RD2, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Turvacao Medicao 3",
          data = round(quality_agua()$Turvacao_RD3, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } 
  })
  
  
  output$var_clr <- renderHighchart({
    if (input$lcl == "Combomune"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Clr Depósito elevado",
          data = round(quality_agua()$`cloro residual_DE`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if (input$lcl == "Homoine"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr ETA",
          data = round(quality_agua()$`Cloro residual_ETA`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    }
    else if (input$lcl == "Inharrime"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    }else if (input$lcl == "Jangamo"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if (input$lcl == "Moamba"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    } else if (input$lcl == "Mopeia"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    }else if (input$lcl == "Tomanine"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Centro distribuidor",
          data = round(quality_agua()$`Cloro residual_CD`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 1",
          data = round(quality_agua()$`Cloro residual_RD1`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    }else if (input$lcl == "Ulongue"){
      highchart() %>%
        
        hc_title(text = "Variação dos níveis de cloro residual") %>%
        
        hc_xAxis(categories = format(quality_agua()$date, format = "%d/%b/%y")) %>%
        
        hc_add_series(
          name = "Clr Casa das bombas",
          data = round(quality_agua()$`cloro residual_CB`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Clr Depósito elevado",
          data = round(quality_agua()$`cloro residual_DE`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 2",
          data = round(quality_agua()$`Cloro residual_RD2`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_add_series(
          name = "Cloro Residual Medicao 3",
          data = round(quality_agua()$`Cloro residual_RD3`, 2),
          dataLabels = list(enabled = TRUE)
        ) %>%
        
        hc_tooltip(
          crosshairs = TRUE,
          backgroundColor = "#FCFFC5",
          shared = FALSE,
          borderWidth = 5
        ) %>%
        
        hc_exporting(enabled = TRUE)
    }
  })
  
  output$conf <- renderHighchart({
    highchart() %>%
      
      hc_title(
        text = "CONFORMIDADE CL-R",
        margin = 20,
        #  align = "left",
        style = list(color = "#2b908f", useHTML = TRUE)
      ) %>%
      
      hc_chart(type = "pie") %>%
      
      hc_plotOptions(series = list(showInLegend = TRUE)) %>%
      
      hc_add_series(data = list(
        list(y = aux_tbl()[1], name = "Testes Conformes:  "),
        list(y = aux_tbl()[2], name = "Testes não conformes:  ")
      ),
      dataLabels = list(enabled = F)) %>%
      
      
      hc_tooltip(
        crosshairs = TRUE,
        borderWidth = 5,
        sort = TRUE,
        shared = TRUE,
        table = TRUE,
        pointFormat = paste(
          '<b>{point.percentage:.1f}%</b><br>Nº de testes: {point.y}</br>'
        )
      )
  })
  
  
  output$intervalos <- renderDataTable({
    ph_min <- min(
      quality()$pH_Captacao,
      quality()$pH_CD,
      quality()$pH_RD1,
      quality()$pH_RD2,
      quality()$pH_RD3,
      quality()$phETA,
      quality()$pCaptacaoF1,
      quality()$pCaptacaoF2,
      quality()$pCaptacaoF3,
      quality()$pCaptacaoF4,
      quality()$phCaptacaoCE1,
      quality()$phCaptacaoCE2,
      quality()$phCaptacaoCE3,
      quality()$phCaptacaoCE4,
      quality()$pH_CF,
      quality()$pH_CB
    )
    clor_min <-
      min(
        quality()$`Cloro residual_CD`,
        quality()$`Cloro residual_RD1`,
        quality()$`Cloro residual_RD2`,
        quality()$`Cloro residual_RD3`,
        quality()$`cloro residual_DE`,
        quality()$`Cloro residual_ETA`,
        quality()$`Cloro residual_CE`,
        quality()$`Cloro residual_DE3`,
        quality()$`cloro residual_CB`
      )
    turv_min <- min(
      quality()$Turvacao_Captacao,
      quality()$Turvacao_CD,
      quality()$Turvacao_RD1,
      quality()$Turvacao_RD2,
      quality()$Turvacao_RD3,
      quality()$turvacao_deposito_elevado,
      quality()$turvacaoETA,
      quality()$turvacao_CD,
      quality()$turvacao_CD
    )
    
    ce_min <- min(
      quality()$ceCaptacaoF1,
      quality()$ceCaptacaoF2,
      quality()$ceCaptacaoF3,
      quality()$ceCaptacaoF4,
      quality()$CECD,
      quality()$CE_RD1,
      quality()$CE_RD2,
      quality()$CE_RD3
      
    )
    
    cd_min <- min(
      quality()$CDRD1,
      quality()$CDRD2,
      quality()$CDRD3
    )
    
    ph_max <- max(
      quality()$pH_Captacao,
      quality()$pH_CD,
      quality()$pH_RD1,
      quality()$pH_RD2,
      quality()$pH_RD3,
      quality()$phETA,
      quality()$pCaptacaoF1,
      quality()$pCaptacaoF2,
      quality()$pCaptacaoF3,
      quality()$pCaptacaoF4,
      quality()$phCaptacaoCE1,
      quality()$phCaptacaoCE2,
      quality()$phCaptacaoCE3,
      quality()$phCaptacaoCE4,
      quality()$pH_CF,
      quality()$pH_CB
    )
    clor_max <-
      max(
        quality()$`Cloro residual_CD`,
        quality()$`Cloro residual_RD1`,
        quality()$`Cloro residual_RD2`,
        quality()$`Cloro residual_RD3`,
        quality()$`cloro residual_DE`,
        quality()$`Cloro residual_ETA`,
        quality()$`Cloro residual_CE`,
        quality()$`Cloro residual_DE3`,
        quality()$`cloro residual_CB`
      )
    turv_max <- max(
      quality()$Turvacao_Captacao,
      quality()$Turvacao_CD,
      quality()$Turvacao_RD1,
      quality()$Turvacao_RD2,
      quality()$Turvacao_RD3,
      quality()$turvacao_deposito_elevado,
      quality()$turvacaoETA,
      quality()$turvacao_CD,
      quality()$turvacao_CD
    )
    ce_max <- max(
      quality()$ceCaptacaoF1,
      quality()$ceCaptacaoF2,
      quality()$ceCaptacaoF3,
      quality()$ceCaptacaoF4,
      quality()$CECD,
      quality()$CE_RD1,
      quality()$CE_RD2,
      quality()$CE_RD3
    )
    cd_max <- max(
      quality()$CDRD1,
      quality()$CDRD2,
      quality()$CDRD3
    )
    
    Minimo <- c(ph_min, clor_min, turv_min)
    Maximo <- c(ph_max, clor_max, turv_max)
    
    Mínimo <- round(Minimo, 2)
    Máximo <- round(Maximo, 2)
    
    int <- rbind(Mínimo, Máximo)
    colnames(int) <-
      c("pH(-)", "Clr (mg/l) ", "Turvacao(NTU)")
    
    
    int <- as.data.frame(int)
    datatable(int,     options = list(
      columnDefs = list(list(searchable = T, targets = 1)),
      searchHighlight = TRUE,
      pageLength = 15,
      scrollX = TRUE,
      dom = 't',
      buttons =
        list(
          list(
            extend = 'collection',
            buttons = c('csv', 'pdf','json','xlsx'),
            text = 'Baixar'
          )
        )
    ))
  })

  
  
  #### FIM QUALIDADE ####
  
  
  ###### INICIO QUIMICOS E REAGENTES #######
  
  
  output$sa <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(
        round(sum(quimicos_tbl()[3]),2), "kg", sep = " "), style = "font-size: 120%;"),
      "Sulfato de Alumínio",
      
      status = "primary",
      icon = "chart-line"
    )
  })
  
  output$cl <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(
        round(sum(quimicos_tbl()$qtd_cg),2), "kg", sep = " "), style = "font-size: 120%;"),
      "Consumo de Cloro granular",
      status = "primary",
      icon = "chart-line"
    )
  })
  
  ### CONSUMO DE ALUMINIO E CLORO GRANUAL
  output$barChartq <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_xAxis(categories=format(quimicos_tbl()$date, format="%m-%y"))%>%
      
      hc_add_series(name="Sulfato de alumínio",
                    data=round(quimicos_tbl()$qtd_sa,1),
                    dataLabels=list(enabled=T))%>%
      
      hc_add_series(name="Cloro granular",
                    data=round(quimicos_tbl()$qtd_cg,1),
                    dataLabels=list(enabled=T))%>%
      
      hc_title(text = "Consumo de sulfato de aluminío e cloro granular") %>%
      
      # hc_subtitle(text = "Conjunto Captação",
      #             style = list(fontWeight = "bold")) %>%
      
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5
      ) %>%
      hc_exporting(enabled = TRUE) 
    
  })
  
  output$barChartt <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_xAxis(categories=format(quimicos_tbl()$date, format="%m-%y"))%>%
      
      hc_add_series(name="DPD1",
                    data=round(quimicos_tbl()$qtd_dpd1,1),
                    dataLabels=list(enabled=T))%>%
      
      hc_add_series(name="pH",
                    data=round(quimicos_tbl()$qtd_ph,1),
                    dataLabels=list(enabled=T))%>%
      
      hc_title(text = "Consumo de DPD1 e pH") %>%
      
      
      # hc_subtitle(text = "Conjunto Captação",
      #             style = list(fontWeight = "bold")) %>%
      
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5
      ) %>%
      hc_exporting(enabled = TRUE) 
    
  })
  
  output$dpd1 <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(
        round(sum(quimicos_tbl()$qtd_dpd1),0), "Unidades", sep = " "
      ), style = "font-size: 120%;"),
      "Consumo de Reagente DPD1",
      status = "primary",
      icon = "chart-line"
      
    )
  })
  
  output$ph_1 <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(
        round(sum(quimicos_tbl()$qtd_ph),0), "Unidades", sep = " "
      ), style = "font-size:120%;"),
      "Consumo de Reagente PH",
      status = "primary",
      icon = "chart-line"
    )
  })
  
  
  output$barChartt <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_xAxis(categories=format(quimicos_tbl()$date, format="%m-%y"))%>%
      
      hc_add_series(name="DPD1",
                    data=round(quimicos_tbl()$qtd_dpd1,1),
                    dataLabels=list(enabled=T))%>%
      
      hc_add_series(name="pH",
                    data=round(quimicos_tbl()$qtd_ph,1),
                    dataLabels=list(enabled=T))%>%
      
      hc_title(text = "Consumo de DPD1 e pH") %>%
      
      
      # hc_subtitle(text = "Conjunto Captação",
      #             style = list(fontWeight = "bold")) %>%
      
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#FCFFC5",
        shared = TRUE,
        borderWidth = 5
      ) %>%
      hc_exporting(enabled = TRUE) 
    
  })
  
  
  
  ##### INICIO AVARIAS E RESTRICOES
  
  output$avarias_adutora <- renderbs4ValueBox({
    bs4ValueBox(
      value = tags$p(
        sum(avarias_tbl()[2]), style = "font-size: 120%;"),
      "Avarias na adutora",
      status = "primary",
      icon = "chart-line"
      
    )
  })
  
  output$avarias_rd <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(
        sum(avarias_tbl()[3]), style = "font-size: 120%;"),
      "Avarias na rede de distribuição",
      status = "primary",
      icon = "chart-line"
    )
  })
  
  output$avarias_outras <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(
        sum(avarias_tbl()[4]), style = "font-size: 120%;"),
      "Outras avarias",
      status = "primary",
      icon = "chart-line"
    )
  })
  
  output$horas_avarias <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(paste(sum(avarias_tbl()[5]),"horas", sep = " ")
                      , style = "font-size: 120%;"),
      "Restrições na adutora",
      status = "primary",
      icon = "chart-line"
    )
  })
  
  output$horas_energia <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(
        paste(sum(avarias_tbl()[6]),"horas", sep = " "), style = "font-size: 120%;"),
      "Restrições por corte de energia",
      status = "primary",
      icon = "chart-line"
    )
  })
  
  output$horas_outras <- renderbs4ValueBox({
    bs4ValueBox(
      value =  tags$p(
        paste(sum(avarias_tbl()[7]),"horas", sep = " "), style = "font-size: 120%;"),
      "Outras restrições",
      status = "primary",
      icon = "chart-line")
  })
  
  output$chartAvariasT <- renderHighchart({
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_title(text = "Avarias nos serviços") %>%
      
      hc_xAxis(
        categories = format(avarias_tbl()$date,format="%d-%m"),
        title = list(text = "Período")
      ) %>%
      
      hc_add_series(
        name = "Avarias na Adutora",
        data = avarias_tbl()$avarias_adutora,
        dataLabels = list(enabled = TRUE)
      ) %>%
      
      hc_add_series(
        name = "Avarias na rede distribuição",
        data =avarias_tbl()$avarias_rd,
        dataLabels = list(enabled = TRUE)
      ) %>%
      
      hc_add_series(
        name = "Outras avarias",
        data = avarias_tbl()$outras,
        dataLabels = list(enabled = TRUE)
      )
    
  })
  
  output$chartAvarias <- renderHighchart({
    
    highchart() %>%
      
      hc_chart(type="column")%>%
      
      hc_title(text = "Horas de restrição nos serviços") %>%
      
      hc_xAxis(
        categories = format(avarias_tbl()$date,format="%d-%m"),
        title = list(text = "Período")
      ) %>%
      
      hc_add_series(
        name = "Restrições na adutora",
        data = avarias_tbl()$horas_restricao_avarias,
        dataLabels = list(enabled = TRUE)
      )%>%
      
      hc_add_series(
        name = "Restrições por corte de energia",
        data =avarias_tbl()$horas_restricao_energia,
        dataLabels = list(enabled = TRUE)
      ) %>%
      
      hc_add_series(
        name = "Outras restrições",
        data = avarias_tbl()$horas_restricao_outro,
        dataLabels = list(enabled = TRUE)
      )
    
  })
  
  
  
  
  ##### INICIO TABELAS #####
  output$tbl <- renderDT({
    datatable(nova_base(), options = list(scrollX = TRUE))
  })
  
  output$tbl1 <- renderDT({
    datatable(quality(),  options = list(scrollX = TRUE))
  })
  
  output$tbl2 <- renderDT({
    datatable(quimicos_tbl(), options = list(scrollX = TRUE))
  })
  
  output$tbl3 <- renderDT({
    datatable(avarias_tbl(), options = list(scrollX = TRUE))
  })
  
  output$tbl4 <- renderDT({
    datatable(horas(), options = list(scrollX = TRUE))
  })
  
  
  
}


shinyApp(ui=ui,server=server)
