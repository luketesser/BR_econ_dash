#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(cowplot)
library(forecast)
library(neverhpfilter)
library(mFilter)
# source("R/sidra_data.R")
# source("R/rbcb_data.R")

# Read pre-saved data ----------------------------------------------------------

sidra <- readRDS('Data/sidra.rds')

bc <- readRDS('Data/bc.rds')

# Values for Info boxes ------------------------------------------------

delta_pib <- sidra$pib |> 
  filter(`Setores e subsetores (Código)` == 90707) |> 
  mutate(delta_pib_margem = Valor/lag(Valor)-1) |> 
  mutate(delta_pib_anual = (((Valor + lag(Valor, 1) + lag(Valor, 2) + lag(Valor, 3))/4)/ 
              ((lag(Valor, 4) + lag(Valor, 5) + lag(Valor, 6) + lag(Valor, 7))/4) - 1 )) |> 
  mutate(delta_pib_trimestral = Valor/lag(Valor, 4) - 1)

gap <- sidra$pib |> 
  filter(`Setores e subsetores (Código)` == 90707) |> 
  mutate(trend = decompose(ts(Valor, frequency = 4))$trend) |> 
  mutate(gap = Valor/trend - 1)
  
gap_hamilton <- yth_filter(xts(gap$Valor, order.by = gap$dates), output = c("trend", "cycle")) |> 
  as_tibble() |> 
  mutate(dates = gap$dates)

gap_hp_m <- hpfilter(ts(gap$Valor, frequency = 4), type='lambda', freq=1600) 

gap_hp <- tibble(dates = gap$dates, trend = gap_hp_m$trend, cycle = gap_hp_m$cycle)

# Application UI ---------------------------------------------------------------

ui <- dashboardPage(
  
  skin = "blue",
  
  # Dashboard Header (App name) ------------------------------------------------
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Conjuntura BR")#,
      # img(src = "reta_logo.png", height = 20, width = 30)
    ),
    titleWidth = "300px",
    
    # Message box top-right corner
    dropdownMenu(
      type = "messages",
      messageItem(
        from    = "Open Source",
        message = "Veja no Github",
        icon    = icon("github"),
        time    = as.character(Sys.time()),
        href    = "https://github.com/luketesser/BR_econ_dash"
      ),
      messageItem(
        from    = "Reta Asset",
        message = "Conheça a Reta Asset",
        icon    = icon("chart-line"),
        time    = as.character(Sys.time()),
        href    = "https://www.retaasset.com"
      )
    )
  ),
  
  # Sidebar --------------------------------------------------------------------
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Atividade", tabName = "activity", icon = icon("money-bill-trend-up"))
    )
  ),
  
  # Dashboard Body (App Body)---------------------------------------------------
  
 body = dashboardBody(
    # TabItems
    
    tabItems(
      
      # Atividade
      tabItem(
        tabName = "activity",
        fluidRow(
          infoBox(title = "PIB (%) Margem", value = round(last(delta_pib$delta_pib_margem)*100, 2)), 
          infoBox(title = "PIB (%) Trimestral", value = round(last(delta_pib$delta_pib_trimestral)*100, 2)),
          infoBox(title = "PIB (%) Anual", value = round(last(delta_pib$delta_pib_anual)*100, 2)),
          # infoBox(title = "Hiato MA (%)", value = round(last(gap$gap)*100, 2)),
          infoBox(title = "Hiato Hamilton (2017) (%)", value = round(last(gap_hamilton$y.cycle)/last(gap_hamilton$y.trend)*100, 2)),
          infoBox(title = "Hiato HP Filter (%)", value = round(last(gap_hp$cycle)/last(gap_hp$trend)*100, 2))
        ),
        fluidRow(
          shinydashboard::box(plotOutput("plot_gdp"), title = "PIB Real", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gdp_open"), title = "PIB Setores", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_ma"),  title = "Hiato do Produto Decomposição MA", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_h"),  title = "Hiato do Produto Hamilton (2017)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_hp"),  title = "Hiato do Produto Filtro HP", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary")
        )
        
      )
      
      # Here would go another tabItem()
    ) # End of tabItems
    
  ), # End of Dashboard Body
  
 # Dashboard footer ------------------------------------------------------------
  
  footer = dashboardFooter(
    left  = p("Desenvolvido por ", strong("Reta Asset LTDA")),
    right = p("Porto Alegre, Rio Grande do Sul, ", strong(lubridate::today()))
  ) # End of Dashboard footer

) # End of Dashboard Page (UI)


# Server -----------------------------------------------------------------------

server <- function(input, output){
  
  pib <- sidra$pib
  
  output$plot_gdp <- renderPlot({
    
    
    # PIB a preços de mercado
    pib |> 
      filter(`Setores e subsetores (Código)` == 90707) |> 
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(aes(y = Valor), linewidth = 1.5, color = 'blue')+
      labs(title = "PIB Real", x = NULL, y = 'Número índice')+
      theme_economist()
      
    
  })
  
  output$plot_gap_ma <- renderPlot({
    
    gap |> 
      ggplot(mapping = aes(x = dates, y = gap*100))+
      geom_line(aes(y = gap*100), linewidth = .7, color = 'black')+
      geom_hline(yintercept = 0, color = 'red')+
      labs(title = 'Hiato do Produto Filtro MA (%)', x = NULL, y = NULL)+
      theme_economist()
      
  })
  
  output$plot_gap_h <- renderPlot({
    
    gap_hamilton |> 
      mutate(gap = y.cycle/y.trend*100) |> 
      ggplot(mapping = aes(x = dates, y = gap))+
      geom_line(aes(y = gap), linewidth = .7, color = 'black')+
      geom_hline(yintercept = 0, color = 'red')+
      labs(title = 'Hiato do Produto Hamilton (2017) (%)', x = NULL, y = NULL)+
      theme_economist()
    
  })
  
  output$plot_gap_hp <- renderPlot({
    
    gap_hp |> 
      mutate(gap = cycle/trend*100) |> 
      ggplot(mapping = aes(x = dates, y = gap))+
      geom_line(aes(y = gap), linewidth = .7, color = 'black')+
      geom_hline(yintercept = 0, color = 'red')+
      labs(title = 'Hiato do Produto Filtro HP (%)', x = NULL, y = NULL)+
      theme_economist()
      
    
  })
  
  output$plot_gdp_open <- renderPlot({
    
    # PIB Agro
    
    pib_agro <- pib |> 
      filter(`Setores e subsetores (Código)` == 90687) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |> 
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'green')+
      labs(title = "PIB Agro", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB Ind
    
    pib_ind <- pib |> 
      filter(`Setores e subsetores (Código)` == 90691) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |> 
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'grey')+
      labs(title = "PIB Indústria", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB Serviços
    
    pib_serv <- pib |> 
      filter(`Setores e subsetores (Código)` == 90696) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |> 
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'blue')+
      labs(title = "PIB Serviços", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB Gov
    
    pib_gov <- pib |> 
      filter(`Setores e subsetores (Código)` == 93405) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |> 
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'yellow')+
      labs(title = "PIB Governo", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB FBCF
    
    pib_fbcf <- pib |> 
      filter(`Setores e subsetores (Código)` == 93406) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'magenta')+
      labs(title = "FBCF", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB CF
    
    pib_cf <- pib |> 
      filter(`Setores e subsetores (Código)` == 93404) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'orange')+
      labs(title = "CF", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB X
    
    pib_x <- pib |> 
      filter(`Setores e subsetores (Código)` == 93407) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'purple')+
      labs(title = "X", x = NULL, y = 'Índice')+
      theme_economist()
    
    #PIB M
    
    pib_m <- pib |> 
      filter(`Setores e subsetores (Código)` == 93408) |> 
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates, y = Valor))+
      geom_line(linewidth = 1, color = 'brown')+
      labs(title = "M", x = NULL, y = 'Índice')+
      theme_economist()
    
    plot_grid(pib_agro, pib_ind, pib_serv, pib_gov, pib_fbcf, pib_cf, pib_x, pib_m, nrow = 3, labels = "AUTO")
    
  })
  
  
}# End of Server

# Shiny App --------------------------------------------------------------------

shinyApp(ui, server)





























