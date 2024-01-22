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

ymd <- "2023-09-01" # last trimester of available data (CNT). Must automatize.

# Values for Info boxes ------------------------------------------------

dates_helper <- sidra$pib |>
  filter(`Setores e subsetores (Código)` == 90707) |>
  mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd), by = "quarter"))

delta_pib <- sidra$pib |>
  filter(`Setores e subsetores (Código)` == 90707) |>
  mutate(delta_pib_margem = Valor/lag(Valor) - 1) |>
  mutate(delta_pib_anual = (((Valor + lag(Valor, 1) + lag(Valor, 2) + lag(Valor, 3))/4)/
              ((lag(Valor, 4) + lag(Valor, 5) + lag(Valor, 6) + lag(Valor, 7))/4) - 1 )) |>
  mutate(delta_pib_trimestral = Valor/lag(Valor, 4) - 1)

gap <- sidra$pib |>
  filter(`Setores e subsetores (Código)` == 90707) |>
  mutate(trend = decompose(ts(Valor, frequency = 4))$trend) |>
  mutate(gap = (Valor/trend - 1)*100)

gap_hamilton <- yth_filter(xts(gap$Valor, order.by = dates_helper$date), output = c("trend", "cycle")) |>
  as_tibble() |>
  mutate(dates = dates_helper$date) |>
  mutate(gap = y.cycle/y.trend*100)

gap_hp_m <- hpfilter(ts(gap$Valor, frequency = 4), type = 'lambda', freq = 1600)

gap_hp <- tibble(dates = dates_helper$date, trend = gap_hp_m$trend, cycle = gap_hp_m$cycle) |>
  mutate(gap = cycle/trend*100)

gap_avg <- tibble(Dates = dates_helper$date, MA = gap$gap, hp = gap_hp$gap, h = gap_hamilton$gap)

gap_avg <- gap_avg |>
  mutate(average = rowMeans(select(gap_avg, MA, hp, h), na.rm = T))

ibc <- bc$ibc |>
  mutate(delta = ibcbr/lag(ibcbr, 1) - 1)

# pmc1_margem <- sidra$pmc1 |> # Comércio Restrito (sem - Veículos, motocicletas, partes e peças. - Material de construção.)
  # filter(`Variável (Código)` == 11708) |>

# Emprego

pop <- sidra$pop |>
  mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd), by = "quarter"))

pnad <- sidra$pnad

pnad_dates <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32385 & pnad$`Variável (Código)` == 1641) |>
  mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd), by = "quarter"))

pia <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32385)

pian <- pia |>
  filter(pia$`Variável (Código)` == 1641)

pea <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32386)

pean <- pea |>
  filter(pea$`Variável (Código)` == 1641)

peap <- pea |>
  filter(pea$`Variável (Código)` == 4104)

pnea <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32447)

pnean <- pnea |>
  filter(pnea$`Variável (Código)` == 1641)

pneap <- pnea |>
  filter(pnea$`Variável (Código)` == 4104)

po <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32387)

pon <- po |>
  filter(po$`Variável (Código)` == 1641)

po_p <- po |>
  filter(po$`Variável (Código)` == 4104)

pd <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32446)

pdn <- pd |>
  filter(pd$`Variável (Código)` == 1641)

pdp <- pd |>
  filter(pd$`Variável (Código)` == 4104)

desemp <- tibble(pdn$Valor/pean$Valor) |>  # PD/PEA
  mutate(dates = pdn$dates)
  colnames(desemp) = c('tx_desemp', 'dates')


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
      menuItem("Atividade", tabName = "activity", icon = icon("money-bill-trend-up")), # fontawesome.com/search?q=chart&o=r&m=free
      menuItem("Emprego", tabName = "emprego", icon = icon("fa-solid fa-person-digging"))
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
          infoBox(title = "IBC-BR Margem (%)", value = round(last(ibc$delta)*100, 2)),
          # infoBox(title = "Hiato MA (%)", value = round(last(gap$gap)*100, 2)),
          infoBox(title = "Hiato Hamilton (2017) (%)", value = round(last(gap_hamilton$gap), 2)),
          infoBox(title = "Hiato HP Filter (%)", value = round(last(gap_hp$gap), 2)),
          infoBox(title = "Hiato Média MA, HP e Hamilton", value = round(last(gap_avg$average), 2)),
          infoBox(title = "Utilização da Capacidade Instalada", value = round(last(bc$uci$uci), 2))
        ),
        fluidRow(
          shinydashboard::box(plotOutput("plot_gdp"), title = "PIB Real", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gdp_open"), title = "PIB Setores", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_ma"),  title = "Hiato do Produto Decomposição MA", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_h"),  title = "Hiato do Produto Hamilton (2017)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_hp"),  title = "Hiato do Produto Filtro HP", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_avg"),  title = "Hiato do Produto Média", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_uci"),  title = "Utilização da Capacidade Intalada (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary")
        )

      ),

      # Emprego
      tabItem(
        tabName = "emprego",
        fluidRow(
          infoBox(title = "População", value = round(last(pop$Valor), 2)),
          infoBox(title = "PIA (População em Idade Ativa)", value = round(last(pian$Valor), 2)),
          infoBox(title = "PEA (População Economicamente Ativa)", value = round(last(pean$Valor), 2)),
          infoBox(title = "PO (População Ocupada)", value = round(last(pon$Valor), 2)),
          infoBox(title = "PD (População Desocupada)", value = round(last(pdn$Valor), 2)),
          infoBox(title = "Taxa de Desemprego % (PD/PEA)", value = round(last(desemp$tx_desemp)*100, 2))
          # infoBox(title = "", value = round(last(), 2))
        ),

        fluidRow(
          shinydashboard::box(plotOutput("plot_desemp"), title = "Taxa de Desemprego %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_pnea"), title = "PNEA % - Pop. Não Econ. Ativa %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_pea"), title = "PEA % - Pop. Econ. Ativa%", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )

      )
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
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(aes(y = Valor), linewidth = 1.5, color = 'blue') +
      labs(title = "PIB Real", x = NULL, y = 'Número índice') +
      theme_economist()


  })

  output$plot_gap_ma <- renderPlot({

    gap |>
      ggplot(mapping = aes(x = dates_helper$date, y = gap*100)) +
      geom_line(aes(y = gap*100), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(title = 'Hiato do Produto Filtro MA (%)', x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_gap_h <- renderPlot({

    gap_hamilton |>
      mutate(gap = y.cycle/y.trend*100) |>
      ggplot(mapping = aes(x = dates_helper$date, y = gap)) +
      geom_line(aes(y = gap), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(title = 'Hiato do Produto Hamilton (2017) (%)', x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_gap_hp <- renderPlot({

    gap_hp |>
      mutate(gap = cycle/trend*100) |>
      ggplot(mapping = aes(x = dates_helper$date, y = gap)) +
      geom_line(aes(y = gap), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(title = 'Hiato do Produto Filtro HP (%)', x = NULL, y = NULL) +
      theme_economist()


  })

  output$plot_gap_avg <- renderPlot({

    gap_avg |>
      ggplot(mapping = aes(x = dates_helper$date, y = average)) +
      geom_line(aes(y = average), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(title = 'Hiato do Produto Média (%)', x = NULL, y = NULL) +
      theme_economist()


  })

  output$plot_gdp_open <- renderPlot({

    # PIB Agro

    pib_agro <- pib |>
      filter(`Setores e subsetores (Código)` == 90687) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'green') +
      labs(title = "PIB Agro", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB Ind

    pib_ind <- pib |>
      filter(`Setores e subsetores (Código)` == 90691) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'grey') +
      labs(title = "PIB Indústria", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB Serviços

    pib_serv <- pib |>
      filter(`Setores e subsetores (Código)` == 90696) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'blue') +
      labs(title = "PIB Serviços", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB Gov

    pib_gov <- pib |>
      filter(`Setores e subsetores (Código)` == 93405) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'yellow') +
      labs(title = "PIB Governo", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB FBCF

    pib_fbcf <- pib |>
      filter(`Setores e subsetores (Código)` == 93406) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'magenta') +
      labs(title = "FBCF", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB CF

    pib_cf <- pib |>
      filter(`Setores e subsetores (Código)` == 93404) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'orange') +
      labs(title = "CF", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB X

    pib_x <- pib |>
      filter(`Setores e subsetores (Código)` == 93407) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'purple') +
      labs(title = "X", x = NULL, y = 'Índice') +
      theme_economist()

    #PIB M

    pib_m <- pib |>
      filter(`Setores e subsetores (Código)` == 93408) |>
      mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
      ggplot(mapping = aes(x = dates_helper$date, y = Valor)) +
      geom_line(linewidth = 1, color = 'brown') +
      labs(title = "M", x = NULL, y = 'Índice') +
      theme_economist()

    plot_grid(pib_agro, pib_ind, pib_serv, pib_gov, pib_fbcf, pib_cf, pib_x, pib_m, nrow = 3, labels = "AUTO")

  })

  output$plot_uci <- renderPlot({
    bc$uci |>
      ggplot(aes(x = date,  y = uci)) +
      geom_line(linewidth = 1, color = "gray") +
      geom_smooth(data = subset(bc$uci, date > '2021-01-01'), method = 'lm', formula = y ~ x, color = 'red') +
      labs(title = "Utilização da Capacidade Instalada (%)", x = NULL, Y = NULL) +
      theme_economist()
  })

  output$plot_desemp <- renderPlot({
    desemp |>
      ggplot(aes(x = dates, y = tx_desemp*100)) +
      geom_line(linewidth = 1, color = "blue") +
      labs(titles = "Taxa de Desemprego %", x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_pnea <- renderPlot({
    pneap |>
      ggplot(aes(x = dates, y = Valor)) +
      geom_line(linewidth = 1, color = "brown") +
      labs(titles = "PNEA % - Pop. Não Econ. Ativa", x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_pea <- renderPlot({
    peap |>
      ggplot(aes(x = dates, y = Valor)) +
      geom_line(linewidth = 1, color = "yellow") +
      labs(titles = "PEA % - Pop. Econ. Ativa", x = NULL, y = NULL) +
      theme_economist()

  })


}# End of Server

# Shiny App --------------------------------------------------------------------

shinyApp(ui, server)





























