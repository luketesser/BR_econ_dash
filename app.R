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

expec_pib_2024 <- bc$expec_pib |>
  filter(DataReferencia == 2024 & Data > (Sys.Date() - 252))

expec_pib_2025 <- bc$expec_pib |>
  filter(DataReferencia == 2025 & Data > (Sys.Date() - 126))

expec_pib_2026 <- bc$expec_pib |>
  filter(DataReferencia == 2026 & Data > (Sys.Date() - 63))

pmc_margem <- sidra$pmc2 |> # Comércio Ampliado (Inclui - Veículos, motocicletas, partes e peças. - Material de construção.)
  filter(`Variável (Código)` == 11708)

pmc_index <- sidra$pmc2 |>
  filter(`Variável (Código)` == 7170)

pim_margem <- sidra$pim |>
  filter(`Variável (Código)` == 11601 & `Seções e atividades industriais (CNAE 2.0) (Código)` == 129314)

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

gap_des <- desemp |>
  mutate(trend_ma = decompose(ts(tx_desemp, frequency = 4))$trend) |>
  mutate(gap_ma = (tx_desemp/trend_ma - 1)*100)

gap_desemp_ham <- yth_filter(xts(desemp$tx_desemp, order.by = desemp$dates), output = c("trend", "cycle")) |>
  as_tibble() |>
  mutate(dates  = desemp$dates) |>
  mutate(gap  = y.cycle/y.trend)

gap_desemp_hp_m <- hpfilter(ts(desemp$tx_desemp, frequency = 4), type = 'lambda', freq = 1600)

gap_desemp_hp <- tibble(dates = desemp$dates, trend = gap_desemp_hp_m$trend, cycle = gap_desemp_hp_m$cycle) |>
  mutate(gap = cycle/trend)

priv_clt <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31722 & `Variável (Código)` == 4108)

priv_s_clt <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31723 & `Variável (Código)` == 4108)

domest <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31724 & `Variável (Código)` == 4108)

public <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31727 & `Variável (Código)` == 4108)

empregador <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 96170 & `Variável (Código)` == 4108)

auton <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 96171 & `Variável (Código)` == 4108)

fam <- sidra$pnad2 |>
  filter(`Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31731 & `Variável (Código)` == 4108)

categ <- tibble(dates = pnad_dates$date, priv_clt = priv_clt$Valor, priv_s_clt = priv_s_clt$Valor, domest = domest$Valor,
                public = public$Valor, empregador = empregador$Valor, auton = auton$Valor, fam = fam$Valor)

rend <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 96165)

r_priv_clt <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31722)

r_priv_s_clt <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31723)

r_domest <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31724)

r_public <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 31727)

r_empregador <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 96170)

r_auton <- sidra$pnad3 |>
  filter(`Variável (Código)` == 5932 & `Posição na ocupação e categoria do emprego no trabalho principal (Código)` == 96171)

r_categ <- tibble(dates = pnad_dates$date, rend = rend$Valor, r_priv_clt = r_priv_clt$Valor, r_priv_s_clt = r_priv_s_clt$Valor,
                  r_domest = r_domest$Valor, r_public = r_public$Valor, r_empregador = r_empregador$Valor, r_auton = r_auton$Valor)

# IPCA
ipca_nucleos <- tibble(date = filter(bc$ipca$ipca, bc$ipca$ipca$date > "2000-01-01")$date,
                      ipca = filter(bc$ipca$ipca, bc$ipca$ipca$date > "2000-01-01")$ipca,
                      p55 = filter(bc$ipca$n8, bc$ipca$n8$date > "2000-01-01")$n8,
                      ex0 = filter(bc$ipca$n3, bc$ipca$n3$date > "2000-01-01")$n3,
                      ex3 = filter(bc$ipca$n7, bc$ipca$n7$date > "2000-01-01")$n7,
                      ms = filter(bc$ipca$n1, bc$ipca$n1$date > "2000-01-01")$n1,
                      dp = filter(bc$ipca$n5,  bc$ipca$n5$date > "2000-01-01")$n5)

ipca_nucleos_tidy <- tidyr::pivot_longer(ipca_nucleos, cols = ipca:dp)

ipca_acum <- bc$ipca$ipca |>
  mutate(plus1 = ipca/100 + 1) |>
  group_by(year(date)) |>
  summarise(annual = round((prod(plus1) - 1)*100, 2))

ipca_12m <- bc$ipca$ipca |>
  mutate(plus1 = ipca/100 + 1)

ipca_yearly = tibble(yearly = RcppRoll::roll_prod(ipca_12m$plus1, n = 12) - 1)*100

# Inércia
## Criando matrizes que guardarão coeficientes e desvios-padrões
p <- 2 # Parâmetros a serem guardados

janela <- 48 # número de meses da janela

coefs <- matrix(NA, ncol = p, nrow = length(ipca_nucleos$date) - janela)
sd <- matrix(NA, ncol = p, nrow = length(ipca_nucleos$date) - janela)

# Loop

for (i in 1:nrow(coefs)) {

  ar1 <- forecast::Arima(ipca_nucleos$ipca[(1 + i - 1):(janela + i - 1)],
               order = c(1,0,0))
  coefs[i,] <- stats::coef(ar1)
  sd[i,] <- lmtest::coeftest(ar1)[,2]
}

colnames(coefs) <- c('AR(1)', 'Intercepto')
colnames(sd) <- c('AR(1)', 'Intercepto')

inercia <- tibble(date = seq.Date(from = dplyr::nth(ipca_nucleos$date, janela + 1), to = dplyr::last(ipca_nucleos$date), by = "month"),
                  ic1 = coefs[,1] - sd[,1], ar1 = coefs[,1], ic2 = coefs[,1] + sd[,1])

ipca_desag <- tibble(date = filter(bc$ipca_desag$al, date > "2000-01-01")$date,
                     aliment = filter(bc$ipca_desag$al, date > "2000-01-01")$al,
                     comun =  filter(bc$ipca_desag$comun, date > "2000-01-01")$comun,
                     educ = filter(bc$ipca_desag$educ, date > "2000-01-01")$educ,
                     hab = filter(bc$ipca_desag$hab, date > "2000-01-01")$hab,
                     pessoais = filter(bc$ipca_desag$pess, date > "2000-01-01")$pess,
                     residenc = filter(bc$ipca_desag$res, date > "2000-01-01")$res,
                     saude =  filter(bc$ipca_desag$saud, date > "2000-01-01")$saud,
                     transp = filter(bc$ipca_desag$trasp, date > "2000-01-01")$trasp)
ipca_desag_tidy <- tidyr::pivot_longer(ipca_desag, cols = aliment:transp)

ipca_sidra <- as_tibble(sidra$ipca)

ipca_difu <- ipca_sidra |>
  group_by(dates) |>
  summarise(diffusion = sum(Valor > 0)/length(Valor))

expec_ipca_2024 <- bc$expec_ipca |>
  filter(DataReferencia == 2024 & Data > (Sys.Date() - 252))

expec_ipca_2025 <- bc$expec_ipca |>
  filter(DataReferencia == 2025 & Data > (Sys.Date() - 252))

expec_ipca_2026 <- bc$expec_ipca |>
  filter(DataReferencia == 2026 & Data > (Sys.Date() - 252))

expec_ipca_top5 <- bc$expec_ipca_top5

expec_ipca_top5_2024 <- expec_ipca_top5 |>
  filter(DataReferencia == 2024 & tipoCalculo == 'M')


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
      menuItem("Emprego", tabName = "emprego", icon = icon("person-digging")),
      menuItem("Inflação", tabName = "inflação", icon = icon("chart-pie"))
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
          infoBox(title = "PIB (%) Margem", value = round(last(delta_pib$delta_pib_margem)*100, 2), icon = icon("chart-line")),
          infoBox(title = "PIB (%) Trimestral", value = round(last(delta_pib$delta_pib_trimestral)*100, 2), icon = icon("chart-line")),
          infoBox(title = "PIB (%) Anual", value = round(last(delta_pib$delta_pib_anual)*100, 2), icon = icon("chart-line")),
          infoBox(title = "IBC-BR Margem (%)", value = round(last(ibc$delta)*100, 2)),
          infoBox(title = "PMC Receita Vendas (%)", value = round(last(filter(pmc_margem, pmc_margem$`Tipos de índice (Código)` == 56735)$Valor), 2), icon = icon("file-invoice-dollar")),
          infoBox(title = "PMC Volume Vendas (%)", value = round(last(filter(pmc_margem, pmc_margem$`Tipos de índice (Código)` == 56736)$Valor), 2), icon = icon("file-invoice-dollar")),
          infoBox(title = "PIM Margem (%)", value = round(last(pim_margem$Valor), 2), icon = icon("industry")),
          infoBox(title = "Hiato Hamilton (2017) (%)", value = round(last(gap_hamilton$gap), 2), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Hiato HP Filter (%)", value = round(last(gap_hp$gap), 2), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Hiato Média MA, HP e Hamilton", value = round(last(gap_avg$average), 2), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Utilização da Capacidade Instalada", value = round(last(bc$uci$uci), 2), icon = icon("industry")),
          infoBox(title = "Mediana Expectativa Focus 2024", value = round(last(expec_pib_2024$Mediana), 2), icon = icon("eye-low-vision"))
        ),
        fluidRow(
          shinydashboard::box(plotOutput("plot_gdp"), title = "PIB Real", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gdp_open"), title = "PIB Setores", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_ma"),  title = "Hiato do Produto Decomposição MA", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_h"),  title = "Hiato do Produto Hamilton (2017)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_hp"),  title = "Hiato do Produto Filtro HP", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_avg"),  title = "Hiato do Produto Média", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_uci"),  title = "Utilização da Capacidade Intalada (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_pmc"),  title = "PMC - Delta (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_pim"),  title = "PIM - Delta (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          shinydashboard::box(plotOutput("plot_expec"), title = "Mediana Expectativa Focus", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )

      ),

      # Emprego
      tabItem(
        tabName = "emprego",
        fluidRow(
          infoBox(title = "População", value = round(last(pop$Valor), 2), icon = icon("people-group")),
          infoBox(title = "PIA (População em Idade Ativa)", value = round(last(pian$Valor), 2), icon = icon("people-group")),
          infoBox(title = "PEA (População Economicamente Ativa)", value = round(last(pean$Valor), 2), icon = icon("person-running")),
          infoBox(title = "PO (População Ocupada)", value = round(last(pon$Valor), 2), icon = icon("briefcase")),
          infoBox(title = "PD (População Desocupada)", value = round(last(pdn$Valor), 2), icon = icon("person-praying")),
          infoBox(title = "Taxa de Desemprego % (PD/PEA)", value = round(last(desemp$tx_desemp)*100, 2), icon = icon("chart-column")),
          # infoBox(title = "", value = round(last(), 2))
        ),

        fluidRow(
          shinydashboard::box(plotOutput("plot_desemp"), title = "Taxa de Desemprego %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_pnea"), title = "PNEA % - Pop. Não Econ. Ativa %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_pea"), title = "PEA % - Pop. Econ. Ativa %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_gap_desemp"), title = "Hiato do Desemprego (%)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_categ"), title = "Evolução Categorias de Emprego (%)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_rend_categ"), title = "Evolução Rendimento Categorias de Emprego R$", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )

      ),

      # Inflação
      tabItem(
        tabName = "inflação",
        fluidRow(
          infoBox(title = "IPCA Margem", value = round(last(ipca_nucleos$ipca), 2), icon = icon("tag")),
          infoBox(title = "Inércia", value = round(dplyr::last(inercia$ar1),2), icon = icon("lines-leaning")),
          infoBox(title = "índice de Difusão", value = round(dplyr::last(ipca_difu$diffusion), 2), icon = icon("users-rays")),
          infoBox(title = "IPCA Acumulado Ano", value = round(dplyr::last(ipca_acum$annual), 2), icon = icon("chart-line")),
          infoBox(title = "IPCA Acumulado 12m", value = round(dplyr::last(yearly), 2), icon = icon("chart-line")),
          infoBox(title = "Mediana Expectativas 2024", value = round(dplyr::last(expec_ipca_2024$Mediana), 2), icon = icon("eye-low-vision")),
          infoBox(title = "Mediana Expectativas 2025", value = round(dplyr::last(expec_ipca_2025$Mediana), 2), icon = icon("eye-low-vision")),
          infoBox(title = "Mediana Expectativas 2026", value = round(dplyr::last(expec_ipca_2026$Mediana), 2), icon = icon("eye-low-vision")),
          infoBox(title = "Mediana Expectativas 2024 Top 5 M", value = round(dplyr::last(expec_ipca_top5_2024$Mediana), 2), icon = icon("eye-low-vision"))

        ),

        fluidRow(
          shinydashboard::box(plotOutput("plot_ipca_nucleos"), title = "IPCA e Núcleos Selecionados", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_ipca_inercia"), title = "Inércia IPCA", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_ipca_desag"), title = "IPCA - Grupos", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_ipca_difu"), title = "índice de Difusão", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_ipca_acum"), title = "IPCA Acumulado no Ano", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_ipca_expec"), title = "Expectativas IPCA (Mediana)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          shinydashboard::box(plotOutput("plot_ipca_expec_top5"), title = "Expectativas IPCA Top 5 (Mediana)", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
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

  output$plot_pmc <- renderPlot({
    pmc_margem |>
      filter(dates > "2022-01-01") |>
      ggplot(aes(x = dates, y = Valor, color = `Tipos de índice`)) +
      geom_line(linewidth = 1) +
      labs(title = "PMC - Delta (%)", x = NULL, Y = NULL) +
      theme_economist()
  })

  output$plot_pim <- renderPlot({
    pim_margem |>
      filter(dates > "2022-01-01") |>
      ggplot(aes(x = dates, y = Valor)) +
      geom_line(linewidth = 1, color = "brown") +
      labs(title = "PIM - Delta (%)", x = NULL, Y = NULL) +
      theme_economist()
  })

  output$plot_expec <- renderPlot({
    expec_pib_2024 |>
      bind_rows(expec_pib_2025) |>
      bind_rows(expec_pib_2026) |>
      filter(baseCalculo == 0) |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(title = "Mediana Expectativa Focus", x = NULL, Y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_desemp <- renderPlot({
    desemp |>
      ggplot(aes(x = dates, y = tx_desemp*100)) +
      geom_line(linewidth = 1, color = "blue") +
      geom_line(aes(x = dates, y = gap_des$trend_ma*100), linewidth = 1, color = "black") +
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

  output$plot_gap_desemp <- renderPlot({
    gap_desemp_ham |>
      ggplot(aes(x = dates, y = gap*100)) +
      geom_line(linewidth = 1, color = "blue") +
      geom_hline(yintercept = 0, color = 'red') +
      labs(titles = "Hiato do Desemprego (%)", x = NULL, y = NULL ) +
      theme_economist()

  })

  output$plot_categ <- renderPlot({
    categ |>
      ggplot(aes(x = dates)) +
      geom_line(aes(y = priv_clt, color = "priv_clt"), linewidth = 1) +
      geom_line(aes(y = priv_s_clt, color = "priv_s_clt"), linewidth = 1) +
      geom_line(aes(y = domest, color = "domest"), linewidth = 1) +
      geom_line(aes(y = public, color = "public"), linewidth = 1) +
      geom_line(aes(y = empregador, color = "empregador"), linewidth = 1) +
      geom_line(aes(y = auton, color = "auton"), linewidth = 1) +
      geom_line(aes(y = fam, color = "fam"), linewidth = 1) +
      labs(titles = "Evolução Categorias do Trabalho (%)", x = NULL, y = NULL ) +
      theme_economist()

  })

  output$plot_rend_categ <- renderPlot({
    r_categ |>
      ggplot(aes(x = dates)) +
      geom_line(aes(y = r_priv_clt, color = "r_priv_clt"), linewidth = 1) +
      geom_line(aes(y = r_priv_s_clt, color = "r_priv_s_clt"), linewidth = 1) +
      geom_line(aes(y = r_domest, color = "r_domest"), linewidth = 1) +
      geom_line(aes(y = r_public, color = "r_public"), linewidth = 1) +
      geom_line(aes(y = r_empregador, color = "r_empregador"), linewidth = 1) +
      geom_line(aes(y = r_auton, color = "r_auton"), linewidth = 1) +
      labs(titles = "Evolução Rendimento Categorias do Trabalho R$", x = NULL, y = NULL ) +
      theme_economist()

  })

  output$plot_ipca_nucleos <- renderPlot({

    ipca_line <- ipca_nucleos_tidy |>
      filter(name == "ipca" & date > "2022-01-01")

    ipca_nucleos_tidy |>
      filter(date > "2022-01-01") |>
      ggplot(aes(x = date, y = value, color = name)) +
      geom_line() +
      geom_line(data = ipca_line, aes(x = date, y = value), linewidth = .9) +
      labs(title = "IPCA e Núcleos Acompanhados", x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_desag <- renderPlot({

    ipca_desag_tidy |>
      filter(date > "2010-01-01") |>
      ggplot(aes(x = date, y = value, color = name)) +
      geom_line() +
      geom_smooth(data = filter(ipca_desag_tidy, ipca_desag_tidy$date > Sys.Date() - 712), method = "lm", formula = y ~ x, color = "brown") +
      facet_wrap(ncol = 2, vars(name), scales = "free") +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_acum <- renderPlot({
    ipca_acum |>
      filter(`year(date)` > "2000-01-01") |>
      ggplot(aes(x = `year(date)`, y = annual)) +
      geom_line(linewidth = 1, color = "darkblue") +
      labs(title = "IPCA Acumulado no Ano", x = NULL, y = NULL) +
      theme_economist()
  })

  output$plot_ipca_inercia <- renderPlot({

    inercia |>
      ggplot(aes(x = date)) +
      geom_ribbon(aes(ymin = ic1, ymax = ic2)) +
      geom_line(aes(y = ar1), linewidth = 1, color = "grey70") +
      geom_hline(aes(yintercept = mean(ar1)), color = "red") +
      labs(title = "Inércia IPCA", x = NULL, y = NULL) +
      theme_economist()
  })

  output$plot_ipca_difu <- renderPlot({
    ipca_difu |>
      ggplot(aes(x = dates, y = diffusion)) +
      geom_line(linewidth = 1, color = "gray35") +
      geom_hline(aes(yintercept = mean(diffusion)), color = "red") +
      labs(title = "Índice de Difusão", x = NULL, y = NULL) +
      theme_economist()
  })

  output$plot_ipca_expec <- renderPlot({
    expec_ipca |>
      filter(Data > Sys.Date() - 180 & baseCalculo == 0) |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(title = "Expectativas IPCA (Mediana)", x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_expec_top5 <- renderPlot({
    expec_ipca_top5 |>
      filter(Data > Sys.Date() - 180 & tipoCalculo == "M") |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(title = "Expectativas IPCA Top 5 (Mediana)", x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })


}# End of Server

# Shiny App --------------------------------------------------------------------

shinyApp(ui, server)





























