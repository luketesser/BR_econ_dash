#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
library(bs4Dash)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(cowplot)
library(forecast)
library(neverhpfilter)
library(mFilter)
library(bizdays)
library(alphavantager)
# library(quantmod)
library(scales)
library(tidyquant)
# source("R/sidra_data.R")
# source("R/rbcb_data.R")

# Read pre-saved data ----------------------------------------------------------

sidra <- readRDS('Data/sidra.rds')

bc <- readRDS('Data/bc.rds')

ipea <- readRDS('Data/ipea.rds')

avg <- readRDS('Data/avg.rds')

ymd <- "2024-03-01" # last trimester of available data (CNT). Must automatize.

ymd_pnad <- "2024-03-01"

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
  mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd_pnad), by = "quarter"))

pnad <- sidra$pnad

pnad_dates <- pnad |>
  filter(pnad$`Condição em relação à força de trabalho e condição de ocupação (Código)` == 32385 & pnad$`Variável (Código)` == 1641) |>
  mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd_pnad), by = "quarter"))

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

caged <- ipea$caged |>
  mutate(var = RETURN(value, n = 12)*100)

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

ipca_expec_margem <- filter(bc$expec_ipca_m, base::as.Date(paste0("01/", DataReferencia), format = "%d/%m/%Y") == last(ipca_nucleos$date)) |>
  filter(Data == base::max(Data) & baseCalculo == 0)

ipca_expec_margem_top5 <- filter(bc$expec_ipca_m_top5, base::as.Date(paste0("01/", DataReferencia), format = "%d/%m/%Y") == last(ipca_nucleos$date)) |>
  filter(Data == base::max(Data) & tipoCalculo == "C")

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

# Pass Through
## Criando matrizes que guardarão coeficientes e desvios-padrões

# janela <- 48 # número de meses da janela
#
# cambio_m <- bc$cambio$ptax_usd |>
#   dplyr::group_by(year_month = format(date, "%Y-%m")) |>
#   dplyr::slice(dplyr::n()) |>
#   dplyr::filter(date > dplyr::first(ipca_nucleos$date)) |>
#   dplyr::select(3, 2) |>
#   dplyr::rename(date = year_month)
#
# ibc_pass <- ibc |>
#   dplyr::mutate(date = lubridate::as_date(date)) |>
#   dplyr::mutate(year_month = format(date, "%Y-%m")) |>
#   dplyr::mutate(date = year_month) |>
#   dplyr::select(date, ibcbr)
#
#
# dados_pass <- dplyr::full_join(dplyr::bind_cols(date = format(ipca_nucleos$date, "%Y-%m"), ipca = ipca_nucleos$ipca), cambio_m, by = dplyr::join_by(date)) |>
#   dplyr::full_join(ibc_pass, dplyr::join_by(date)) |>
#   dplyr::filter(!is.na(ipca)) |>
#   dplyr::filter(!is.na(ptax_usd)) |>
#   dplyr::filter(!is.na(ibcbr)) |>
#   dplyr::mutate(ipca = tidyquant::RETURN(ipca),
#                         ptax_usd = tidyquant::RETURN(ptax_usd),
#                         ibcbr = tidyquant::RETURN(ibcbr)) |>
#   dplyr::filter(!is.na(ipca)) |>
#   dplyr::filter(!is.na(ptax_usd)) |>
#   dplyr::filter(!is.na(ibcbr))
#   # dplyr::mutate(ipca = dplyr::case_when(ipca <= 0 ~ 10^-20, TRUE ~ ipca))
#
# coefs <- matrix(NA, ncol = 1, nrow = length(dados_pass$date) - janela)
# sd <- matrix(NA, ncol = 1, nrow = length(dados_pass$date) - janela)
#
# # Loop
#
# for (i in 1:nrow(coefs)) {
#
#   pass_ecm <- urca::ca.jo(dados_pass[((1 + i - 1):(janela + i - 1)), 2:3], c("eigen"), ecdet = c("const"), K = 2, spec = ("transitory"), season = NULL, dumvar = NULL)
#
#   pass_ecm_ols <- urca::cajools(pass_ecm)
#
#   coefs[i,] <- stats::coef(pass_ecm_ols)[6]
#
#   sd[i,] <- lmtest::coeftest(pass_ecm_ols)[30]
#
# }
#
# pass_through <- tibble(date = seq.Date(from = dplyr::nth(as.Date(paste0(dados_pass$date, "-01")), janela + 1), to = dplyr::last(as.Date(paste0(dados_pass$date, "-01"))), by = "month"),
#                   ic1 = coefs[, 1] - sd[, 1], pass_through = coefs[, 1], ic2 = coefs[, 1] + sd[, 1])



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

# Monetário

expec_selic_2024 <- bc$expec_selic |>
  filter(DataReferencia == 2024 & baseCalculo == 0)

expec_selic_2025 <- bc$expec_selic |>
  filter(DataReferencia == 2025 & baseCalculo == 0)

expec_selic_2026 <- bc$expec_selic |>
  filter(DataReferencia == 2026 & baseCalculo == 0)

fed_funds <- ipea$fed_funds |>
  filter(date > "2003-01-01") |>
  right_join(y = bc$selic$meta, join_by(date == date)) |>
  filter(!is.na(code)) |>
  mutate(diff = ((1 + meta/100)/(1 + value/100) - 1)*100)

yc_anbima <- GetTDData::get.yield.curve()

# Fiscal

prim <- filter(bc$fiscal$Governos_Municipais_primario, date > "2003-01-01") |>
  full_join(filter(bc$fiscal$Empresas_Estatais_primario, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Governo_Federal_primario, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Governos_Estaduais_primario, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$INSS_primario, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Setor_Publico_Consolidado_primario, date > "2003-01-01"))

juros <- filter(bc$fiscal$Governos_Municipais_juros, date > "2003-01-01") |>
  full_join(filter(bc$fiscal$Empresas_Estatais_juros, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Governo_Federal_juros, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Governos_Estaduais_juros, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Setor_Publico_Consolidado_juros, date > "2003-01-01"))

nominal <- filter(bc$fiscal$Governos_Municipais_nominal, date > "2003-01-01") |>
  full_join(filter(bc$fiscal$Empresas_Estatais_nominal, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Governo_Federal_Nominal, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Governos_Estaduais_nominal, date > "2003-01-01")) |>
  full_join(filter(bc$fiscal$Setor_Publico_Consolidado_nominal, date > "2003-01-01"))

# Externo

yc_us <- avg






# Application UI ---------------------------------------------------------------

ui <- dashboardPage(

  dark = TRUE,

  help = NULL,

  # skin = "blue",

  # Dashboard Header (App name) ------------------------------------------------
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Conjuntura BR")#,
      # img(src = "reta_logo.png", height = 20, width = 30)
    ),
    titleWidth = "300px",

    # Message box top-right corner
    rightUi = dropdownMenu(
      type = "tasks",
      icon = icon("circle-chevron-down"),
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
      menuItem("Inflação", tabName = "inflação", icon = icon("chart-pie")),
      menuItem("Política Monetária", tabName = "mon", icon = icon("dollar-sign")),
      menuItem("Fiscal", tabName = "fiscal", icon = icon("scale-unbalanced")),
      menuItem("Externo", tabName = "externo", icon = icon("globe")),

      sliderInput(inputId = "slider_atividade", label = "Atividade", min = base::as.Date("1996-01-02"), max = base::as.Date(ymd), value =  base::as.Date("2015-01-01")),
      sliderInput(inputId = "slider_emprego", label = "Emprego", min = base::as.Date("2012-01-02"), max = base::as.Date(ymd), value =  base::as.Date("2015-01-01")),
      sliderInput(inputId = "slider_inf", label = "Inflação",  min = base::as.Date("1996-01-02"), max = Sys.Date(), value =  base::as.Date("2015-01-01")),
      sliderInput(inputId = "slider_mon", label = "Monetária", min = base::as.Date("1996-01-02"), max = Sys.Date(), value =  base::as.Date("2015-01-01")),
      sliderInput(inputId = "slider_fisc", label = "Fiscal", min = base::as.Date("2004-01-02"), max = Sys.Date(), value =  base::as.Date("2015-01-01")),
      sliderInput(inputId = "slider_ext", label = "Externo", min = base::as.Date("1985-01-02"), max = Sys.Date(), value =  base::as.Date("2015-01-01"))
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
          infoBox(title = "PIB (%) Margem", value = number(last(delta_pib$delta_pib_margem)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-line")),
          infoBox(title = "PIB (%) Trimestral", value = number(last(delta_pib$delta_pib_trimestral)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-line")),
          infoBox(title = "PIB (%) Anual", value = number(last(delta_pib$delta_pib_anual)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-line")),
          infoBox(title = "IBC-BR Margem (%)", value = number(last(ibc$delta)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%")),
          infoBox(title = "PMC Receita Vendas (%)", value = number(last(filter(pmc_margem, pmc_margem$`Tipos de índice (Código)` == 56735)$Valor), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("file-invoice-dollar")),
          infoBox(title = "PMC Volume Vendas (%)", value = number(last(filter(pmc_margem, pmc_margem$`Tipos de índice (Código)` == 56736)$Valor), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("file-invoice-dollar")),
          infoBox(title = "PIM Margem (%)", value = number(last(pim_margem$Valor), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("industry")),
          infoBox(title = "Hiato Hamilton (2017) (%)", value = number(last(gap_hamilton$gap), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Hiato HP Filter (%)", value = number(last(gap_hp$gap), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Hiato Média MA, HP e Hamilton", value = number(last(gap_avg$average), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Utilização da Capacidade Instalada", value = number(last(bc$uci$uci), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("industry")),
          infoBox(title = "Mediana Expectativa Focus 2024", value = number(last(expec_pib_2024$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision"))
        ),
        fluidRow(
          box(plotOutput("plot_gdp"), title = "PIB Real", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_gdp_open"), title = "PIB Setores", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_gap_ma"),  title = "Hiato do Produto Decomposição MA", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_gap_h"),  title = "Hiato do Produto Hamilton (2017)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_gap_hp"),  title = "Hiato do Produto Filtro HP", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_gap_avg"),  title = "Hiato do Produto Média", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_uci"),  title = "Utilização da Capacidade Intalada (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_pmc"),  title = "PMC - Delta (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_pim"),  title = "PIM - Delta (%)", collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE, status = "primary"),
          box(plotOutput("plot_expec"), title = "Mediana Expectativa Focus - PIB", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )

      ),

      # Emprego
      tabItem(
        tabName = "emprego",
        fluidRow(
          infoBox(title = "População", value = number(last(pop$Valor), big.mark = ".", decimal.mark = ","), icon = icon("people-group")),
          infoBox(title = "PIA (População em Idade Ativa)", value = number(last(pian$Valor), big.mark = ".", decimal.mark = ","), icon = icon("people-group")),
          infoBox(title = "PEA (População Economicamente Ativa)", value = number(last(pean$Valor), big.mark = ".", decimal.mark = ","), icon = icon("person-running")),
          infoBox(title = "PO (População Ocupada)", value = number(last(pon$Valor), big.mark = ".", decimal.mark = ","), icon = icon("briefcase")),
          infoBox(title = "PD (População Desocupada)", value = number(last(pdn$Valor), big.mark = ".", decimal.mark = ","), icon = icon("person-praying")),
          infoBox(title = "Taxa de Desemprego % (PD/PEA)", value = number(last(desemp$tx_desemp)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-column")),
          infoBox(title = "Variação Interanual Saldo Caged", value = number(last(caged$var)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-column"))
        ),

        fluidRow(
          box(plotOutput("plot_desemp"), title = "Taxa de Desemprego %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_pnea"), title = "PNEA % - Pop. Não Econ. Ativa %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_pea"), title = "PEA % - Pop. Econ. Ativa %", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_gap_desemp"), title = "Hiato do Desemprego (%)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_categ"), title = "Evolução Categorias de Emprego (%)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_rend_categ"), title = "Evolução Rendimento Categorias de Emprego R$", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_caged"), title = "Caged saldo e variação mesmo mês ano passado", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )

      ),

      # Inflação
      tabItem(
        tabName = "inflação",
        fluidRow(
          infoBox(title = "IPCA Margem", value = number(last(ipca_nucleos$ipca), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("tag")),
          infoBox(title = "IPCA Margem Expectativa", value = number(ipca_expec_margem$Mediana, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision")),
          infoBox(title = "IPCA Margem Expectativa Top 5", value = number(ipca_expec_margem_top5$Mediana, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision")),
          infoBox(title = "Inércia", value = number(dplyr::last(inercia$ar1), accuracy = 0.01, big.mark = ".", decimal.mark = ","), icon = icon("lines-leaning")),
          infoBox(title = "índice de Difusão", value = number(dplyr::last(ipca_difu$diffusion), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("users-rays")),
          infoBox(title = "IPCA Acumulado Ano", value = number(dplyr::last(ipca_acum$annual), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-line")),
          infoBox(title = "IPCA Acumulado 12m", value = number(dplyr::last(ipca_yearly$yearly), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("chart-line")),
          infoBox(title = "Mediana Expectativas 2024", value = number(dplyr::last(expec_ipca_2024$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision")),
          infoBox(title = "Mediana Expectativas 2025", value = number(dplyr::last(expec_ipca_2025$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision")),
          infoBox(title = "Mediana Expectativas 2026", value = number(dplyr::last(expec_ipca_2026$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision")),
          infoBox(title = "Mediana Expectativas 2024 Top 5 M", value = number(dplyr::last(expec_ipca_top5_2024$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision")),
          infoBox(title = "Inflação Implícita 1 ano (%)", value = number(filter(yc_anbima, type == "implicit_inflation" & n.biz.days == 252)$value, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("eye-low-vision"))
          # infoBox(title = "Pass Through", value = number(dplyr::last(pass_through$pass_through)*100, accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("fill-drip"))

        ),

        fluidRow(
          box(plotOutput("plot_ipca_nucleos"), title = "IPCA e Núcleos Selecionados", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_inercia"), title = "Inércia IPCA", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_desag"), title = "IPCA - Grupos", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_difu"), title = "índice de Difusão", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_acum"), title = "IPCA Acumulado no Ano", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_expec"), title = "Expectativas IPCA (Mediana)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_expec_top5"), title = "Expectativas IPCA Top 5 (Mediana)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_expec_median_mean"), title = "Diferença Média - Mediana expectativas 2024", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ipca_expec_median_mean_25"), title = "Diferença Média - Mediana expectativas 2025", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )
      ),
      # Monetário
      tabItem(
        tabName = "mon",
        fluidRow(
          infoBox(title = "Selic Meta", value = number(last(bc$selic$meta$meta), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("dollar-sign")),
          infoBox(title = "Focus 2024", value = number(last(expec_selic_2024$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("dollar-sign")),
          infoBox(title = "Focus 2025", value = number(last(expec_selic_2025$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("dollar-sign")),
          infoBox(title = "Focus 2026", value = number(last(expec_selic_2026$Mediana), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("dollar-sign")),
          infoBox(title = "Diferencial de Juros BR - EUA", value = number(last(fed_funds$diff), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("arrows-left-right-to-line")),
          infoBox(title = "Regra de Taylor", value = number(last(ipca_yearly$yearly) + .5*last(gap_avg$average) + .5*(last(ipca_yearly$yearly) - filter(yc_anbima, type == "real_return" & n.biz.days == 2520)$value) + filter(yc_anbima, type == "real_return" & n.biz.days == 2520)$value,
                                                            accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("arrows-left-right-to-line"))

        ),

        fluidRow(
          box(plotOutput("plot_selic"), title = "Selic Meta", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_selic_expec"), title = "Expectativa Selic", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_selic_diff"), title = "Diferencial de Juros", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_yc_anbima"), title = "Curva de Juros de Títulos Públicos (Nominal)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_yc_anbima_real"), title = "Curva de Juros de Títulos Públicos (Real)", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_yc_anbima_implicit"), title = "Curva de Juros de Títulos Públicos (Implícita)", collapsible = T, collapsed = T, solidHeader = T, status = "primary")

        )

      ),
      # Fiscal
      tabItem(
        tabName = "fiscal",
        fluidRow(
          infoBox(title = "Dívida Bruta / PIB (%)", value = number(last(bc$fiscal$div_pib$div_pib), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("circle-exclamation"))
        ),

        fluidRow(
          box(plotOutput("plot_prim"), title = "NFSP Primário", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_nom"), title = "NFSP Nominal", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_juros"), title = "NFSP Juros", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_div_pib"), title = "Razão Dívida Bruta PIB", collapsible = T, collapsed = T, solidHeader = T, status = "primary")
        )
      ),
      #Externo
      tabItem(
        tabName = "externo",
        fluidRow(
          infoBox(title = "BRL/USD", value = number(last(bc$cambio$ptax_usd$ptax_usd), accuracy = 0.01, big.mark = ".", decimal.mark = ","), icon = icon("dollar-sign")),
          infoBox(title = "BRL/EUR", value = number(last(bc$cambio$ptax_eur$ptax_eur), accuracy = 0.01, big.mark = ".", decimal.mark = ","), icon = icon("euro-sign")),
          infoBox(title = "Fed Funds Rate", value = number(last(fed_funds$value), accuracy = 0.01, big.mark = ".", decimal.mark = ",", suffix = "%"), icon = icon("dollar-sign"))
        ),

        fluidRow(
          box(plotOutput("plot_ptax"), title = " PTAX BRL/USD e BRL/EUR", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_yc_us"), title = "Curva de Juros Nominal EUA", collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_bp"), title = "Balança de Pagamentos",  collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_tc_pib"), title = "Saldo em Transações Correntes/PIB (%)",  collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_ied"), title = "Investimento Estrangeiro Direto/PIB (%)",  collapsible = T, collapsed = T, solidHeader = T, status = "primary"),
          box(plotOutput("plot_reserv"), title = "Reservas dólar",  collapsible = T, collapsed = T, solidHeader = T, status = "primary")

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

    pib_data <- reactive({
      pib1 <- pib |>
        filter(`Setores e subsetores (Código)` == 90707) |>
        mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd), by = "quarter")) |>
        filter(date >= input$slider_atividade)

      return(pib1)
    })

    pib_data() |>
      ggplot(mapping = aes(x = date, y = Valor)) +
      geom_line(aes(y = Valor), linewidth = 1.5, color = 'blue') +
      labs(x = NULL, y = 'Número índice') +
      theme_economist()


  })

  output$plot_gap_ma <- renderPlot({

    gap_data <- reactive({

      gap1 <- gap |>
        mutate(date = dates_helper$date) |>
        filter(date >= input$slider_atividade)

      return(gap1)

    })

    gap_data() |>
      ggplot(mapping = aes(x = date, y = gap*100)) +
      geom_line(aes(y = gap*100), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_gap_h <- renderPlot({

    gap_hamilton_data <- reactive({

      gap_hamilton1 <- gap_hamilton |>
        mutate(gap = y.cycle/y.trend*100) |>
        mutate(date = dates_helper$date) |>
        filter(date >= input$slider_atividade)

      return(gap_hamilton1)

    })

    gap_hamilton_data() |>
      ggplot(mapping = aes(x = date, y = gap)) +
      geom_line(aes(y = gap), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_gap_hp <- renderPlot({

    gap_hp_data <- reactive({

      gap_hp1 <- gap_hp |>
        mutate(gap = cycle/trend*100) |>
        mutate(date = dates_helper$date) |>
        filter(date >= input$slider_atividade)

      return(gap_hp1)

    })

    gap_hp_data() |>
      ggplot(mapping = aes(x = date, y = gap)) +
      geom_line(aes(y = gap), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(x = NULL, y = NULL) +
      theme_economist()


  })

  output$plot_gap_avg <- renderPlot({

    gap_avg_data <- reactive({

      gap_avg1 <- gap_avg |>
        mutate(date =  dates_helper$date) |>
        filter(date >= input$slider_atividade)

      return(gap_avg1)

    })

    gap_avg_data() |>
      ggplot(mapping = aes(x = date, y = average)) +
      geom_line(aes(y = average), linewidth = .7, color = 'black') +
      geom_hline(yintercept = 0, color = 'red') +
      labs(x = NULL, y = NULL) +
      theme_economist()


  })

  output$plot_gdp_open <- renderPlot({

    pib_open <- reactive({

      pib_open1 <- pib |>
        group_by(`Setores e subsetores (Código)`) |>
        mutate(deseasonalized = seasadj(decompose(ts(Valor, frequency = 4)))) |>
        mutate(date = seq.Date(from = dplyr::first(dates), to  = ymd(ymd), by = "quarter")) |>
        filter(`Setores e subsetores (Código)` == c(90687, 90691, 90696, 93405, 93406, 93404, 93407, 93408)) |>
        filter(date >= input$slider_atividade)

      return(pib_open1)

    })

    pib_open() |>
      ggplot(mapping = aes(x = date, y = deseasonalized, color = `Setores e subsetores`)) +
      geom_line(linewidth = 1) +
      facet_wrap(ncol = 4, vars(`Setores e subsetores`), scales = "free") +
      labs(y = NULL, color = NULL) +
      theme_economist()

  })

  output$plot_uci <- renderPlot({

    uci_data <- reactive({

      uci1 <- bc$uci |>
        filter(date >= input$slider_atividade)

      return(uci1)

    })

    uci_data() |>
      ggplot(aes(x = date,  y = uci)) +
      geom_line(linewidth = 1, color = "gray") +
      geom_smooth(color = 'red') + # subset(bc$uci, date > uci_data$date)
      labs(x = NULL, Y = NULL) +
      theme_economist()


  })

  output$plot_pmc <- renderPlot({

    pmc_margem_data <- reactive({

      pmc_margem1 <- pmc_margem |>
        filter(dates >= input$slider_atividade)

      return(pmc_margem1)

    })

    pmc_margem_data() |>
      ggplot(aes(x = dates, y = Valor, color = `Tipos de índice`)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, Y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_pim <- renderPlot({

    pim_margem_data <- reactive({

      pim_margem1 <- pim_margem |>
      filter(dates >= input$slider_atividade)

      return(pim_margem1)

    })

    pim_margem_data() |>
      ggplot(aes(x = dates, y = Valor)) +
      geom_line(linewidth = 1, color = "brown") +
      labs(x = NULL, Y = NULL) +
      theme_economist()

  })

  output$plot_expec <- renderPlot({
    expec_pib_2024 |>
      bind_rows(expec_pib_2025) |>
      bind_rows(expec_pib_2026) |>
      filter(baseCalculo == 0) |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, Y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_desemp <- renderPlot({

    desemp_data <- reactive({

      desemp1 <- desemp |>
        mutate(trend = gap_des$trend_ma) |>
        filter(dates >= input$slider_emprego)

      return(desemp1)

    })

    desemp_data() |>
      ggplot(aes(x = dates, y = tx_desemp*100)) +
      geom_line(linewidth = 1, color = "blue") +
      geom_line(aes(x = dates, y = trend*100), linewidth = 1, color = "black") +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_pnea <- renderPlot({

    pneap_data <- reactive({

      pneap_data1 <- pneap |>
        filter(dates >= input$slider_emprego)

      return(pneap_data1)

    })

    pneap_data() |>
      ggplot(aes(x = dates, y = Valor)) +
      geom_line(linewidth = 1, color = "brown") +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_pea <- renderPlot({

    peap_data <- reactive({

      peap1 <- peap |>
        filter(dates >= input$slider_emprego)

      return(peap1)

    })

    peap_data() |>
      ggplot(aes(x = dates, y = Valor)) +
      geom_line(linewidth = 1, color = "yellow") +
      labs(titles = "PEA % - Pop. Econ. Ativa", x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_gap_desemp <- renderPlot({

    gap_desemp_ham_data <- reactive({

      gap_desemp_ham1 <- gap_desemp_ham |>
        filter(dates >= input$slider_emprego)

      return(gap_desemp_ham1)

    })

    gap_desemp_ham_data() |>
      ggplot(aes(x = dates, y = gap*100)) +
      geom_line(linewidth = 1, color = "blue") +
      geom_hline(yintercept = 0, color = 'red') +
      labs(x = NULL, y = NULL ) +
      theme_economist()

  })

  output$plot_categ <- renderPlot({

    categ_data <- reactive({

      categ1 <- categ |>
        filter(dates >= input$slider_emprego)

      return(categ1)

    })

    categ_data() |>
      ggplot(aes(x = dates)) +
      geom_line(aes(y = priv_clt, color = "priv_clt"), linewidth = 1) +
      geom_line(aes(y = priv_s_clt, color = "priv_s_clt"), linewidth = 1) +
      geom_line(aes(y = domest, color = "domest"), linewidth = 1) +
      geom_line(aes(y = public, color = "public"), linewidth = 1) +
      geom_line(aes(y = empregador, color = "empregador"), linewidth = 1) +
      geom_line(aes(y = auton, color = "auton"), linewidth = 1) +
      geom_line(aes(y = fam, color = "fam"), linewidth = 1) +
      labs(x = NULL, y = NULL, color =NULL) +
      theme_economist()

  })

  output$plot_rend_categ <- renderPlot({

    r_categ_data <- reactive({

      r_categ1 <- r_categ |>
        filter(dates >= input$slider_emprego)

      return(r_categ1)

    })

    r_categ_data() |>
      ggplot(aes(x = dates)) +
      geom_line(aes(y = r_priv_clt, color = "r_priv_clt"), linewidth = 1) +
      geom_line(aes(y = r_priv_s_clt, color = "r_priv_s_clt"), linewidth = 1) +
      geom_line(aes(y = r_domest, color = "r_domest"), linewidth = 1) +
      geom_line(aes(y = r_public, color = "r_public"), linewidth = 1) +
      geom_line(aes(y = r_empregador, color = "r_empregador"), linewidth = 1) +
      geom_line(aes(y = r_auton, color = "r_auton"), linewidth = 1) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()

  })

  output$plot_caged <- renderPlot({

    caged_data <- reactive({

      caged1 <- caged |>
        filter(date >= input$slider_emprego)

      return(caged1)

    })

    caged_data() |>
      ggplot(aes(x = date, y = value)) +
      geom_col() +
      geom_text(aes(label = number(var*100, accuracy = 0.1, big.mark = ".", decimal.mark = ",", suffix = "%")), color = "red", vjust = 1, size = 3) +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_ipca_nucleos <- renderPlot({

    ipca_nucleos_tidy_data <- reactive({

      ipca_nucleos_tidy1 <- ipca_nucleos_tidy |>
        filter(date >= input$slider_inf)

      return(ipca_nucleos_tidy1)

    })

    ipca_nucleos_tidy_data() |>
      ggplot(aes(x = date, y = value, color = name)) +
      geom_line() +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_desag <- renderPlot({


    ipca_desag_tidy_data <- reactive({

      ipca_desag_tidy1 <- ipca_desag_tidy |>
        filter(date >= input$slider_inf)

      return(ipca_desag_tidy1)

    })

    ipca_desag_tidy_data() |>
      ggplot(aes(x = date, y = value, color = name)) +
      geom_line() +
      geom_smooth() +
      facet_wrap(ncol = 2, vars(name), scales = "free") +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_acum <- renderPlot({

    ipca_acum_data <- reactive({

      ipca_acum1 <- ipca_acum |>
        filter(`year(date)` >= year(input$slider_inf))

      return(ipca_acum1)

    })

    ipca_acum_data() |>
      ggplot(aes(x = `year(date)`, y = annual)) +
      geom_line(linewidth = 1, color = "darkblue") +
      geom_point(color = "red", size = 2) +
      geom_text(aes(label = round(annual, 2)), vjust = -0.5, hjust = -0.5, color = "black")  +
      labs(x = NULL, y = NULL) +
      theme_economist()
  })

  output$plot_ipca_inercia <- renderPlot({

    inercia_data <- reactive({

      inercia1 <- inercia |>
        filter(date >= input$slider_inf)

      return(inercia1)

    })

    inercia_data() |>
      ggplot(aes(x = date)) +
      geom_ribbon(aes(ymin = ic1, ymax = ic2)) +
      geom_line(aes(y = ar1), linewidth = 1, color = "grey70") +
      geom_hline(aes(yintercept = mean(ar1)), color = "red") +
      labs(x = NULL, y = NULL) +
      theme_economist()
  })

  output$plot_ipca_difu <- renderPlot({

    ipca_difu_data <- reactive({

      ipca_difu1 <- ipca_difu |>
        filter(dates >= input$slider_inf)

      return(ipca_difu1)

    })

    ipca_difu_data() |>
      ggplot(aes(x = dates, y = diffusion)) +
      geom_line(linewidth = 1, color = "gray35") +
      geom_hline(aes(yintercept = mean(diffusion)), color = "red") +
      labs(x = NULL, y = NULL) +
      theme_economist()
  })

  # output$plot_pass_through <- renderPlot({
  #
  #   pass_through_data <- reactive({
  #
  #     pass <- pass_through |>
  #       filter(date >= input$slider_inf)
  #
  #     return(pass)
  #
  #   })
  #
  #   pass_through_data() |>
  #     ggplot(aes(x = date)) +
  #     geom_ribbon(aes(ymin = ic1, ymax = ic2)) +
  #     geom_line(aes(y = pass_through), linewidth = 1, color = "grey70") +
  #     geom_hline(aes(yintercept = mean(pass_through)), color = "red") +
  #     labs(x = NULL, y = NULL) +
  #     theme_economist()
  #
  # })

  output$plot_ipca_expec <- renderPlot({
    bc$expec_ipca |>
      filter(Data > Sys.Date() - 180 & baseCalculo == 0) |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_expec_top5 <- renderPlot({
    expec_ipca_top5 |>
      filter(Data > Sys.Date() - 180 & tipoCalculo == "M") |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()
  })

  output$plot_ipca_expec_median_mean <- renderPlot({

    expec_ipca_2024 |>
      filter(baseCalculo == 0) |>
      ggplot(aes(x = Data, y = Media - Mediana)) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = NULL, title = NULL) +
      theme_economist()

  })

  output$plot_ipca_expec_median_mean_25 <- renderPlot({

    expec_ipca_2025 |>
      filter(baseCalculo == 0) |>
      ggplot(aes(x = Data, y = Media - Mediana )) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = NULL, title = NULL) +
      theme_economist()

  })

  output$plot_selic <- renderPlot({

    selic_meta_data <- reactive({

      selic_meta1 <- bc$selic$meta |>
        filter(date >= input$slider_mon)

      return(selic_meta1)

    })

    selic_meta_data() |>
      ggplot(aes(x = date, y = meta)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, y = "SELIC (%)") +
      theme_economist()

  })

  output$plot_selic_expec <- renderPlot({

    bc$expec_selic |>
      filter(baseCalculo == 0 & DataReferencia == c(2024, 2025, 2026)) |>
      ggplot(aes(x = Data, y = Mediana, color = DataReferencia)) +
      geom_line(linewidth = 1) +
      labs(x = NULL, y = "SELIC (%)", color = NULL) +
      theme_economist()

  })

  output$plot_selic_diff <- renderPlot({

    fed_funds_data <- reactive({

      fed_funds1 <- fed_funds |>
        filter(date >= input$slider_mon)

      return(fed_funds1)

    })

    fed_funds_data() |>
      ggplot(aes(x = date, y = diff)) +
      geom_line(linewidth = 1) +
      geom_hline(aes(yintercept = mean(diff)), color = "red") +
      labs(x = NULL, y = "Diferencial (%)") +
      theme_economist()

  })

  output$plot_yc_anbima <- renderPlot({

    yc_anbima |>
      filter(type == "nominal_return") |>
      ggplot(aes(x = ref.date, y = value)) +
      geom_line(linewidth = 1.3, color = "darkgreen") +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_yc_anbima_real <- renderPlot({

    yc_anbima |>
      filter(type == "real_return") |>
      ggplot(aes(x = ref.date, y = value)) +
      geom_line(linewidth = 1.3, color = "darkred") +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_yc_anbima_implicit <- renderPlot({

    yc_anbima |>
      filter(type == "implicit_inflation") |>
      ggplot(aes(x = ref.date, y = value)) +
      geom_line(linewidth = 1.3, color = "grey30") +
      labs(x = NULL, y = NULL) +
      theme_economist()

  })

  output$plot_prim <- renderPlot({

    prim_data <- reactive({

      prim1 <- prim |>
        dplyr::group_by(year(date)) |>
        dplyr::summarise(across(c(2:7), list(mean))) |>
        tidyr::pivot_longer(cols = c(2:7), names_to = "nivel") |>
        dplyr::filter(`year(date)` >= year(input$slider_fisc))

      return(prim1)

    })

    prim_data() |>
      ggplot(aes(x = `year(date)`, y = value, color = nivel)) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()

  })

  output$plot_nom <- renderPlot({

    nominal_data <- reactive({

      nominal1 <- nominal |>
        dplyr::group_by(year(date)) |>
        dplyr::summarise(across(c(2:6), list(mean))) |>
        tidyr::pivot_longer(cols = c(2:6), names_to = "nivel") |>
        dplyr::filter(`year(date)` >= year(input$slider_fisc))

      return(nominal1)

    })

    nominal_data() |>
      ggplot(aes(x = `year(date)`, y = value, color = nivel)) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()

  })

  output$plot_juros <- renderPlot({

    juros_data <- reactive({

      juros1 <- juros |>
        dplyr::group_by(year(date)) |>
        dplyr::summarise(across(c(2:6), list(mean))) |>
        tidyr::pivot_longer(cols = c(2:6), names_to = "nivel") |>
        dplyr::filter(`year(date)` >= year(input$slider_fisc))

      return(juros1)

    })

    juros_data() |>
      ggplot(aes(x = `year(date)`, y = value, color = nivel)) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()

  })

  output$plot_div_pib <- renderPlot({

    div_pib_data <- reactive({

      div_pib1 <- bc$fiscal$div_pib |>
        filter(date >= input$slider_fisc)

      return(div_pib1)

    })

    div_pib_data() |>
      filter(!is.na(div_pib)) |>
      ggplot(aes(x = date, y = div_pib)) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = "(%)") +
      theme_economist()

  })

  output$plot_ptax <- renderPlot({

    ptax_data <- reactive({

      ptax1 <-  bc$cambio$ptax_usd |>
        dplyr::right_join(bc$cambio$ptax_eur) |>
        tidyr::pivot_longer(cols = c(2:3), names_to = "ptax") |>
        dplyr::filter(date >= input$slider_ext)

      return(ptax1)

    })

    ptax_data() |>
      ggplot(aes(x = date, y = value, color = ptax)) +
      geom_line(linewidth = 1.3) +
      labs(x = NULL, y = NULL, color = NULL) +
      theme_economist()

  })

  output$plot_yc_us <- renderPlot({

    yc_us |>
      filter(timestamp == max(timestamp)) |>
      ggplot(aes(x = duration, y = value)) +
      # ggplot(aes(x = factor(duration, levels = unique(duration)), y = value)) +
      geom_line(aes(group = 1), linewidth = 1.3) +
      labs(x = NULL, y = NULL, title = "Treasury Yield Curve") +
      theme_economist()
      # ylim(3.5,5.5)

  })

  output$plot_bp <- renderPlot({

    bp_data <- reactive({

      bp1 <- bc$bp$tc |>
        dplyr::left_join(bc$bp$k) |>
        tidyr::pivot_longer(cols = c(2:3), names_to = "name") |>
        dplyr::filter(date >= input$slider_ext)

      return(bp1)

    })

    bp_data() |>
      ggplot(aes(x = date, y = value, color = name)) +
      geom_col() +
      labs(color = NULL) +
      theme_economist()

  })

  output$plot_tc_pib <- renderPlot({

    tc_pib_data <- reactive({

      tc_pib1 <- bc$bp$tc_pib |>
        filter(date >= input$slider_ext)

      return(tc_pib1)

    })

    tc_pib_data() |>
      ggplot(aes(x = date, y = tc_pib)) +
      geom_col() +
      theme_economist()

  })

  output$plot_ied <- renderPlot({

    ied_data <- reactive({

      ied1 <- bc$bp$ied_pib |>
        filter(date >= input$slider_ext)

      return(ied1)

    })

    ied_data() |>
      ggplot(aes(x = date, y = ied_pib)) +
      geom_col() +
      theme_economist()

  })

  output$plot_reserv <- renderPlot({

    reserv_data <- reactive({

      reserv1 <- bc$bp$reserv |>
        filter(date >= input$slider_ext)

      return(reserv1)

    })

    reserv_data() |>
      ggplot(aes(x = date, y = reserv)) +
      geom_area() +
      theme_economist()

  })

}# End of Server

# Shiny App --------------------------------------------------------------------

shinyApp(ui, server)





























