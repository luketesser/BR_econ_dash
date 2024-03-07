#' sidra_data
#'
#' @description
#' Downloads data of a set of economic measures from the IBGE SIDRA API.
#'
#'
#' @param name description
#'
#'
#' @return
#' @export
#'
#'
#'
#'
#'


sidra_data <- function(){

  # Define the period to download the data automatically setting the end to be as recent as possible.

  year <- seq(from = 1980, to = lubridate::year(Sys.Date()), by = 1)

  quarter <- c('01', '02', '03', '04')

  # month_of_quarter <- c('03', '06', '09', '12')

  month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

  # Create data frames with all combinations of year and quarter and year and month

  period <- expand.grid(year = year, quarter = quarter)

  # period_q_of_m <- expand.grid(year = year, quarter = month_of_quarter)

  period_m <- expand.grid(year = year, month = month)

  # Combine the columns into a single character column

  period <- paste0(period$year, period$quarter, sep = '')

  # period_q_of_m <- paste0(period_q_of_m$year, period_q_of_m$quarter, sep = '')

  period_m <- paste0(period_m$year, period_m$month, sep = '')

  # periods <- tibble(as.Date(paste0(period, '01'), format = '%Y%m%d'), as.Date(paste0(period_q_of_m, '01'), format = '%Y%m%d'))

  # colnames(periods) = c("period", "period_q_of_m")

  # Donwload data through sidra API

  pib <- sidrar::get_sidra(x = 1621, period = period) |>
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates) #|>
    # mutate(dates = case_when(date_code == periods$period ~ periods$period_q_of_m))

  pib_v <- sidrar::get_sidra(x = 1846, period = period) |>
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pop <- sidrar::get_sidra(x = 6462, period = period, variable = '606') |>
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pnad <- sidrar::get_sidra(x = 4092, period = period, variable = c(1641, 4104)) |>
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pnad2 <- sidrar::get_sidra(x = 4097, period = period) |>
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pnad3 <- sidrar::get_sidra(x = 5440, period = period) |>
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pmc1 <- sidrar::get_sidra(x = 8880, period = period_m) |> # Receita Nominal e Vol de vendas com. varej.
    dplyr::mutate(dates = as.Date(paste0(`Mês (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pmc2 <- sidrar::get_sidra(x = 8881, period = period_m) |> # Receita Nominal e Vol de vendas com. varej. ampliado
    dplyr::mutate(dates = as.Date(paste0(`Mês (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pmc3 <- sidrar::get_sidra(x = 8882, period = period_m) |> # Receita Nominal e Vol de vendas com. varej. por setor
    dplyr::mutate(dates = as.Date(paste0(`Mês (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pmc4 <- sidrar::get_sidra(x = 8883, period = period_m) |> # Receita Nominal e Vol de vendas com. varej. ampliado por setor
    dplyr::mutate(dates = as.Date(paste0(`Mês (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  pim <- sidrar::get_sidra(x = 8888, period = period_m) |> # Pesquisa Industrial Mensal
    dplyr::mutate(dates = as.Date(paste0(`Mês (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  ipca <- sidrar::get_sidra(x = 7060, period = period_m, variable = 63) |>
    dplyr::mutate(dates = as.Date(paste0(`Mês (Código)`, '01'), format = '%Y%m%d')) |>
    dplyr::arrange(dates)

  results <- list(pib = pib, pib_v = pib_v, pop = pop, pnad = pnad, pnad2 = pnad2, pnad3 = pnad3, pmc1 = pmc1, pmc2 = pmc2, pmc3 = pmc3,
                  pmc4 = pmc4, pim = pim, ipca = ipca)

  return(results)

}

# Put argument to save or not
# devtools::load_all(); sidra <- sidra_data(); saveRDS(sidra, 'Data/sidra.rds')

