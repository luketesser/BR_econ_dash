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
#' @examples
#' 
#' 
#' 


sidra_data <- function(){
  
  # Define the period to download the data automatically setting the end to be as recent as possible.
  
  year <- seq(from = 1980, to = lubridate::year(Sys.Date()), by = 1)
  
  quarter <- c('01', '02', '03', '04')
  
  month <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

  # Create data frames with all combinations of year and quarter and year and month
  
  period <- expand.grid(year = year, quarter = quarter)
  
  period_m <- expand.grid(year = year, month = month)
  
  # Combine the columns into a single character column
  
  period <- paste0(period$year, period$quarter, sep = '')
  
  period_m <- paste0(period_m$year, period_m$month, sep = '')
  
  # Donwload data through sidra API
    
  pib <- sidrar::get_sidra(x = 1620, period = period) |> 
    dplyr::mutate(dates = as.Date(paste0(`Trimestre (Código)`, '01'), format = '%Y%m%d')) |> 
    dplyr::arrange(dates) 
    
  pop <- sidrar::get_sidra(x = 6462, period = period, variable = '606') |> 
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
  
  results <- list(pib = pib, pop = pop, pmc1 = pmc1, pmc2 = pmc2, pmc3 = pmc3, 
                  pmc4 = pmc4, pim = pim)
  
  return(results)
  
}


# saveRDS()
# readRDS()