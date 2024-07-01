#' avg_get
#'
#' @return
#' @export
#'
#'
avg_get <- function(){

  alphavantager::av_api_key("2YPWNULLH2LT7U9V")

 ty_3mo <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "3month", interval = "daily") |> dplyr::mutate(value = as.double(value), duration = base::as.Date(timestamp + 90))

 ty_2y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "2year", interval = "daily") |> dplyr::mutate(value = as.double(value), duration = base::as.Date(timestamp + 730))

 ty_5y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "5year", interval = "daily") |> dplyr::mutate(value = as.double(value), duration = base::as.Date(timestamp + 1825))

 ty_7y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "7year", interval = "daily") |> dplyr::mutate(value = as.double(value), duration = base::as.Date(timestamp + 2555))

 ty_10y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "10year", interval = "daily") |> dplyr::mutate(value = as.double(value), duration = base::as.Date(timestamp + 3650))

 ty_30y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "30year", interval = "daily") |> dplyr::mutate(value = as.double(value), duration = base::as.Date(timestamp + 10950))

  yc_data <- dplyr::full_join(ty_3mo, ty_2y) |> dplyr::full_join(ty_5y) |> dplyr::full_join(ty_7y) |> dplyr::full_join(ty_10y) |> dplyr::full_join(ty_30y)

  return(yc_data)

}

# devtools::load_all(); avg <- avg_get(); saveRDS(avg, 'Data/avg.rds')

