#' avg_get
#'
#' @return
#' @export
#'
#'
avg_get <- function(){

  alphavantager::av_api_key("2YPWNULLH2LT7U9V")

 ty_1mo <-  alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "1month", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 30))
 ty_2mo <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "2month", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 60))
 ty_3mo <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "3month", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 90))
 ty_6mo <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "6month", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 180))
 ty_1y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "1year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 365))
 ty_2y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "2year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 730))
 ty_3y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "3year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 1095))
 ty_5y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "5year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 1825))
 ty_7y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "7year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 2555))
 ty_10y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "10year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 3650))
 ty_20y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "20year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 7300))
 ty_30y <- alphavantager::av_get(av_fun = "TREASURY_YIELD", maturity = "30year", interval = "daily") |> mutate(value = as.double(value), duration = base::as.Date(timestamp + 10950))

  yc_data <- full_join(ty_1mo, ty_2mo) |> full_join(ty_3mo) |> full_join(ty_6mo) |> full_join(ty_1y) |> full_join(ty_2y) |> full_join(ty_3y) |> full_join(ty_5y) |> full_join(ty_7y) |>
    full_join(ty_10y) |> full_join(ty_20y) |> full_join(ty_30y)

  return(yc_data)

}

# devtools::load_all(); avg <- avg_get(); saveRDS(avg, 'Data/avg.rds')
