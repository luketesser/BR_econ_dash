#' avg_get
#'
#' @return
#' @export
#'
#'
avg_get <- function(){

  alphavantager::av_api_key("2YPWNULLH2LT7U9V")

  alphavantager::ty_1mo <- av_get(av_fun = "TREASURY_YIELD", maturity = "1month", interval = "daily") |> mutate(value = as.double(value), duration = "1_month")
  alphavantager::ty_2mo <- av_get(av_fun = "TREASURY_YIELD", maturity = "2month", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "2_month")
  alphavantager::ty_3mo <- av_get(av_fun = "TREASURY_YIELD", maturity = "3month", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "3_month")
  alphavantager::ty_6mo <- av_get(av_fun = "TREASURY_YIELD", maturity = "6month", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "6_month")
  alphavantager::ty_1y <- av_get(av_fun = "TREASURY_YIELD", maturity = "1year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "1_year")
  alphavantager::ty_2y <- av_get(av_fun = "TREASURY_YIELD", maturity = "2year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "2_year")
  alphavantager::ty_3y <- av_get(av_fun = "TREASURY_YIELD", maturity = "3year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "3_year")
  alphavantager::ty_5y <- av_get(av_fun = "TREASURY_YIELD", maturity = "5year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "5_year")
  alphavantager::ty_7y <- av_get(av_fun = "TREASURY_YIELD", maturity = "7year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "7_year")
  alphavantager::ty_10y <- av_get(av_fun = "TREASURY_YIELD", maturity = "10year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "10_year")
  alphavantager::ty_20y <- av_get(av_fun = "TREASURY_YIELD", maturity = "20year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "20_year")
  alphavantager::ty_30y <- av_get(av_fun = "TREASURY_YIELD", maturity = "30year", interval = "daily", from = Sys.Date() - 4, to = Sys.Date()) |> mutate(value = as.double(value), duration = "30_year")

  yc_data <- full_join(ty_1mo, ty_2mo) |> full_join(ty_3mo) |> full_join(ty_6mo) |> full_join(ty_1y) |> full_join(ty_2y) |> full_join(ty_3y) |> full_join(ty_5y) |> full_join(ty_7y) |>
    full_join(ty_10y) |> full_join(ty_20y) |> full_join(ty_30y)

  return(yc_data)

}

# devtools::load_all(); avg <- avg_get(); saveRDS(avg, 'Data/avg.rds')
