#' Yield Curve Risk Premia
#' @description
#' Estimates automatically the implicit risk premia (moving window) in the Brazilian sovereign term structure based on the methodology suggested in the BCB Inflation report of September 2006.
#'
#' @param yield_spread The desired yield spread to analyse. It can be "3 months", "6 months" or "12 months".
#' @param frequency Daily ("daily") or monthly ("monthly") average yields data.
#' @param window Number of bussiness days in the moving window.
#'
#' @return A data set with the estimate intercept and regression parameter of the regression and a graph of the intercept (risk premia).
#'
#'
#'
yield_curve_risk_premia <- function(yield_spread, frequency, window){

  library("bizdays") # This package does not work without loading
  library("ggplot2")

  # Download Data and Organize -------------------------------------------------

  url <- "https://api.data.economatica.com/1/oficial/datafeed/download/1/rJXGF3XnVxnLeyOOYc5ewkt%2F0TRueXMpOoZbz2rKDiQneLd%2F7uTyI3sFe0RsUFoviBVVzL5X4WviGYMWlEBTq%2BTxAXVkDQEnOs3Qoa2P7mc8BDn1SKNqkrCgPbB%2FGtXhmF2wzrBe9Vsi2NRWclz%2Fb%2F0uzpm4ny%2BkwbUpSC1RKy73krOIQiMuH4UZMekw8JFHJYOjIWO1HMm1JzV8AmsI%2BRCdUBLoZ62gkAUOxOgzKBepiT2alT8Qhe5QWbIz%2BV%2BUe%2BB9UOz76jeqgIwqZdVjCxxulflljCEzc9fUjdTX2%2B2%2FeSlVr2mkypLIC1dOOADGT8%2F3u1PAbigCYtGv5ago8Q%3D%3D"

  di_data <- vroom::vroom(file = url, delim = ",", locale = vroom::locale(encoding = "latin1")) |>
    janitor::clean_names() |>
    dplyr::mutate(data = as.Date(data)) |>
    dplyr::mutate(taxa = dplyr::case_when(taxa == "-" ~ NA,
                                          taxa != "-" ~ taxa)) |>
    dplyr::filter(!is.na(taxa)) |>
    dplyr::mutate(maturity = dplyr::case_when(
      substr(ativo, start = 4, stop = 4) == "F" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-01-01")),
      substr(ativo, start = 4, stop = 4) == "G" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-02-01")),
      substr(ativo, start = 4, stop = 4) == "H" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-03-01")),
      substr(ativo, start = 4, stop = 4) == "J" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-04-01")),
      substr(ativo, start = 4, stop = 4) == "K" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-05-01")),
      substr(ativo, start = 4, stop = 4) == "M" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-06-01")),
      substr(ativo, start = 4, stop = 4) == "N" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-07-01")),
      substr(ativo, start = 4, stop = 4) == "Q" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-08-01")),
      substr(ativo, start = 4, stop = 4) == "U" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-09-01")),
      substr(ativo, start = 4, stop = 4) == "V" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-10-01")),
      substr(ativo, start = 4, stop = 4) == "X" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-11-01")),
      substr(ativo, start = 4, stop = 4) == "Z" ~ (paste0("20", substr(ativo, start = 5, stop = 6), "-12-01"))
    ),
    maturity = as.Date(maturity),
    taxa = as.double(taxa)) |>
    dplyr::group_by(data) |>
    dplyr::arrange(maturity) |>
    dplyr::arrange(data) |>
    dplyr::mutate(duration = bizdays::bizdays(from = data, to = maturity, cal = "Brazil/ANBIMA"),
                  volume = as.double(volume_em_moeda_orig_em_milhares),
                  q_negs = as.double(q_negs)) |>
    dplyr::mutate(duration = dplyr::case_when(duration == 0 ~ 1, duration != 0 ~ duration)) |>
    dplyr::ungroup()

  # Build Yield Curve ------------------------------------------------------------

  mats <- c(21, 63, 126, 252)

  dates_di <- di_data |>
    dplyr::select(data) |>
    dplyr::distinct()


  curves_spline <- tibble::tibble(dates_di)

  for (i in 1:length(mats)) {

    curves_spline[, i] <- (di_data |>
                             dplyr::select(data, duration, taxa) |>
                             dplyr::group_by(data) |>
                             dplyr::mutate(yield = stats::spline(x = duration, y = taxa, xout = mats[i], method = "fmm")$y) |>
                             dplyr::summarise(data, yield) |>
                             dplyr::slice(which.max(yield)) |>
                             dplyr::select(yield))$yield

  }

  mats_names <- paste0("v", mats)

  colnames(curves_spline) <- mats_names

  curves_spline <- dates_di |>
    dplyr::bind_cols(curves_spline)

  # Build dataset to run regressions -------------------------------------------

  reg_data <- curves_spline |>
    dplyr::mutate(delta_i_t_3  = tidyquant::RETURN(v21, n = 63),
                  delta_i_t_6  = tidyquant::RETURN(v21, n = 126),
                  delta_i_t_12 = tidyquant::RETURN(v21, n = 252),
                  i_t_3_1      = v63  - v21,
                  i_t_6_1      = v126 - v21,
                  i_t_12_1     = v252 - v21)

  p <- 2 # Parâmetros a serem guardados

  janela <- window # número de dias da janela

  # delta_i_t_3 ~ i_t_3_1 ------------------------------------------------------

  coefs <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data$delta_i_t_3)) - janela)

  sd <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data$delta_i_t_3)) - janela)

  # Loop

  for (i in 1:nrow(coefs)) {

    reg1      <- stats::lm(na.omit(reg_data$delta_i_t_3)[(1 + i - 1):(janela + i - 1)] ~ na.omit(reg_data$i_t_3_1)[(1 + i - 1):(janela + i - 1)])

    coefs[i,] <- stats::coef(reg1)

    sd[i,]    <- lmtest::coeftest(reg1)[,2]

  }

  colnames(coefs) <- c('Intercepto', 'Beta')
  colnames(sd)    <- c('Intercepto', 'Beta')

  results1 <- tibble::tibble(date = reg_data$data[(63 + janela + 1):length(reg_data$data)],
                             ic_i_1 = coefs[,1] - sd[,1], intercept = coefs[,1], ic_i_2 = coefs[,1] + sd[,1],
                             ic_b_1 = coefs[,2] - sd[,2], beta = coefs[,2], ic_b_2 = coefs[,2] + sd[,2])

  # delta_i_t_6 ~ i_t_6_1 ------------------------------------------------------

  coefs_2 <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data$delta_i_t_6)) - janela)

  sd_2 <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data$delta_i_t_6)) - janela)

  # Loop

  for (i in 1:nrow(coefs_2)) {

    reg2      <- stats::lm(na.omit(reg_data$delta_i_t_6)[(1 + i - 1):(janela + i - 1)] ~ na.omit(reg_data$i_t_6_1)[(1 + i - 1):(janela + i - 1)])

    coefs_2[i,] <- stats::coef(reg2)

    sd_2[i,]    <- lmtest::coeftest(reg2)[,2]

  }

  colnames(coefs_2) <- c('Intercepto', 'Beta')
  colnames(sd_2)    <- c('Intercepto', 'Beta')

  results2 <- tibble::tibble(date = reg_data$data[(126 + janela + 1):length(reg_data$data)],
                             ic_i_1 = coefs_2[,1] - sd_2[,1], intercept = coefs_2[,1], ic_i_2 = coefs_2[,1] + sd_2[,1],
                             ic_b_1 = coefs_2[,2] - sd_2[,2], beta = coefs_2[,2], ic_b_2 = coefs_2[,2] + sd_2[,2])

  # delta_i_t_12 ~ i_t_3_12 ----------------------------------------------------

  coefs_3 <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data$delta_i_t_12)) - janela)

  sd_3 <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data$delta_i_t_12)) - janela)

  # Loop

  for (i in 1:nrow(coefs_3)) {

    reg3      <- stats::lm(na.omit(reg_data$delta_i_t_12)[(1 + i - 1):(janela + i - 1)] ~ na.omit(reg_data$i_t_12_1)[(1 + i - 1):(janela + i - 1)])

    coefs_3[i,] <- stats::coef(reg3)

    sd_3[i,]    <- lmtest::coeftest(reg3)[,2]

  }

  colnames(coefs_3) <- c('Intercepto', 'Beta')
  colnames(sd_3)    <- c('Intercepto', 'Beta')

  results3 <- tibble::tibble(date = reg_data$data[(252 + janela + 1):length(reg_data$data)],
                             ic_i_1 = coefs_3[,1] - sd_3[,1], intercept = coefs_3[,1], ic_i_2 = coefs_3[,1] + sd_3[,1],
                             ic_b_1 = coefs_3[,2] - sd_3[,2], beta = coefs_3[,2], ic_b_2 = coefs_3[,2] + sd_3[,2])

  # Yields monthly average -----------------------------------------------------

  reg_data_m <- curves_spline |>
    dplyr::mutate(month = lubridate::floor_date(data, "month")) |>
    dplyr::group_by(month) |>
    dplyr::summarise(dplyr::across(starts_with("v"), mean, na.rm = TRUE)) |>
    dplyr::mutate(delta_i_t_3  = tidyquant::RETURN(v21, n = 3),
                  delta_i_t_6  = tidyquant::RETURN(v21, n = 6),
                  delta_i_t_12 = tidyquant::RETURN(v21, n = 12),
                  i_t_3_1      = v63  - v21,
                  i_t_6_1      = v126 - v21,
                  i_t_12_1     = v252 - v21)

  p <- 2

  janela <- janela/21

  # delta_i_t_3 ~ i_t_3_1 ------------------------------------------------------

  coefs_m <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data_m$delta_i_t_3)) - janela)

  sd_m <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data_m$delta_i_t_3)) - janela)

  # Loop

  for (i in 1:nrow(coefs_m)) {

    reg1      <- stats::lm(na.omit(reg_data_m$delta_i_t_3)[(1 + i - 1):(janela + i - 1)] ~ na.omit(reg_data_m$i_t_3_1)[(1 + i - 1):(janela + i - 1)])

    coefs_m[i,] <- stats::coef(reg1)

    sd_m[i,]    <- lmtest::coeftest(reg1)[,2]

  }

  colnames(coefs_m) <- c('Intercepto', 'Beta')
  colnames(sd_m)    <- c('Intercepto', 'Beta')

  results1_m <- tibble::tibble(date = reg_data_m$month[(3 + janela + 1):length(reg_data_m$month)],
                             ic_i_1 = coefs_m[,1] - sd_m[,1], intercept = coefs_m[,1], ic_i_2 = coefs_m[,1] + sd_m[,1],
                             ic_b_1 = coefs_m[,2] - sd_m[,2], beta = coefs_m[,2], ic_b_2 = coefs_m[,2] + sd_m[,2])

  # delta_i_t_6 ~ i_t_6_1 ------------------------------------------------------

  coefs_2_m <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data_m$delta_i_t_6)) - janela)

  sd_2_m <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data_m$delta_i_t_6)) - janela)

  # Loop

  for (i in 1:nrow(coefs_2_m)) {

    reg2      <- stats::lm(na.omit(reg_data_m$delta_i_t_6)[(1 + i - 1):(janela + i - 1)] ~ na.omit(reg_data_m$i_t_6_1)[(1 + i - 1):(janela + i - 1)])

    coefs_2_m[i,] <- stats::coef(reg2)

    sd_2_m[i,]    <- lmtest::coeftest(reg2)[,2]

  }

  colnames(coefs_2_m) <- c('Intercepto', 'Beta')
  colnames(sd_2_m)    <- c('Intercepto', 'Beta')

  results2_m <- tibble::tibble(date = reg_data_m$month[(6 + janela + 1):length(reg_data_m$month)],
                             ic_i_1 = coefs_2_m[,1] - sd_2_m[,1], intercept = coefs_2_m[,1], ic_i_2 = coefs_2_m[,1] + sd_2_m[,1],
                             ic_b_1 = coefs_2_m[,2] - sd_2_m[,2], beta = coefs_2_m[,2], ic_b_2 = coefs_2_m[,2] + sd_2_m[,2])

  # delta_i_t_12 ~ i_t_3_12 ----------------------------------------------------

  coefs_3_m <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data_m$delta_i_t_12)) - janela)

  sd_3_m <- matrix(NA, ncol = p, nrow = length(na.omit(reg_data_m$delta_i_t_12)) - janela)

  # Loop

  for (i in 1:nrow(coefs_3_m)) {

    reg3      <- stats::lm(na.omit(reg_data_m$delta_i_t_12)[(1 + i - 1):(janela + i - 1)] ~ na.omit(reg_data_m$i_t_12_1)[(1 + i - 1):(janela + i - 1)])

    coefs_3_m[i,] <- stats::coef(reg3)

    sd_3_m[i,]    <- lmtest::coeftest(reg3)[,2]

  }

  colnames(coefs_3_m) <- c('Intercepto', 'Beta')
  colnames(sd_3_m)    <- c('Intercepto', 'Beta')

  results3_m <- tibble::tibble(date = reg_data_m$month[(12 + janela + 1):length(reg_data_m$month)],
                             ic_i_1 = coefs_3_m[,1] - sd_3_m[,1], intercept = coefs_3_m[,1], ic_i_2 = coefs_3_m[,1] + sd_3_m[,1],
                             ic_b_1 = coefs_3_m[,2] - sd_3_m[,2], beta = coefs_3_m[,2], ic_b_2 = coefs_3_m[,2] + sd_3_m[,2])

  g1 <- results1 |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ic_i_1, ymax = ic_i_2)) +
    geom_line(aes(y = intercept), linewidth = 1, color = "grey70") +
    geom_hline(aes(yintercept = mean(intercept), color = "red")) +
    labs(x = NULL, y = NULL, title = "Intercept delta_3_month", color = "Média")

  g2 <- results2 |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ic_i_1, ymax = ic_i_2)) +
    geom_line(aes(y = intercept), linewidth = 1, color = "grey70") +
    geom_hline(aes(yintercept = mean(intercept), color = "red")) +
    labs(x = NULL, y = NULL, title = "Intercept delta_6_month", color = "Média")

  g3 <- results3 |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ic_i_1, ymax = ic_i_2)) +
    geom_line(aes(y = intercept), linewidth = 1, color = "grey70") +
    geom_hline(aes(yintercept = mean(intercept), color = "red")) +
    labs(x = NULL, y = NULL, title = "Intercept delta_12_month", color = "Média")

  g4 <- results1_m |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ic_i_1, ymax = ic_i_2)) +
    geom_line(aes(y = intercept), linewidth = 1, color = "grey70") +
    geom_hline(aes(yintercept = mean(intercept), color = "red")) +
    labs(x = NULL, y = NULL, title = "Intercept delta_3_month", color = "Média")

  g5 <- results2_m |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ic_i_1, ymax = ic_i_2)) +
    geom_line(aes(y = intercept), linewidth = 1, color = "grey70") +
    geom_hline(aes(yintercept = mean(intercept), color = "red")) +
    labs(x = NULL, y = NULL, title = "Intercept delta_6_month", color = "Média")

  g6 <- results3_m |>
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = ic_i_1, ymax = ic_i_2)) +
    geom_line(aes(y = intercept), linewidth = 1, color = "grey70") +
    geom_hline(aes(yintercept = mean(intercept), color = "red")) +
    labs(x = NULL, y = NULL, title = "Intercept delta_12_month", color = "Média")


  ifelse(yield_spread == "3 months" & frequency == "daily", return(results1, g1),
    ifelse(yield_spread == "6 months" & frequency == "daily", return(results2, g2),
      ifelse(yield_spread == "12 months" & frequency == "daily", return(results3, g3),
        ifelse(yield_spread == "3 months" & frequency == "monthly", return(results1_m, g4),
          ifelse(yield_spread == "6 months" & frequency == "monthly", return(results2_m, g5),
           ifelse(yield_spread == "12 months" & frequency == "monthly", return(results3_m, g6),
                NULL
            )
          )
        )
      )
    )
 )

}



