
# Screening ---------------------------------------------------------------

#' Imports the file Screening Ações.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_screening_stocks()
#' }
get_screening_stocks <- function() {

  x <- economatica(
    "https://api.data.economatica.com/1/oficial/datafeed/download/1/qXq1Pd4SZwbS7bX3Kq4TWdi80mbZ0W3AqYR17rPo1%2Bda85E7%2BljZm197DeyDSbU9PCCgzO338PLTip%2FlylyJGtWIgx2ESk6E9WF8dIHoPKMGXHKCo81pQ1LbhE5w8NmzFr8S3iul2UVyjncb%2FRp6kogbBS9456i2BKgfJWcy5Q6debeG%2BOPhBYL4PUjV1C80jMhaF6O82YY1g46JjWxZJAusahofyuEFZxMEHUmOtqhNV4%2B1lKqUrkexcwl7ClDhGdyv9CdgnK%2FpnxFmtejDet%2BJ0TaWYuDFrgQyTV6jun6m5iFZ9Men463ZCSYx3ZtbXXHuRQo%2FdygjXkocOAdWcw%3D%3D"
  )

  # names(x) <- names(x) |>
  #   stringr::str_to_lower() |>
  #   stringr::str_replace(pattern = "\\.", replacement = "_") |>
  #   accentless()

  x |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor))

}


#' Imports the file Screening Debentures.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_screening_debentures()
#' }
get_screening_debentures <- function() {

  x <- economatica(
    "https://api.data.economatica.com/1/oficial/datafeed/download/1/NEDU6qPchuL%2F8%2BAUYAwlIMIxfIvJqBpItKu3J9JUyLtScBDW7Hir1cy8AnDYv5MK%2BYUU2GyttrrTeEOF0V2jtxGHdHl5W1wX%2BkFX2vPlV4Q%2FG7qWaTCXbj8dI1u50bakh9bXoli6RSk%2BAOgzKbzeMifHhpFtcqZaHZIebdbgOs323bgPOw%2BJWBkEHHgo7%2Fc4HVvFfnpPEWP97j%2FTRlyx%2BNqyK0VVEUrfw%2F%2FqJ3ggWoL%2Ftmy54toHmmmO6B%2BETc3xKX2KCeqAkvLAflqvhL1bteQdESQUl7N7FDb6rzaAnzxqaob3pPO2eKs4CUpSVO8%2BD8DVJuv1tCe6y4g9%2Fea5Dw%3D%3D"
  )

  # names(x) <- names(x) |>
  #   stringr::str_to_lower() |>
  #   stringr::str_replace(pattern = "\\.", replacement = "_") |>
  #   accentless()

  x |>
    janitor::clean_names() |>
    dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<BraANB>")) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), as.factor))

}

# Asset Files -------------------------------------------------------------

#' Imports the file ida_cdi.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_ida()
#' }
get_ida <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/dnSXitb0Y0JUOOzOjICJQmQyKM6L25Vy4SbNlDykeLgwLVLfrS4%2B%2FR9HkkfvXevkhbAdCLVCobJ6bq6kWbRwD3ZXq5SmLt4hO%2F0eI0aekUqwE0AbbSC3GHucPpVXfcG8%2BgoDCVeO9BXcJ1B%2BRIrWvxTxlEUas0PcJBSewNX7FLF5GDawLb16gh4gZFlEHcmGHk8tpSCgbZCyX8qcKEcDCQp9cjvjZFV72d%2BXl%2BJs8wS1Me%2BjCqAaimpUZvGQLfwSn1RSS%2FLzuKv2tkqyZ6BZgXYkAjYqwF5Rq7FA5QZT1yRxFJBzeWnLCya3N26eMKzTW0aVfEbMK00%2BnYhfPzITww%3D%3D"),
      col_names = c("ativo", "data", "values"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>

      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<BraNa>|<BraANB>| Acumulado| Inf"))

  )

}

#' Imports the file Screening Titulos Publicos.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_tp()
#' }
get_tp <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/NmQyC7JsSUHgwk3jYJCTYoagBAvu5O%2BRAaxTPAsD0gjcTxC9rYR5GlsEeg9NwPC4%2BaXxap7t2QzLqAMwfhCHMBnp%2FYxbMp2O7U1yvQDriCTG8SNPjyeVLnrgmsdADFpd6w%2Bc9bIxlmCXWlJasFNE%2F0gTaPkjXetQpBNB3K8ux2vpFKoHxsGy%2F9veUFlgUSM0C1QtCSvihous9kpHBNO0tjoWebQnFuMcdD%2BPf2VRJM6bbTDFuTFbTOudZ8MJrDCPI9EFzkpWmRvBjLYHWb8im7pOgSZ09GgCWEwZcehRHng2UlBiSxsdH6NnwT1AIbpdJqZERgC5x%2BpKZTosjjWTbA%3D%3D"),
      col_names = c("ativo", "data", "pu", "duration", "ytm"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double(), vroom::col_double(), vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>

      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<BraANB>"))

  )

}

#' Imports the file Screening Titulos Publicos.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_tp()
#' }
get_tp_screening <- function() {

  suppressWarnings(

    vroom::vroom(

      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/UpXvU%2FrVVn8h0CCsOguj%2FaAhqPbS14rMep3PyVWtx1bwsQurydw5W4zp7d5mXjzPId%2BRzebMiA4ueNJxnq8gi2EdgHWq3C92RZzWM3vpnwOs5H50f3dBUpjBUYwnkOerJFtgKEpZzcnaMapnc5dxQFKuPfSzcy4UaQziDs5lqXkiWmB7RNzzgIiZBeBimRhXnIhLVmfQjIW%2F53R3Avp%2F8FlbWVbeqY653ws%2BsEhoI43mHDGfrdY0aLqRYRFQr5mujynOZLJNyZngyHqG9g3FbB5fL%2BSPkF4IXlac8ruAF%2FceN63W8LmgJkitTTMM%2FuVxwLz726K%2BDcXbvm2Ug1LGDQ%3D%3D"),
      col_names = c("classe", "codigo", "nome", "vencimento", "ativo_cancelado", "bolsa_fonte", "ytm", "duration", "data_inicio", "pu", "isin"),
      col_types = list(vroom::col_date(), vroom::col_factor(), vroom::col_factor(), vroom::col_date(), vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>

      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<BraANB>"))

  )

}

#' Imports the file Screening DI.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_di()
#' }
get_di <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/pGxD1YWeWeonoRjmjWRbho2Jn0zClJwII3tDZyS8oX56mjaOhftimBac3PwrXiAxkfgn47GtCpagBgUV%2F5Nue9Ii7UUruZXF%2BaF1DvFxGVGQ7Bdtnhcb2B986lhY8TipLhOmyw2ygcTIjCzJA0%2BqyWEfAlDlmTXEDTifsoQ7SjHxCFOQCUQy6CGuS1rjWmB3hvLsrIJots0ra5Q321FV%2B8hFjBt3oDiMnqNVcng1Gj3DkcwMk2TyzxfBhjY2gX1p7NqR2M%2F9r2%2B6V4hwGDLOXuhviiku0YQVmPxxWq%2BgWKz%2FQBDAWFkS1Tkqq%2B6TX1FFDTASBwCA%2BA5wIZityztevA%3D%3D"),
      col_names = c("ativo", "data", "values"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>

      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<XBMF>"), values = values / 100)

  )

}

#' Imports the file Debentures.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_debentures()
#' }
get_debentures <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/Dx36nozxNDqOhfUU2YXbuu6jTrhFmZ8WjldI3%2BwG5iPqpo9iayNQk27jl2t%2BEojlV3l3gR3g%2FFwrIlGpJmXxDIjkM62N1NOMKupX8foHB0vdI6RjeEMshMpZ0voXQeCXozslIoZxGWjgxG0OD5lsGxJdLazjLdRstCptsacjfmqcSEoSVAI5FwRdcJhjbF4WsEgri1ea5o8zFpgGy8KiVK9%2BN4S8ctqYD8nzLESnpWChtEfZI1Jk8nNkzaqt%2F%2F3rsqe5vetw15bD%2Fkn3thIDapZd6S1MxB7MgowOiLFHNhaK2g%2BV%2FLGwlXj1Mt9siGOCBvhuECWZW05C%2BZhzBVLQLA%3D%3D"),
      col_names = c("ativo", "data", "ytm", "taxa_compra", "taxa_venda", "spread", "duration", "pu"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double(), vroom::col_double(),
                       vroom::col_double(), vroom::col_double(), vroom::col_double(), vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>

      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<BraANB>"),
                    ytm         = ytm / 100,
                    taxa_compra = taxa_compra / 100,
                    taxa_venda  = taxa_venda / 100,
                    duration    = duration / 252)

  )

}


#' Imports the file PPP IUP.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_uncovered_interest_rate_parity()
#' }
get_uncovered_interest_rate_parity <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/di9lRO016PsyyDI00brXhMN9A01Xw86c76bQkv3odDEqXpzJAHtbaiPJf96G5iuk7gazpd16lR2rbkJAB7RkhaIqoDRKWFuqmoDxUL4wjt%2FZjY3jLU6w5XXxKs%2FXBYXmPIMAiM19wxukNCImrRbM2G0ozSjphtZwTeZIaXR7pKA%2B5DpBUaEBEPNStQQcGAISn1iOZQ1dgDAFd7kDotBXWkTSgNwUzpvvXozlLkf88kQuGPmaGPicD6NStRsM83BvukZiU%2F5We4pVvz1T6xrqGToAPWhlsJq0PCEj1ygAl3PLXydwZLEYhM0japIOQmqZzloi3J%2FSZA2LgGgE3DZjYg%3D%3D"),
      col_names = c("delme", "data", "cpi", "dolof", "fed", "ipca", "selic"),
      col_types = list(vroom::col_character(), vroom::col_character(), vroom::col_double(), vroom::col_double(),
                       vroom::col_double(), vroom::col_double(), vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>
      dplyr::select(-delme) |>
      tidyr::separate(col = data, into = c("month", "year")) |>
      dplyr::mutate(month = dplyr::case_when(
        month == "Jan" ~ 01,
        month == "Fev" ~ 02,
        month == "Mar" ~ 03,
        month == "Abr" ~ 04,
        month == "Mai" ~ 05,
        month == "Jun" ~ 06,
        month == "Jul" ~ 07,
        month == "Ago" ~ 08,
        month == "Set" ~ 09,
        month == "Out" ~ 10,
        month == "Nov" ~ 11,
        .default       = 12),
        year = as.double(year),
        data = lubridate::make_date(year = year, month = month, day = 01)
      ) |>
      dplyr::select(-c(month, year)) |>
      dplyr::select(data, dplyr::everything())

  )

}


#' Imports the file risk_parity_brasil.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_risk_parity_brasil()
#' }
get_risk_parity_brasil <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/jmIzpDOM%2Blz56Av8cIIJQLniBIsqYDj8Om3IglwtG%2BUA1fkHX0Qn7zs2qFoHFEa%2B9iwDf9W3keGM9k5s1lv1i19rFyey0LS%2BFXO7mRY1PhwnOg33atPkP5dg6nXl%2FSYd4zFsEu4yfUYIt8DZ3EKozsYkDDvBdZZ3Myh073gdLLddoz2VFQDL8V5lnYWWUds6Se4ZWPVQFBviObZL44IQSKqiidBmnAbhWkmc87rtmaCK6Lt7N5g7PhgkY7MoKQUEQz1abeTnCLSmgPUSqODWc3isLwlSC7FW%2BBltnvstdmSH%2BVp%2BrLlwHbVoFoRUACIoDT27WoCyioGJGY9Z5aRhvg%3D%3D"),
      col_names = c("delme", "data", "cdi", "dolar", "djia", "ibov", "ida_di", "ida_ipca",
                    "idka_ipca_10a", "idka_ipca_2a", "idka_ipca_5a", "idka_pre_2a", "idka_pre_5a"),
      col_types = list(vroom::col_character(), vroom::col_date(), vroom::col_double(), vroom::col_double(),
                       vroom::col_double(), vroom::col_double(), vroom::col_double(),  vroom::col_double(),
                       vroom::col_double(), vroom::col_double(), vroom::col_double(), vroom::col_double(),
                       vroom::col_double()),
      na = "NA",
      skip = 1
    ) |>
      dplyr::select(-delme)

  )

}

#' Imports the file Indicadores Ibovespa.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_indicadores_ibovespa()
#' }
get_indicadores_ibovespa <- function() {

  suppressWarnings(

    x <- vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/gAfNbduDf17oQjvdMP6HL6Telunn8866C74NkYD9awTn6nx3jMt%2FQ47XBt6%2BLxFLOwFENIhonm2VWLF1Naaqzj0JI53OvHxuoQZgWwmRPKH%2FfVQc0g0PRWFpqvsRwWXvZG7wZsjUQJBzFj84XrGrtOhL2z4uuTN7SXoLZe5kGMHby0ZYv3aHRV1L8Pr0O3djRdVFy3tbCJSY0HIK8fH3gWsnVSq3%2BnJjhYwT9yAoGW9t0EEVdVFiIOqVcHVhfS9nbDiVMbNCTpmQVbpZUvydFPAI4xauP%2FqDNLoMlsiuIUq8Pf%2F9QhRcm9S15I1yXp8O5RCOdoDBxANiT75TCVX29A%3D%3D"),
      delim = ",",
      col_types = list(
        vroom::col_character(), vroom::col_date(), vroom::col_double(), vroom::col_double(), vroom::col_double(),
        vroom::col_double(), vroom::col_double(), vroom::col_double(), vroom::col_double(), vroom::col_double(),
        vroom::col_double(), vroom::col_double(), vroom::col_double()
      ),
      .name_repair = janitor::make_clean_names)

  )

  x |>
    dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<XBSP>"))

}


# Stocks -----------------------------------------------------------------

#' Imports the file cointegration_stocks_ibrx100.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_ibrx100_stocks()
#' }
get_ibrx100_stocks <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/JosSf54bsciGW6CvUALmHKsSKBFdzzEJe0RFOtTYt1rJ0qgIIFLqpnloBepwZOOAdK6VnYmhTaR%2BH4hr14Qg1auBqQX%2FTUdRosFAjRMSANoy%2BUhYxdjY2FZm1cq6qSrA7Iwux%2BLSR1OfL7zOBsfkn2WslETJCmkdQKEU6R4okjOEqP%2F0xVuB4TPPCUW86cFIPff3REJxaP2VVDmu1SwLsXj2zpz%2Bvo7jT2uIt8dslPNpsPj40ezb6iVa53zqa7d16uU3%2FMrvjQnvTL3LoilKaPM8IkGSv17zJ9tBa0sIsBRL5Q03D4VoYTL4kN2GY%2B7ZVmPG92eH1TUPUVYqKBbTog%3D%3D"),
      col_names = c("ativo", "data", "values"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double()),
      na        = "NA",
      skip      = 1
    ) |>
      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<XBSP>"))

  )

}

#' Imports the file cointegration_stocks_ibovespa.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_ibovespa_stocks()
#' }
get_ibovespa_stocks <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/gvj0UxAUZzewYevkLmntJqu%2BYxCLNhVXF6nbR0eRKu6UhFMWjH6o%2BN3drsmXPe1jGg4gl20SGN%2BODTQ5WLhaVHy%2Fg8Ve%2BGTBOcVbpjKhbZrQ04OSbkdnrgYjc8dk0bx8mylgwQ1pkr7Mp140clU7vJE2Mq63H5gAD9pUlpE%2Fzjf2U3TbSTLgsKxcw4DArvLCygCj9W6tM3oxBHfS5jZ16Gs6jxmlDdFZx7bg6pSCkz8FLYWZTCaG628EozCJZ0o%2BsNf9NXes8uA5tYUhFJfiGnfuwLH8VdU7SFzcFOp6i0skyX2ogDvKkYrTJ9tTQFfM3EZf1o%2FhBg%2FbDhzHPeXSsQ%3D%3D"),
      col_names = c("ativo", "data", "values"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double()),
      na        = "NA",
      skip      = 1
    ) |>
      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<XBSP>"))

  )

}

#' Imports the file cointegration_stocks_ibrx50.GLW
#'
#' @return A \code{tibble}.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   get_ibrx50_stocks()
#' }
get_ibrx50_stocks <- function() {

  suppressWarnings(

    vroom::vroom(
      file = url("https://api.data.economatica.com/1/oficial/datafeed/download/1/T29ywClIhCk12l4XWwlNZgjiD4DePsGIg6YhmXd1WimcMe4PJjbSwU%2FEBXQx0IK1yewVQPV4NKlvMEYrUyx2LDqkl3H8cGjzuQTH44uVIA%2FIGEwqeG3DMX9ow0euTFUCQ9Xkz7yb4gD2bPP%2BXPJVzuypKLmUC4kTazZUgF0QDnG1lzgjx0G%2Bz9Mwyaa2sVzlVeFPox4RUviqrqPiCqG1sLyj%2BHO3zEhWXh8mtRFr0Hyg3gq%2B3pqnW1E9o9pqQmu5P1r%2BwARqccrxLwSmpEkMd8dHBxIXEyHTWMX3zbQGMNN2v16TB2QmiV4tZ7NWk6NNkv5lgkylgJ%2BAUL6BzEHOdQ%3D%3D"),
      col_names = c("ativo", "data", "values"),
      col_types = list(vroom::col_factor(), vroom::col_date(), vroom::col_double()),
      na        = "NA",
      skip      = 1,

    ) |>
      dplyr::mutate(ativo = stringr::str_remove_all(ativo, "<XBSP>"))

  )

}
