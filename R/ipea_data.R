#' ipea_data
#'
#' @return
#' @export
#'
#'
ipea_data <- function(){

  fed_funds <- ipeadatar::ipeadata(code = "VALOR366_FEDFUND366", language = "en")

  caged <- ipeadatar::ipeadata(code = "CAGED12_SALDON12", language = "en")

  # nfsp_n <- ipeadatar::ipeadata(code = "BM12_NFGFNNS12", language = "en") # nominal sem desvalorização cambial
  #
  # nfsp_n_12m <- ipeadatar::ipeadata(code = "BM12_NFGFNYS12", language = "en") # nominal sem desvalorização cambial
  #
  # nfsp_p <- ipeadatar::ipeadata(code = "BM12_NFGFPNS12", language = "en") # primário sem desvalorização cambial
  #
  # nfsp_p_12m <- ipeadatar::ipeadata(code = "BM12_NFGFPNAS12", language = "en") # primário sem desvalorização cambial

  results <- list(fed_funds = fed_funds, caged = caged)

  return(results)

}

# saveRDS(ipea, 'Data/ipea.rds')
