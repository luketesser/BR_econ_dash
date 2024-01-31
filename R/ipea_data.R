#' ipea_data
#'
#' @return
#' @export
#'
#' @examples
ipea_data <- function(){

  fed_funds <- ipeadatar::ipeadata(code = "VALOR366_FEDFUND366", language = "en")

  results <- list(fed_funds = fed_funds)

  return(results)

}

# saveRDS(ipea, 'Data/ipea.rds')
