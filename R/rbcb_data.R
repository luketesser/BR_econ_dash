#' sidra_data
#'
#' @description
#' Downloads data of a set of economic measures with the rbcb package.
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
rbcb_data <- function(){

  ibc <- rbcb::get_series(c(ibcbr = 24364))

  uci <- rbcb::get_series(c(uci = 28561)) # Utilização da Capacidade Instalada

  ipca <- rbcb::get_series(c(ipca = 433, n1 = 4466, n2 = 11426, n3 = 11427, n4 = 16121, n5 = 16122, n6 = 27838, n7 = 27839, n8 = 28750, n9 = 28751)) # índice cheio e núcleos

  ipca_desag <- rbcb::get_series(c(al = 1635, hab = 1636, res = 1637, vest = 1638, trasp = 1639, comun = 1640, saud = 1641, pess = 1642, educ = 1643))

  expec_pib <- rbcb::get_market_expectations('annual', 'PIB Total')

  expec_agro <- rbcb::get_market_expectations('annual', 'PIB Agropecuária')

  expec_gov <- rbcb::get_market_expectations('annual', 'PIB Despesa de consumo da administração pública')

  expec_c <- rbcb::get_market_expectations('annual', 'PIB despesa de consumo das famílias')

  expec_exp <- rbcb::get_market_expectations('annual', 'PIB Exportação de bens e serviços')

  expec_imp <- rbcb::get_market_expectations('annual', 'PIB Importação de bens e serviços')

  expec_fbcf <- rbcb::get_market_expectations('annual', 'PIB Formação Bruta de Capital Fixo')

  expec_ind <- rbcb::get_market_expectations('annual', 'PIB Indústria')

  expec_serv <- rbcb::get_market_expectations('annual', 'PIB Serviços')

  results <- list(ibc = ibc, uci = uci, ipca = ipca, ipca_desag = ipca_desag, expec_pib = expec_pib, expec_agro = expec_agro,
                 expec_gov = expec_gov, expec_c = expec_c, expec_exp = expec_exp,
                 expec_imp = expec_imp, expec_fbcf = expec_fbcf, expec_ind = expec_ind,
                 expec_serv = expec_serv)

  return(results)


}

# saveRDS(bc, 'Data/bc.rds')
