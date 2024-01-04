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
  
  ibc <- rbcb::get_series(c(ibcbr = 24363))
  
  uci <- rbcb::get_series(c(uci = 24352)) # Utilização da Capacidade Instalada
  
  expc_pib <- rbcb::get_market_expectations('annual', 'PIB Total')
  
  expc_agro <- rbcb::get_market_expectations('annual', 'PIB Agropecuária')
  
  expec_gov <- rbcb::get_market_expectations('annual', 'PIB Despesa de consumo da administração pública')
  
  expec_c <- rbcb::get_market_expectations('annual', 'PIB despesa de consumo das famílias')
  
  expec_exp <- rbcb::get_market_expectations('annual', 'PIB Exportação de bens e serviços')
  
  expec_imp <- rbcb::get_market_expectations('annual', 'PIB Importação de bens e serviços')
  
  expec_fbcf <- rbcb::get_market_expectations('annual', 'PIB Formação Bruta de Capital Fixo')
  
  expec_ind <- rbcb::get_market_expectations('annual', 'PIB Indústria')
  
  expec_serv <- rbcb::get_market_expectations('annual', 'PIB Serviços')
  
  return(ibc)
  return(uci)
  return(expc_pib)
  return(expc_agro)
  return(expec_gov)
  return(expec_c)
  return(expec_exp)
  return(expec_imp)
  return(expec_fbcf)
  return(expec_ind)
  return(expec_serv)
  
  
}