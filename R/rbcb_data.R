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
#'
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

  expec_ipca <- rbcb::get_market_expectations('annual', 'IPCA')

  expec_ipca_m <- rbcb::get_market_expectations('monthly', 'IPCA', start_date = Sys.Date() - 30)

  expec_ipca_m_top5 <- rbcb::get_market_expectations('top5s-monthly', 'IPCA', start_date = Sys.Date() - 30)

  expec_ipca_top5 <- rbcb::get_market_expectations('top5s-annual', 'IPCA')

  selic <- rbcb::get_series(c(meta = 432, efetiva = 1178), start_date = "2003-01-01", end_date = Sys.Date())

  expec_selic <- rbcb::get_market_expectations('annual', 'Selic', start_date = Sys.Date() - 365, end_date = Sys.Date())

  fiscal <- rbcb::get_series(c(Governo_Federal_primario = 7853, INSS_primario = 7854, Governos_Estaduais_primario = 4643,
                               Governos_Municipais_primario = 4644, Empresas_Estatais_primario = 4645, Setor_Publico_Consolidado_primario = 4649,
                               Governo_Federal_juros = 4607, Banco_Central_juros = 4608, Governos_Estaduais_juros = 4610,
                               Governos_Municipais_juros = 4611, Empresas_Estatais_juros = 4612, Setor_Publico_Consolidado_juros = 4616,
                               Governo_Federal_Nominal = 4574, Banco_Central_nominal = 4575, Governos_Estaduais_nominal = 4577,
                               Governos_Municipais_nominal = 4578, Empresas_Estatais_nominal = 4579, Setor_Publico_Consolidado_nominal = 4583,
                               div_pib = 13762))

  cambio <- rbcb::get_series(c(ptax_usd = 1, ptax_eur = 21619))

  bp <- rbcb::get_series(c(tc = 22701, k = 22863, reserv = 3546, tc_pib = 23079, ied_pib = 23080))

  results <- list(ibc = ibc, uci = uci, ipca = ipca, ipca_desag = ipca_desag, expec_pib = expec_pib, expec_agro = expec_agro,
                 expec_gov = expec_gov, expec_c = expec_c, expec_exp = expec_exp,
                 expec_imp = expec_imp, expec_fbcf = expec_fbcf, expec_ind = expec_ind,
                 expec_serv = expec_serv, expec_ipca = expec_ipca, expec_ipca_top5 = expec_ipca_top5, selic = selic, expec_selic = expec_selic,
                 fiscal = fiscal, expec_ipca_m = expec_ipca_m, expec_ipca_m_top5 = expec_ipca_m_top5, cambio = cambio, bp = bp)

  return(results)


}

# devtools::load_all(); bc <- rbcb_data(); saveRDS(bc, 'Data/bc.rds')

