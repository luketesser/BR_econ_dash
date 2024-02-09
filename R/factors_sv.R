#' Estimates the 4 factors and 2 decay parameters in the Svensson (1994) factor model. The decay parameters are optimized using the two step approach.
#'
#' @description
#' Determines the optimal decay parameters calling the rmse function.
#' Then, performs an OLS regression using the 4 factor yield curve model in order to determine the optimal \eqn{\lambda} decay parameters:
#' \deqn{y_{\tau} = \beta_{1} +
#'                  \beta_{2}((1-e^{(-\tau^{'}  \lambda_{1})})/(\tau^{'}\lambda_{1})) +
#'                  \beta_{3}((1-e^{(-\tau^{'}  \lambda_{1})})/(\tau^{'}\lambda_{1}) - e^{(-\tau^{'}  \lambda_{1})}) +
#'                  \beta_{4}((1-e^{(-\tau^{'}  \lambda_{2})})/(\tau^{'}\lambda_{2}) - e^{(-\tau^{'}  \lambda_{2})})}
#'
#' @param lambda Initial guess for the decay parameters. A pair of numeric values.
#' @param yields Observed yields. Numeric Vector.
#' @param maturidades Time to maturity. Numeric. Must match the length of Y and be in the same base, i.e, annual, monthly etc.
#'
#' @return A matrix containing the 4 factors and 2 decay parameters, respectively.
#' @export
#'
#' @examples
#' 
#' #library('Quandl')
#' 
#' #dados <- Quandl("USTREASURY/YIELD")
#' dados <- ettj_package_data
#' yields <- dados[1,2:ncol(dados)]
#' maturidades <- c(1/12, 2/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30);
#' factors <- factors_sv(c(.9,.035), yields, maturidades)
#' yc <- ycsv(factors[5:6], factors[1:4], maturidades)
#'
factors_sv <- function(lambda, yields, maturidades){
  # Treating data -----------------------------------------------------------
  
  yields <- as.double(yields)
  
  maturidades <- as.double(maturidades)
  
  index <- dplyr::full_join(dplyr::as_tibble(which(is.na(yields))), dplyr::as_tibble(which(is.na(maturidades))))

  index <- as.double(as.matrix(index))

  if(purrr::is_empty(index)){
    yields <- yields
    maturidades <- maturidades
  }else{
    yields <- yields[-c(index),]
    maturidades <- maturidades[-c(index),]
  }


  # Calculating optimal decay parameters --------------------------------


  optlamfun <- stats::optim(lambda, rmse_sv, Y = yields, tau = maturidades , control = list(maxit = 10000, reltol = 0.00000001))

  lam1 <- optlamfun$par[1]
  lam2 <- optlamfun$par[2]

  # Estimating Betas --------------------------------------------------------


  H <-  matrix(NA,length(yields),4)
  B <-  matrix(NA,4,1)

  H[1:length(yields),1] <- 1
  H[1:length(yields),2] <- (1 - exp(-t(maturidades)*lam1))/(t(maturidades)*lam1)
  H[1:length(yields),3] <- (1 - exp(-t(maturidades)*lam1))/(t(maturidades)*lam1) - exp(-t(maturidades)*lam1)
  H[1:length(yields),4] <- (1 - exp(-t(maturidades)*lam2))/(t(maturidades)*lam2) - exp(-t(maturidades)*lam2)
  B[,1] <- solve(t(H) %*% H, tol = 10^-50) %*% (t(H) %*% yields)


  final <- rbind(B, lam1, lam2)
  
  rownames(final) <- c('Beta 1', 'Beta 2', 'Beta 3', 'Beta 4', 'lambda 1', 'lambda 2')

  return(final)
}
