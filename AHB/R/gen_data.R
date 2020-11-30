# author: Xian Sun, Duke University, 2020


#' Generate Toy Data for AHB Matching
#'
#' \code{gen_data} generates toy data that can be used to explore AHB's
#' functionality.
#'
#' \code{gen_data} simulates data in the format accepted by
#' \code{\link{AHB_fast_match}} and \code{\link{AHB_MIP_match}}. Covariates
#' \eqn{X} and treatment \eqn{T} are independently generated according to
#' uniformly distribution and Bernoulli(0.5) distribution respectively. The
#' outcome \eqn{Y} is generated according to \eqn{Y = 2 + 5*(X[,1] > 1.5)*T +
#' \epsilon}, where \eqn{\epsilon \sim N(0, I_n)}. Thus, the value of \code{p}
#' must be at least 2.
#'
#' @param n_units Number of units desired in the data set. Defaults to 100.
#' @param p Number of covariates in the data set. Must be greater than 2.
#'   Defaults to 5.

#' @return  A data frame that may be passed to  \code{\link{AHB_fast_match}} and
#'   \code{\link{AHB_MIP_match}}. Covariates are continuous. Treatment is binary
#'   numeric and outcome is numeric.
#'
#' @export


gen_data <- function(n_units=100, p=5){


  beta0 <- 2 # Baseline response
  beta_tilde <- 5 # treatment effect
  beta <- runif(p, -1, 1) # To go from X to propensity score

  ## For generating propensity scores and assigning treatment
  X <- matrix(runif(p * n_units, -5, 5), nrow = n_units)
  Z <- rbinom(n_units, 1, 0.5)
  ## Generate outcome
  eps <- rnorm(n_units, 0, 1)
  Y1 <- beta0 + (X[, 1] > 1.5) * beta_tilde + eps
  Y0 <- beta0 + eps

  Y = Y1 * Z + Y0 * (1-Z)

  df <- cbind(data.frame(X), data.frame(outcome = Y, treated = Z))
  return(data = df)
}




