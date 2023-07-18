#' lrt
#'
#' @param chi_square_data chi-squared htest class
#'
#' @return list of likelihood_ratio_statistic, degrees of freedom, p_value
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c(12, 5)
#' x_chi <- chisq.test(x)
#' lrt(x_chi)
#' }

lrt <- function(chi_square_data){
  # contingency table of observed and expected counts
  observed <- chi_square_data[['observed']] |> as.table()
  expected <- chi_square_data[['expected']] |> as.table()

  # Log-likelihood of observed and expected counts
  LL1 <- stats::logLik(observed)
  LL2 <- stats::logLik(expected)

  # Likelihood ratio test statistic
  LR_stat <- 2 * (LL2 - LL1)

  # Degrees of freedom
  df <- chi_square_data[['parameter']]
  p_value <- stats::pchisq(LR_stat, df = df, lower.tail = FALSE)

  result <- list(
    likelihood_ratio_statistic = LR_stat,
    degrees_of_freedom = df,
    p_value = p_value
  )

  return(result)
}
