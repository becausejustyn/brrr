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
  # Log-likelihood of observed counts
  LL1 <- stats::logLik(chi_square_data[['observed']])
  # Log-likelihood of expected counts, e.g. null
  LL2 <- stats::logLik(chi_square_data[['expected']])

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
