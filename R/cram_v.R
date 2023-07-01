#' cram_v
#'
#' @param test chi square test
#'
#' @return cramer's V
#' @export
#'
#' @examples
#' chi_sq_test <- chisq.test(c(A = 20, B = 15, C = 25))
#' cram_v(chi_sq_test)
cram_v <- function(test){
  v = base::sqrt((test[['statistic']]) / (base::sum(test[['observed']]) * test[['parameter']]))[[1]]
  return(v)
}
