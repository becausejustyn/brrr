#' chi_square_total
#'
#' This does chi square for all conditions combined.
#'
#' @param df dataframe
#'
#' @return chi-squared htest class
#' @export
#'
#' @examples
#' \dontrun{
#' chi_square_total(df)
#' }
chi_square_total <- function(df) {

  df1 <- df |>
    dplyr::group_by(uuid) |>
    dplyr::count(preferred_mod) |>
    dplyr::ungroup()

  # this warning is not needed. It is about approximation,
  # however, it is not relevant in this instance
  result <- suppressWarnings(
    xtabs(n ~ uuid + preferred_mod, df1) |>
      chisq.test()
  )

  #result <- xtabs(n ~ uuid + preferred_mod, df1) |>
  #  chisq.test()

  return(result)
}
