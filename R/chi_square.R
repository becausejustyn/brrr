#' chi_square
#'
#' @param df dataframe
#' @param type condition column
#'
#' @return chi-squared htest class
#' @export
#'
#' @examples
#' \dontrun{
#' chi_square(df, 'biased')
#' }
chi_square <- function(df, type) {
  summary_cols <- switch(
    type,
    fair_light = c('fair', 'light'),
    fair_dark = c('fair', 'dark'),
    biased = c('dark', 'light'),
    NULL
  )

  if (is.null(summary_cols)) {
    warning("Invalid type specified.")
    return(NULL)
  }

  df1 <- df |>
    dplyr::filter(!!rlang::enquo(type) == type) |>
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
