#' get_mode
#'
#' @param x vector
#'
#' @return mode of vector
#' @export
#'
#' @examples get_mode(mtcars[['carb']])
get_mode <- function(x) {
  ux <- base::unique(x)
  ux[base::which.max(base::tabulate(base::match(x, ux)))]
}
