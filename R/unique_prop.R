#' unique_prop
#'
#' @param x vector
#'
#' @return proportion of unique values in x
#' @export
#'
#' @examples
#' mtcars[['carb']] |> unique_prop()
unique_prop <- function(x) {
  unique_count <- dplyr::n_distinct(x)
  prop <- unique_count / base::length(x)
  return(prop)
}
