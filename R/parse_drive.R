#' parse_drive
#'
#' @param id document id from google drive
#'
#' @return document path to pass to a function that reads files
#' @export
#'
#' @examples
#' parse_drive(id = '23492ff83')
parse_drive <- function(id){
  # from https://stackoverflow.com/questions/42461806/getting-a-csv-read-into-r-though-a-shareable-google-drive-link
  x <- paste0("https://docs.google.com/uc?id=", id, "&export=download")
  return(x)
}
