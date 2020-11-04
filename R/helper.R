#' Read in data from PRPT task
#'
#' @param file A character string that contains the path to the file.
#'
#' @return A data frame that contains the PRPT behavioral data with time stamps.
#' @export
#'


read_prpt <- function(file) {

  readr::read_tsv(file = file,
                  col_names = c("time", "data"),
                  col_types = c(readr::col_double(),
                                readr::col_character()))

}

