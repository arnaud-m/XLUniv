#' Converts row & column indices to Excel relative cell references
#'
#' @param x  Numeric (integer) matrix or vector of indices.
#' @return Returns a character vector of corresponding Excel cell relative references.
#'
#' @seealso \code{\link[XLConnect]{idx2cref}}
#'
#' @export
#' @examples
#'
#' Idx2Ref(c(5, 8, 14, 38))
Idx2Ref <- function(x) {
  XLConnect::idx2cref(x, absRow=FALSE, absCol=FALSE) 
}           
