#' Write a number in a given base
#'
#' 
#' 
#' @param n  Number to convert
#' @param base Base of conversion
#' @param digits the character that represent the digits 0...(base-1)
#'
#' @export
#' @examples
#'
#' NumberToString(42)
#' NumberToString(42, 2)
#' NumberToString(42, 3, letters) 
NumberToString <- function(n, base = 10, digits = c(0:9, LETTERS), dec = '.') {
  acc <- ""
  stopifnot(n >= 0, base <= length(digits))
  f <- floor(n)
  d <- n - f
  ## integer part
  if(f > 0) {
    while(f > 0) {
      acc <- paste0( digits[ (f %% base) + 1], acc)
      f <- f %/% base
    }
  } else {
    acc <- "0"
  }
  ## fractional part
  if(d > 0) {
    acc <- paste0( acc, dec)
    while(d > 0) {
      d <- d * base
      fd <- floor(d)
      acc <- paste0( acc, digits[fd + 1])
      d <- d-fd
    }
  }
  return(acc)
}
