#' Encode a number in a given base
#'
#' 
#' @param n  Number to convert
#' @param base Base of conversion
#' @param digits Character vector that represent the digits 0...(base-1)
#' @param dec Decimal separator
#' @param precision Maximum precision of the fractional part 
#'
#' @examples
#' EncodeNumber(42)
#' EncodeNumber(42, 2)
#' EncodeNumber(42, 3, letters) 
EncodeNumber <- function(n, base = 10, digits = c(0:9, LETTERS), dec = '.', precision = 31) {
  stopifnot(n >= 0, base > 1, base <= length(digits), is.character(dec), is.numeric(precision))
  tolerance <- 2**(-30)
  f <- floor(n)
  d <- n - f
  ## integer part
  if(f > 0) {
    acc <- ''
    while(f > 0) {
      acc <- paste0( digits[ (f %% base) + 1], acc)
      f <- f %/% base
    }
  } else {
    acc <- digits[1]
  }
  ## fractional part
  if(d > tolerance && precision > 0) {
    acc <- paste0( acc, dec)
    prec <- 0
    repeat {
      d <- d * base
      fd <- floor(d)
      acc <- paste0( acc, digits[fd + 1])
      d <- d - fd
      prec <- prec + 1
      if( d < tolerance || prec >= precision) break;
    }
    ## Check if something has gone wrong
    if(prec == precision) {
      acc <- gsub('\\.0*$','', acc) ## remove possible trailing zeros
      warning(
        sprintf(
          'Maximum precision (%d) reached when converting %f in base %d: %s',
          precision, n, base, acc
        )
      )
    }
  }
  return(acc)
}
