#' Get basic statistics for a given grade column
#'
#' 
#' 
#' @param n  Maximal row index of the cells to apply the statistics to.
#' @param m Column index of the cells to apply the statistics to.
#' @param nmin  Minimal row index of the cells to apply the statistics to.
#'
#' @export
#' @examples
#'
#' GetStatFormula(n = 5, m = 10)
#' GetStatFormula( n = 10, m = 6, nmin = 6)
#' 
GetStatFormula <- function(n, m, nmin = 2) {
  
  stopifnot((n > 0 && nmin > 0) || (nmin == 0 && nmin == n), n%%1 == 0 && nmin%%1 == 0, m > 0 && m%%1 == 0)
  
  sprintf(
    "%s(%s%s)",
    c("COUNTA", "AVERAGE", "STDEV", "COUNTIF", "COUNTIF", "COUNTIF"),
    paste(Idx2Ref(c(nmin, m, n, m)), collapse=":"),
    c("", "", "", ', ">=10"', ', "<=5"', ', "<=1"')
  )
}
