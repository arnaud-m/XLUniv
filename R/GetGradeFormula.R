#' Get an excel formula to grade a student  
#'
#' @param n Row indices to apply the formula
#' @param m Column index where start the grades
#' @param coefs Named vector with coefficients
#' @param max Index in \code{coefs} of the special grade that replaces any other lower grade
#' @param round Rounding value (default: 0.5)
#'
#'
#' @export 
#' @examples
#'
#' coefs <- c(CC = 0.3, CP = 0.3, CT = 0.4)
#' GetGradeFormula(5, 10, coefs)
#' GetGradeFormula(5, 10, coefs, max = 3)
#' GetGradeFormula(5, 10, coefs, round = NA) 
GetGradeFormula <- function(n, m, coefs, max = NA, round = 0.5) {
  
  stopifnot(coefs > 0, n > 0 && n%%1 == 0, m > 0 && m%%1 == 0, round > 0 || round == NA, max > 0 || && max == NA, max <= length(coefs))
  
  input <- Idx2Ref(c(rbind(n, m + seq_along(coefs) - 1)))
  if(is.numeric(max)) {
    input <- paste("MAX(", input, ", ", Idx2Ref(c(n, m + max - 1)), ")", sep = "")
  }
  formula <- paste(coefs, '*', input, sep = "", collapse = ' + ')
  if(is.numeric(round)) {
    formula <- paste("MROUND(", formula, ", ", round, ")", sep = "")
  }
  return(formula)
}

