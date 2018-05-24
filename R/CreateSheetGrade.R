#' Create a grading sheet with styles and formulas
#'
#' @param wb The 'workbook' to use
#' @param df the data frame that contains the student list
#' @inheritParams GetGradeFormula
#'
#'
#' @export 
#' @examples
#' 
#' # Load workbook (create if not existing)
#' wb <- XLConnect::loadWorkbook("CreateSheetGrade.xlsx", create = TRUE)
#' df <- data.frame(nom=letters, prenom=LETTERS, numetu=1:26)
#' coefs <- c(CC = 0.3, CP = 0.3, CT=0.4)
#' CreateSheetGrade(wb, df, coefs)
#' # Save workbook (this actually writes the file to disk)
#' XLConnect::saveWorkbook(wb)
#' 
CreateSheetGrade <- function(wb, df, coefs, max = NA, round = 0.5) {
  ## Create sheet
  sheet <- "NOTES"
  XLConnect::createSheet (wb , sheet)
  ## Retrieve dimensions
  n <- nrow(df) + 1 ## header
  m1 <- ncol(df)
  m2 <- length(coefs) + 1
  m  <- m1 + m2
  ## Add grade columns 
  newcols <- matrix(NA, 1, m2)
  colnames(newcols) <- c(names(coefs), "NOTE") 
  df <- cbind(df, newcols)

  ## Write sheet
  XLConnect::writeWorksheet(wb, df, sheet = sheet, startRow = 1, startCol = 1)
  ## Apply style
  SetCellStyle(wb, sheet = sheet,
                     row = seq(2, n , 2),
                     col = seq(m),
                     cellstyle = GetOrCreateCellStyle(wb, "highlight"))
  ## Set formulas
  for(k in seq(2,n)) {
    XLConnect::setCellFormula(wb, sheet, k , m, GetGradeFormula(k, m1+1, coefs, max, round))
  }
  SetStatFormula(wb, sheet, n, seq(m1 + 1, m), mlab = m1)   
}
