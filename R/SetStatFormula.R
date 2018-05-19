#'  Sets basic statistics for grade columns in a 'workbook'.
#'
#' @param wb The 'workbook' to use
#' @param sheet Name or index of the sheet the cells are on.
#' @param mlab  Optional column index where to set the statistics labels
#'
#' @inheritParams GetStatFormula
#'
#' @details 
#'
#' The statistics are set below the last row. 
#'
#' @export
#' @examples
#'  # Load workbook (create if not existing)
#'  wb <- XLConnect::loadWorkbook("SetStatFormula.xlsx", create = TRUE)
#'     
#'  # Create a sheet named 'stat'
#'  XLConnect::createSheet(wb, name = "stat")
#'  XLConnect::writeWorksheet(wb,data.frame(x = 1:10), "stat", 1, 2)
#'  SetStatFormula(wb, 'stat', n = 12, m = 2, mlab = 1)
#' 
#'  # Save workbook (this actually writes the file to disk)
#'  XLConnect::saveWorkbook(wb)
#' 
SetStatFormula <- function(wb, sheet, n, m, nmin = 2, mlab = NA) {
  nf <- 6
  rows <- n + seq(nf)
  csStat <- GetOrCreateCellStyle(wb, "stat")
  ## Write labels 
  if(mlab > 0) {
    XLConnect::writeWorksheet(wb, c("PRESENTS", "MOYENNE", "ECART-TYPE", ">=10", "<= 5", "<= 1"), sheet = sheet, startRow = n + 1 , startCol = mlab, header = FALSE)
    SetCellStyle(wb, sheet = sheet,
                 row = rows,
                 col = mlab,
                 cellstyle = csStat)
  }
  ## Set formulas 
  sapply(m, function(k) XLConnect::setCellFormula(wb, sheet, rows , rep(k, nf), GetStatFormula(n, k)))
  ## Set Stat style
  SetCellStyle(wb, sheet = sheet,
               row = rows,
               col = m,
               cellstyle = csStat)
}
