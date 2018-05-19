#'  Sets cell styles for specific set of cells in a 'workbook'.
#' 
#' @param wb The 'workbook' to use
#' @param sheet Name or index of the sheet the cell is on.
#' @param row Vector of row indices of the cells to apply the cellstyle to.
#' @param col Vector of column indices of the cell to apply the cellstyle to.
#' @param cellstyle 'cellstyle' to apply
#' 
#' The function sets the style for the cartesian product of the vectors row and col.
#' It is a simple wrapper around XLConnect::setCellStyle which sets the style to pairs of indices.
#' @seealso \code{\link[XLConnect]{setCellStyle}} 
#'
#' @export 
#' @examples
#'  # Load workbook (create if not existing)
#'  wb <- XLConnect::loadWorkbook("SetCellStyle.xlsx", create = TRUE)
#'     
#'  # The first time, the style does not exist yet and gets created
#'  myStyle <- GetOrCreateCellStyle(wb, name = "stat")
#'
#'  # Create a sheet named 'mtcars'
#'  XLConnect::createSheet(wb, name = "mtcars")
#'  XLConnect::writeWorksheet(wb,mtcars,'mtcars', 1, 1)
#'  # Which cars have a weight > 3.5 ?
#'  rowIndex <- which(mtcars$wt > 3.5)
#'     
#'   # The same holds for the column index
#'   colIndex <- which(names(mtcars) %in% c("wt", "qsec"))
#'   
#'   # Set the cell style for the corresponding cells.
#'   # Note: the row and col arguments are vectorized!
#'   SetCellStyle(wb, sheet = "mtcars", row = rowIndex, col = colIndex, 
#'                cellstyle = myStyle)
#'   
#'   # Save workbook (this actually writes the file to disk)
#'   XLConnect::saveWorkbook(wb)
SetCellStyle <- function(wb, sheet, row, col, cellstyle) {
  for(r in row) {
    XLConnect::setCellStyle(wb, sheet = sheet, row = r, col = col, cellstyle = cellstyle)
  }
}
