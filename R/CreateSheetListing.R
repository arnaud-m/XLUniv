#' Create a listing sheet with bordered cells
#'
#' @param wb The 'workbook' to use
#' @param df The 'listing' to display
#'
#' 
#' @export
#' @examples
#'
#' # Load workbook (create if not existing)
#' wb <- XLConnect::loadWorkbook("CreateSheetListing.xlsx", create = TRUE)
#' df <- data.frame(l=letters, L=LETTERS)
#' CreateSheetListing(wb, df)
#' # Save workbook (this actually writes the file to disk)
#' XLConnect::saveWorkbook(wb)
CreateSheetListing <- function(wb, df) {
  ## Create sheet
  sheet <- "LISTING"
  XLConnect::createSheet (wb , sheet)
  XLConnect::writeWorksheet(wb, cbind(df,signature=NA), sheet = sheet, startRow = 1, startCol = 1)
  ## Retrieve data frame dimensions
  n <- nrow(df) + 1 ## header
  m <- ncol(df) + 1 ## signature
  rows <- seq(n)
  cols <- seq(m)
  ## Apply style and set cell dimensions 
  SetCellStyle(wb, sheet, utils::tail(rows, -1), cols, GetOrCreateCellStyle(wb, "listing"))
  XLConnect::setRowHeight(wb, sheet = sheet, row = rows, height = 25)
  XLConnect::setColumnWidth(wb, sheet = sheet, col = cols, width = 256*15)
  XLConnect::setColumnWidth(wb, sheet = sheet, col = m, width = 256*40)
}
