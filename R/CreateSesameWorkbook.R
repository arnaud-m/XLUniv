#' Generates a workbook from a sesame list with a grading and a listing sheet.
#'
#'
#'  The function generates a workbook with a grading and a listing sheet and saves the workbook to the corresponding Excel file.
#' This method actually writes the 'workbook' object to disk.
#'
#' @param filename The file to which to save the 'workbook' ("save as")
#' @param listing columns of the data frame used for the listing
#' @param sort sort lexicographically the data frame according to the listing columns
#' @inheritParams CreateSheetGrade
#'
#' @export
#' @examples
#'
#' df <- data.frame(nom=sample(letters), prenom=sample(LETTERS), numetu=seq(26))
#' listing <- c("nom", "prenom", "numetu")
#' CreateSesameWorkbook("CreateSesameWorkbook.xlsx", df, listing = listing, sort = TRUE)
#' 
CreateSesameWorkbook <- function(filename, df,
                              listing = colnames(df),
                              coefs = c("CC"=0.3, "PROJ"=0.3, "CT"=0.4),
                              max = NA, round = 0.5, sort = FALSE) {
  ## Load and configure the workbook
  wb <- XLConnect::loadWorkbook ( filename , create = TRUE)
  ## two digits after the decimal point
  XLConnect::setDataFormatForType(wb, type = XLConnect::XLC$"DATA_TYPE.NUMERIC", format = "0.00")
  ## FIXME must use : setStyleAction(object,type) or setDataFormat
  
  ## Sort lexicographically the data frame based on the listing
  if(sort && is.character(listing)) {
    df <- df[ do.call(order, df[ , listing]), ]
  }

  # Create sheets
  CreateSheetGrade(wb, df, coefs, max, round)
  CreateSheetListing(wb, df[, listing])
  ## And save
  XLConnect::saveWorkbook(wb)
}
