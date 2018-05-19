#' Retrieves a named cell style from a 'workbook'.
#'
#' @param wb The 'workbook' to use
#' @param name The name of the 'cellstyle' to retrieve
#'
#' @return the 'cellstyle' with the specified 'name' (and configure it if necessary).
#' 
#' The function is a wrapper around XLConnect::getOrCreateCellStyle that configure the style if it does not exist.
#' There are actually four predefined styles: header; highlight; stat; listing.
#'
#' @export 
#' @examples
#'  # Load workbook (create if not existing)
#'  wb <- XLConnect::loadWorkbook("GetOrCreateCellStyle.xlsx", create = TRUE)
#'     
#'  # The first time, the style does not exist yet and gets created
#'  myStyle <- GetOrCreateCellStyle(wb, name = "highlight")
#'     
#'  # The second time, we retrieve the already existing style 
#'  myStyle <- GetOrCreateCellStyle(wb, name = "highlight")
#'
GetOrCreateCellStyle <- function(wb, name) {
  XLC <- XLConnect::XLC
  if(XLConnect::existsCellStyle(wb, name)) {
    return(XLConnect::getCellStyle(wb, name))
  } else {
    cs <- XLConnect::createCellStyle(wb, name = name)
    if(name == "header") {
      XLConnect::setFillPattern(cs, fill = XLC$FILL.SOLID_FOREGROUND)
      XLConnect::setFillForegroundColor(cs, color = XLC$COLOR.GREY_25_PERCENT)
    } else if(name == "highlight") {
      XLConnect::setFillPattern(cs, fill = XLC$FILL.SOLID_FOREGROUND)
      XLConnect::setFillForegroundColor(cs, color = XLC$COLOR.LIGHT_CORNFLOWER_BLUE )
    } else if(name == "stat") {
      XLConnect::setFillPattern(cs, fill = XLC$FILL.SOLID_FOREGROUND)
      XLConnect::setFillForegroundColor(cs, color = XLC$COLOR.LIGHT_GREEN)
    } else if(name == "listing") {
      XLConnect::setBorder(cs, side = "all", type = XLC$BORDER.THIN, color = XLC$COLOR.BLACK)
    } 
    return(cs)
  }
}
