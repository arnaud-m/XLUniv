#' Export conversion SA questions to GIFT format
#'
#' @param exdata Data frame of questions
#'
#' @export
#' @examples
#'
#' exdata <- CreateConvertQCM(10)
#' GiftConvertSA(exdata)
GiftConvertSA <- function(exdata) {
  ConvertSA <- function(x) {
    x <- sapply(x, trimws)
    GIFTSA(
      sprintf("::Conversion de base::Convertir le nombre %s écrit en base %s vers la base %s.", x['valueO'], x['orig'], x['dest']),
      x['valueD']
    )
  }
  apply(exdata, 1, ConvertSA)
  return(invisible(NULL))
}
