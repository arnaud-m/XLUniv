#' Create data for a QCM with base conversion questions
#'
#' 
#' 
#' @param n  Number of questions
#' @param minNum Minimum possible number to convert
#' @param maxNum Maximum possible number to convert 
#' @param precision Power of the increment of the numbers to convert (2**(-precision))
#'
#' @export
#' @examples
#'
#' CreateConvertQCM(6)
#' CreateConvertQCM(10, minNum = 10, maxNum = 20, precision = 1)
CreateConvertQCM <- function(n,
                             minNum = 100,
                             maxNum = 2048,
                             precision = 5
                             ) {
  stopifnot(n > 0, maxNum >= minNum, minNum >= 0, precision >= 0)
  
  conversions <- data.frame(
    orig = c(10, 10, 10, 10,  2,  4,  8, 16,  2,  2,  2,  4,  4,  8),
    dest = c( 2,  4,  8, 16, 10, 10, 10, 10,  4,  8, 16,  8, 16, 16),
    prob = c( 2,  2,  2,  2,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1)
  )
  
  convQ <- sample(seq_along(conversions$prob), n, replace = TRUE, prob = conversions$prob)
  exdata <- conversions[ convQ, c('orig', 'dest') ]
  exdata <- as.data.frame(exdata)
  rownames(exdata) <- NULL
  
  values <- seq(minNum, maxNum, 2 ** (-precision))
  if( precision > 0) {
    ## remove integers.
    values <- subset(values, round(values) != values)
  }
  exdata$value <- sample(values, n)

  ## First, apply on NUMERIC vector
  valueO <- apply(exdata, 1, function(x) EncodeNumber(x['value'], base = x['orig']))
  valueD <- apply(exdata, 1, function(x) EncodeNumber(x['value'], base = x['dest']))
  ## Second, add the new columns
  exdata$valueO <- valueO
  exdata$valueD <- valueD

  return(exdata)
}


