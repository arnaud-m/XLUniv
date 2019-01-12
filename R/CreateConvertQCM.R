#' Create data for a QCM with base conversion questions
#'
#' 
#' 
#' @param n  Number of questions
#' @param m Column index of the cells to apply the statistics to.
#' @param nmin  Minimal row index of the cells to apply the statistics to.
#'
#' @export
#' @examples
#'
#' CreateConvertQCM(6)
#' CreateConvertQCM(10, bases = c(2, 10), prefBases = NA, minNum = 10, maxNum = 20, by = 1/16)
CreateConvertQCM <- function(n,
                             bases = c(2, 3, 4, 8, 10, 16),
                             prefBases = 10,
                              minNum = 100,
                             maxNum = 2048,
                             by = 1,
                             ) {
  stopifnot(n > 0, bases > 0, maxNum >= minNum)
  
  # Generate the base conversion
  conversions <- expand.grid(orig = bases, dest = bases)
  conversions <- subset(conversions, conversions$orig != conversions$dest )     
  probconv <- 1 + (conversions$orig %in% prefBases)

  exdata <- conversions[ sample(seq_along(probconv), n, replace = TRUE, prob = probconv), ]
  rownames(exdata) <- NULL
  exdata <- as.data.frame(exdata)
  
  values <- seq(minNum, maxNum, by)
  if( by != round(by)) {
    ## Use only decimals, remove integers.
    values <- subset(values, round(values) != values)
  }
  exdata$value <- sample(values, n)

  valueO <- apply(exdata, 1, function(x) NumberToString(x['value'], base = x['orig']))
  valueD <- apply(exdata, 1, function(x) NumberToString(x['value'], base = x['dest']))

  exdata$valueO <- valueO
  exdata$valueD <- valueD
  return(exdata)
}
