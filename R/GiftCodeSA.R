#' Print a single SA question about reading code.
#'
#' 
#' @param pathname  filename of the question code
#'
#' The first line of file contains the parameters of the questions in  a vector named 'params'.
#' The next lines contains the source code of the question.
#' The source code requires a single parameter named 'n'.
#' 
#' The expected output is a sequence of integer or characters.
#'
#' @export
#' @examples
#'
GiftCodeSA <- function(pathname) {
  EvalText <- function(text) eval.parent(parse(text = text))
  if(! require("RGIFT")) {
    warning('Cannot ask the question: RGIFT not available.')
  }
   
  ## Read question code
  lines <- readLines(pathname)
  ## Retrieve the variable 'params'
  EvalText(head(lines, 1))

  ## Build expressions
  code <- paste(tail(lines, -1), collapse="\n")
  
  ## Protect GIFT special characters
  codeQ <- code;
  EscapeChar <- function(str, ch) gsub(ch, paste0('\\', ch), str, fixed = TRUE)
  for(ch in c('~','=','#','{','}',':')) {
    codeQ <- EscapeChar(codeQ, ch)
  }
  codeQ <- gsub('\n', '\\\\n    ', codeQ)

  codename <- basename(pathname)
  ## Loop over params to build the parametrized questions
  for(n in params) {
    ## Question 
    codeN <- paste(
      '::Lecture du code ',codename, ' (n=' ,n, ')::\n[markdown]\n',
      'Qu\'affiche le code suivant ?\n',
      '\\n\n\\n    n <- ', n, '\\n    ',
      codeQ, '\n\\n\\n\\n',
      sep = '', collapse = ''
    )
    ## Answer
    outputN <- paste(
      sapply(
        capture.output(EvalText(code)),
        trimws
      ), 
      collapse = ' ')
    ## Print 
    GIFTSA(codeN, outputN)
  }
}

