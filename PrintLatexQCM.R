#' Create data for a QCM with base conversion questions
#'
#'
#'
#' @param n  Number to convert
#' @param base Base of conversion
#' @param digits the character that represent the digits 0...(base-1)
#'
#' @export
#' @examples
#'
#' exdata <- CreateConvertQuestions(8)
#' PrintLaTexQCM(exdata)
PrintLaTexQCM <- function(exdata) {
  cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
  cat("\\begin{Exercise}[title={Notations binaire, octale et hexadécimale}, difficulty={1}]\n")
  cat("\\ExeText\n$(x)_b$ indique que le nombre $x$ est écrit en base $b$.\nEffectuer les conversions suivantes:\n")

  questions <- seq(1, nrow(exdata) - 1, 2)

  ExQuestion <- function(x, i) {
    sprintf(
      "\\Question\n\\Versions{$(%s)_{%s}$}{$(%s)_{%s}$} en base \\Versions{%s}{%s}.\n\\ExamLines{1}",
      x[i, 'valueO'], x[i, 'orig'],
      x[i+1, 'valueO'], x[i+1, 'orig'],
      x[i, 'dest'], x[i+1, 'dest']
      )
  }
  cat(paste(
    sapply(questions, ExQuestion, x = exdata),
    collapse = "\n"
  ))

  ExAnswer <- function(x, i) {
    sprintf(
      "\\Question\n\\Versions{$(%s)_{%s}=(%s)_{%s}$}{$(%s)_{%s}=(%s)_{%s}$}.",
      x[i, 'valueO'], x[i, 'orig'], x[i, 'valueD'], x[i, 'dest'],
      x[i+1, 'valueO'], x[i+1, 'orig'], x[i+1, 'valueD'], x[i+1, 'dest']
    )
  }

  cat("\n\\end{Exercise}\n\\begin{Answer}\n")
  cat(paste(
    sapply(questions, ExAnswer, x = exdata),
    collapse = "\n"
  ))
  cat("\n\\end{Answer}\n")

  return(invisible(NULL))
}
