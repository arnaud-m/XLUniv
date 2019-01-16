params <- 30:40
f <- function(n) {
  cat(n, '')
  if(n > 0) {
    if(n %% 2 == 0) return(f(n %/% 2))
    else return(f(n %/% 3))
  }
}
f(n)
