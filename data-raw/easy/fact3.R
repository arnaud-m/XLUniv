params <- 4:8
f <- function(n) {
  if(n <= 1) return(1)
  else return(n * f(n-1))
}
cat(f(n))
