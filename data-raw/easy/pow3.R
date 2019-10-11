params <- 2:5
f <- function(n, k) {
  if(k < 1) return(1)
  else return(n * f(n, k-1))
}
cat(f(n, n))
