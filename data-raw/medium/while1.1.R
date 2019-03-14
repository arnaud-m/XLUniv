params <- 4:6
while(n > 0) {
  m <- n 
  while(m > 0.5) {
    cat(m, '')
    m <- m / 2
  }
  n <- n - 1;
}
