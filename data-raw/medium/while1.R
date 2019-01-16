params <- 4:6
while(n > 0) {
  m <- n 
  while(m > 0.5) {
    m <- m / 2
    cat(m, '')
  }
  n <- n - 1;
}
