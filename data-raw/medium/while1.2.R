params <- 4:6
while(n > 0) {
  m <- n 
  while(m > 0.25) {
    cat(m, '')
    m <- m / 4
  }
  n <- n - 1;
}
