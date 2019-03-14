params <- 4:6
while(n > 0) {
  m <- n 
  while(m > 0.25) {
    m <- m / 4
    cat(m, '')
  }
  n <- n - 1;
}
