params <- 8:12
m <- (2*n) %/% 3
while(n >= 1 || m >= 1) {
  if(n < m) {
    cat(m, '')
    m <- m / 2
  } else {
    cat(n, '')
    n <- n / 2;
  }
}

