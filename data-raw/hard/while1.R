params <- 10:15
m <- (2*n) %/% 3
while(n > 0 || m > 0) {
  cat(n, '') 
  if(n < m) {
    m <- m %/% 2
  } else {
    n <- n %/% 2;
  }
}

